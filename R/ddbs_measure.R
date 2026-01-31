

#' Calculate the area of geometries
#'
#' Computes the area of polygon geometries in square meters.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template new_column
#' @template crs
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @returns When `new_column = NULL` it returns a `units` vector in \eqn{m^2}. When `new_column` is not NULL, the
#' output depends on the \code{output} argument (or global preference set by \code{\link{ddbs_options}}):
#'   \itemize{
#'     \item \code{duckspatial_df} (default): A lazy spatial data frame backed by dbplyr/DuckDB.
#'     \item \code{sf}: An eagerly collected \code{sf} object in R memory.
#'     \item \code{tibble}: An eagerly collected \code{tibble} without geometry in R memory.
#'     \item \code{raw}: An eagerly collected \code{tibble} with WKB geometry (no conversion).
#'     \item \code{geoarrow}: An eagerly collected \code{tibble} with geometry converted to \code{geoarrow_vctr}.
#'   }
#'   When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).
#' 
#' @details
#' When the input geometry is in `EPSG:4326`, the function uses `ST_Area_Spheroid`, which
#' use the GeographicLib library for calculating the area using an ellipsoidal model of the
#' earth. This method is highly accurate for calculating the area of a line geometry considering
#' the curvature of the earth, but it's also the slowest.
#' 
#' If the input geometry is in a projected CRS, the function will use `ST_Area` to calculate the
#' area in squared meters.
#' 
#' In other cases, the function will use `ST_Area_Spheroid` and display a warning.
#'
#' @export
#' @references \url{https://geographiclib.sourceforge.io/}
#' 
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' ) |>
#'   ddbs_transform("EPSG:3857")
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## calculate area (returns sf object with area column)
#' ddbs_area("argentina", conn)
#'
#' ## calculate area with custom column name
#' ddbs_area("argentina", conn, new_column = "area_sqm")
#'
#' ## create a new table with area calculations
#' ddbs_area("argentina", conn, name = "argentina_with_area", new_column = "area_sqm")
#'
#' ## calculate area in a sf object
#' ddbs_area(argentina_ddbs)
#' }
ddbs_area <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    # 0. Validate inputs
    assert_xy(x, "x")
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(new_column, "new_column")
    assert_name(output, "output")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    
    if (!is.null(name) && is.null(new_column)) cli::cli_abort("Please, specify the {.arg new_column} name.")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- ddbs_crs(x, conn)
    sf_col_x <- attr(x, "sf_column")

    ## 1.2. Extract units, and warn if they aren't meters or EPSG:4326
    ## for EPSG:4326, we can use ST_Area_Spheroid to get the area in meters
    ## so that will be an exception
    crs_units <- crs_x$units_gdal

    if (crs_units != "metre" && "EPSG:4326" != crs_x$input) {
        cli::cli_warn(
          "Input is in {.val {crs_x$input}}, not {.val EPSG:4326}. Area calculations may be less accurate. Consider transforming to {.val EPSG:4326} or a projected CRS."
        )
    }

    ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 2.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

    ## 3.3. Use the right function depending on the CRS
    st_area_fun <- if (crs_units == "metre") {
      glue::glue("ST_Area({x_geom})")
    } else {
      glue::glue("ST_Area_Spheroid(ST_FlipCoordinates({x_geom}))")
    }


    # 4. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT {st_area_fun} as area,
            FROM {x_list$query_name};
        ")
        
        data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
        feedback_query(quiet)
    
        ## get vector, and convert it to units
        data_units <- units::as_units(data_vec[, 1], "m^2")
    
        return(data_units)
    }


    # 5. if name is not NULL (i.e. no data frame returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            {st_area_fun} AS {new_column},
            {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute area query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Get data frame
    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        {st_area_fun} AS {new_column},
        ST_AsWKB({x_geom}) as {x_geom}
        FROM {x_list$query_name}
    ")
    ## 5.2. retrieve results of the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

    ## 6. convert to target output
    data_sf <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}






#' Calculate the length of geometries
#'
#' Computes the length of linear geometries, typically in meters.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template new_column
#' @template crs
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @returns When `new_column = NULL` it returns a `units` vector in meters. When `new_column` is not NULL, the
#' output depends on the \code{output} argument (or global preference set by \code{\link{ddbs_options}}):
#'   \itemize{
#'     \item \code{duckspatial_df} (default): A lazy spatial data frame backed by dbplyr/DuckDB.
#'     \item \code{sf}: An eagerly collected \code{sf} object in R memory.
#'     \item \code{tibble}: An eagerly collected \code{tibble} without geometry in R memory.
#'     \item \code{raw}: An eagerly collected \code{tibble} with WKB geometry (no conversion).
#'     \item \code{geoarrow}: An eagerly collected \code{tibble} with geometry converted to \code{geoarrow_vctr}.
#'   }
#'   When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).
#' 
#' @details
#' 
#' When the input geometry is in `EPSG:4326`, the function uses `ST_Length_Spheroid`, which
#' use the GeographicLib library for calculating the length using an ellipsoidal model of the
#' earth. This method is highly accurate for calculating the length of a line geometry considering
#' the curvature of the earth, but it's also the slowest.
#' 
#' If the input geometry is in a projected CRS, the function will use `ST_Length` to calculate the
#' length in meters.
#' 
#' In other cases, the function will use `ST_Length_Spheroid` and display a warning.
#' 
#' @export
#' 
#' @references \url{https://geographiclib.sourceforge.io/}
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson", 
#'   package = "duckspatial")
#' )
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, rivers_ddbs, "rivers")
#'
#' ## calculate length (returns sf object with length column)
#' ddbs_length("rivers", conn)
#'
#' ## calculate length with custom column name
#' ddbs_length("rivers", conn, new_column = "length_meters")
#'
#' ## create a new table with length calculations
#' ddbs_length("rivers", conn, name = "rivers_with_length", new_column = "length_meters")
#'
#' ## calculate length in a sf object (without a connection)
#' ddbs_length(rivers_ddbs)
#' }
ddbs_length <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    deprecate_crs(crs_column, crs)

    # 0. Validate inputs
    assert_xy(x, "x")
    assert_conn_character(conn, x)  
    assert_name(name)
    assert_name(new_column, "new_column")
    assert_name(output, "output")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    if (!is.null(name) && is.null(new_column)) cli::cli_abort("Please, specify the {.arg new_column} name.")
        
    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- ddbs_crs(x, conn)
    sf_col_x <- attr(x, "sf_column")
    
    ## 1.2. Extract units, and warn if they aren't meters or EPSG:4326
    ## for EPSG:4326, we can use ST_Length_Spheroid to get the length in meters
    ## so that will be an exception
    crs_units <- crs_x$units_gdal

    if (crs_units != "metre" && "EPSG:4326" != crs_x$input) {
        cli::cli_warn(
          "Input is in {.val {crs_x$input}}, not {.val EPSG:4326}. Length calculations may be less accurate. Consider transforming to {.val EPSG:4326} or a projected CRS."
        )
    }


    ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 2.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

    ## 3.3. Use the right function depending on the CRS
    st_length_fun <- if (crs_units == "metre") {
      glue::glue("ST_Length({x_geom})")
    } else {
      glue::glue("ST_Length_Spheroid(ST_FlipCoordinates({x_geom}))")
    }


    # 4. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT {st_length_fun} as length,
            FROM {x_list$query_name};
        ")
        
        data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
    
        ## get vector, and convert it to units
        data_units <- units::as_units(data_vec[, 1], "metre")
    
        return(data_units)
    }


    # 5. if name is not NULL (i.e. no data frame returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            {st_length_fun} AS {new_column},
            {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute length query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Get data frame
    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        {st_length_fun} AS {new_column},
        ST_AsWKB({x_geom}) as {x_geom}
        FROM {x_list$query_name}
    ")
    ## 5.2. retrieve results of the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

    # 6. convert to target output
    data_sf <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}





#' Calculate the distance between geometries
#'
#' Computes the distance between two geometries, automatically using an appropriate 
#' measurement based on their coordinate reference system and geometry type.
#'
#' @template x
#' @template y
#' @param dist_type Character. Distance type to be calculated. By the default it uses
#' the best option for the input CRS (see details).
#' @template conn_null
#' @template conn_x_conn_y
#' @template quiet
#'
#' @returns A `units` matrix in meters
#'
#' @export
#' 
#' @details
#' The `dist_type` argument can take any of the following values:
#'  \itemize{
#'      \item \code{NULL} (default): it will use the best option for the inputs CRS
#'      \item \code{"planar"}: planar distance between two geometries (default for projected CRS)
#'      \item \code{"geos"}: planar distance between two geometries using GEOS library
#'      \item \code{"haversine"}: returns the great circle distance. Requires the input to be
#' in WGS84 (EPSG:4326) and POINT geometry (default for EPSG:4326).
#'      \item \code{"spheroid"}: returns the distance using an ellipsoidal model of the earth's
#' surface using the GeographicLib library. It's highly accurate but the slowest
#' }
#' 
#' Note that geometries different than POINT are not supported by "haversine" nor "spheroid".
#'
#' @examples
#' \dontrun{
#' # load packages
#' library(duckspatial)
#'
#' # create points data
#' n <- 10
#' points_sf <- data.frame(
#'     id = 1:n,
#'     x = runif(n, min = -180, max = 180),
#'     y = runif(n, min = -90, max = 90)
#' ) |>
#'     ddbs_as_spatial(coords = c("x", "y"), crs = "EPSG:4326")
#'
#' # option 1: passing sf objects
#' output1 <- duckspatial::ddbs_distance(
#'     x = points_sf,
#'     y = points_sf,
#'     dist_type = "haversine"
#' )
#'
#' head(output1)
#'
#'
#' ## option 2: passing the names of tables in a duckdb db and output as sf
#'
#' # creates a duckdb
#' conn <- duckspatial::ddbs_create_conn()
#'
#' # write sf to duckdb
#' ddbs_write_vector(conn, points_sf, "points", overwrite = TRUE)
#'
#' output2 <- ddbs_distance(
#'     conn = conn,
#'     x = "points",
#'     y = "points",
#'     dist_type = "haversine"
#' )
#' head(output2)
#'
#' }
ddbs_distance <- function(
    x,
    y,
    dist_type = NULL,
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    quiet = FALSE) {

    
    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
    assert_name(dist_type, "dist_type")
    assert_logic(quiet, "quiet")


    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x <- if (is.null(conn_x)) ddbs_crs(x, conn) else ddbs_crs(x, conn_x)
    crs_y <- if (is.null(conn_y)) ddbs_crs(y, conn) else ddbs_crs(y, conn_y)
    sf_col_x <- attr(x, "sf_column")
    sf_col_y <- attr(y, "sf_column")

    ## 1.2. Resolve conn_x/conn_y defaults from 'conn' for character inputs
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

    ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn_x)
    y <- normalize_spatial_input(y, conn_y)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y, conn, conn_x, conn_y)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    y            <- resolve_conn$y
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 2.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)
    y_list <- get_query_list(y, target_conn)
    on.exit(y_list$cleanup(), add = TRUE)

    ## 2.3. CRS already extracted at start of function
    if (!is.null(crs_x) && !is.null(crs_y)) {
      if (!crs_equal(crs_x, crs_y)) {
        cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
      }
    } else {
      assert_crs(target_conn, x_list$query_name, y_list$query_name)
    }
  
    ## 2.4. Get crs units and geom type for next checks
    crs_units <- crs_x$units_gdal
    geom_type_x <- as.character(ddbs_geometry_type(x, conn = target_conn, by_feature = FALSE, quiet = TRUE))
    geom_type_y <- as.character(ddbs_geometry_type(y, conn = target_conn, by_feature = FALSE, quiet = TRUE))

    ## 2.4. Get the right distance type if user uses the default
    if (is.null(dist_type)) {
      if (crs_units == "degree") {
        ## Default to haversine if it's point and EPSG:4326
        if (crs_x$input == "EPSG:4326" && all(c(geom_type_x, geom_type_y) == "POINT")) {
          dist_type <- "haversine"
        } else {
          ## Default to spheroid if it's not POINT or if it's not EPSG:4326
          dist_type <- "spheroid"
        }
      } else {
        ## Otherwise, default to planar
        dist_type <- "planar"
      }
      if (!quiet) cli::cli_alert_info("Using {.arg dist_type = {.val {dist_type}}} by default.")
    }
  
    ## 2.5. Warnings/Errors for wrong election of dist_type
    ## Error: using an invalid distance type
    valid_types <- c("planar", "haversine", "geos", "spheroid")
    if (!dist_type %in% valid_types) {
        cli::cli_abort("{.arg dist_type} must be one of {.or {.val {valid_types}}}, not {.val {dist_type}}.")
    }

    ## Error: Using planar/geos on geographic coordinates
    if (crs_units == "degree" && dist_type %in% c("planar", "geos")) {
        cli::cli_abort(
            "When using {.arg dist_type = {.val {dist_type}}}, inputs must be in projected coordinates (e.g., UTM), not geographic (degrees)."
        )
    }

    ## Error: haversine/spheroid require POINT geometries
    if (dist_type %in% c("haversine", "spheroid") && !all(c(geom_type_x, geom_type_y) == "POINT")) {
        cli::cli_abort(
            "When using {.arg dist_type = {.val {dist_type}}}, inputs must be POINT geometries."
        )
    }

    ## Error: Using haversine/spheroid on projected coordinates
    if (crs_units == "metre" && dist_type %in% c("haversine", "spheroid")) {
        cli::cli_abort(
            "When using {.arg dist_type = {.val {dist_type}}}, inputs must be in {.val EPSG:4326} coordinates, not projected coordinates."
        )
    }

    ## Warning: Geographic CRS but not WGS84 (spheroid/haversine might be less accurate)
    if (crs_units == "degree" && dist_type %in% c("haversine", "spheroid") && crs_x$input != "EPSG:4326") {
        cli::cli_warn(
            "Inputs are in {.val {crs_x$input}}, not {.val EPSG:4326}. Distance calculations may be less accurate. Consider transforming to {.val EPSG:4326} or a projected CRS."
        )
    }
  

    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
  
    ## 3.2. Select the right DuckD's function
    st_distance_fun <- switch(
      dist_type,
      "planar"    = "ST_Distance",
      "geos"      = "ST_Distance_GEOS",
      "haversine" = "ST_Distance_Sphere",
      "spheroid"  = "ST_Distance_Spheroid"
    )
  
    ## 3.3. Select the right coordinates order
    if (dist_type %in% c("haversine", "spheroid")) {
      coords <- glue::glue("ST_FlipCoordinates(x.{x_geom}), ST_FlipCoordinates( y.{y_geom})")
    } else {
      coords <- glue::glue("x.{x_geom}, y.{y_geom}")
    }
    
    ## 3.2. create query
    tmp.query <- glue::glue("
        SELECT {st_distance_fun}({coords}) as distance
        FROM {x_list$query_name} x
        CROSS JOIN {y_list$query_name} y
    ")

    ## 3.3. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

    ## convert to matrix
    # get number of rows
    nrowx <- get_nrow(target_conn, x_list$query_name)
    nrowy <- get_nrow(target_conn, y_list$query_name)

    ## convert results to matrix -> to list
    ## return matrix if sparse = FALSE
    dist_mat  <- matrix(
        data_tbl[["distance"]],
        nrow = nrowx,
        ncol = nrowy,
        byrow = TRUE
    )
  
    ## set units
    dist_mat <- units::set_units(dist_mat, "metre")

    feedback_query(quiet)
    return(dist_mat)
  
}

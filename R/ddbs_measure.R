

#' Calculates the area of geometries
#'
#' Calculates the area of geometries from a DuckDB table or a `sf` object
#' Returns the result as an \code{sf} object with an area column or creates a new table in the database.
#' Note: Area units depend on the CRS of the input geometries (e.g., square meters for projected CRS,
#' or degrees for geographic CRS).
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
#' @returns When `new_column = NULL` it returns a logical vector. When `new_column` is not NULL, the
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
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial")) |>
#'     st_transform("EPSG:3857")
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## calculate area (returns sf object with area column)
#' ddbs_area("argentina", conn)
#'
#' ## calculate area with custom column name
#' ddbs_area("argentina", conn, new_column = "area_sqm")
#'
#' ## create a new table with area calculations
#' ddbs_area("argentina", conn, name = "argentina_with_area")
#'
#' ## calculate area in a sf object
#' ddbs_area(argentina_sf)
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
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)
    
    if (!is.null(name) && is.null(new_column)) cli::cli_abort("Please, specify the {.arg new_column} name.")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- detect_crs(x)
    sf_col_x <- attr(x, "sf_column")

    ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
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


    # 4. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_Area({x_geom}) as area,
            FROM {x_list$query_name};
            ")

            data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
            return(data_vec[, 1])
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
            ST_Area({x_geom}) AS {new_column},
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
        ST_Area({x_geom}) AS {new_column},
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






#' Calculates the length of geometries
#'
#' Calculates the length of geometries from a DuckDB table or a `sf` object
#' Returns the result as an \code{sf} object with a length column or creates a new table in the database.
#' Note: Length units depend on the CRS of the input geometries (e.g., meters for projected CRS,
#' or degrees for geographic CRS).
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
#' @returns When `new_column = NULL` it returns a logical vector. When `new_column` is not NULL, the
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
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, rivers_sf, "rivers")
#'
#' ## calculate length (returns sf object with length column)
#' ddbs_length("rivers", conn)
#'
#' ## calculate length with custom column name
#' ddbs_length("rivers", conn, new_column = "length_meters")
#'
#' ## create a new table with length calculations
#' ddbs_length("rivers", conn, name = "rivers_with_length")
#'
#' ## calculate length in a sf object (without a connection)
#' ddbs_length(rivers_sf)
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
  assert_name(name)
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)  
  if (!is.null(name) && is.null(new_column)) cli::cli_abort("Please, specify the {.arg new_column} name.")
    
  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (CRS and geometry column name)
  ## this step should be before normalize_spatial_input()
  crs_x    <- detect_crs(x)
  sf_col_x <- attr(x, "sf_column")

  ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
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


  # 4. Handle new column = NULL
  if (is.null(new_column)) {
      tmp.query <- glue::glue("
          SELECT ST_Length({x_geom}) as length,
          FROM {x_list$query_name};
        ")

        data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
        return(data_vec[, 1])
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
          ST_Length({x_geom}) AS {new_column},
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
      ST_Length({x_geom}) AS {new_column},
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





#' Returns the distance between two geometries
#'
#' Returns the planar or haversine distance between two geometries, and returns
#' a \code{data.frame} object or creates a new table in a DuckDB database.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @param dist_type String. One of `c("planar", "haversine")`. Defaults to
#'        `"haversine"` and returns distance in meters, but the input is expected
#'        to be in WGS84 (EPSG:4326) coordinates. The option `"haversine"` only
#'        accepts `POINT` geometries. When `dist_type = "planar"`, distances
#'        estimates are in the same unit as the coordinate reference system (CRS)
#'        of the input.
#' @template conn_null
#' @template conn_x_conn_y
#' @template quiet
#'
#' @returns A `data.frame` object or `TRUE` (invisibly) for table creation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # load packages
#' library(duckspatial)
#' library(sf)
#'
#' # create points data
#' n <- 10
#' points_sf <- data.frame(
#'     id = 1:n,
#'     x = runif(n, min = -180, max = 180),
#'     y = runif(n, min = -90, max = 90)
#' ) |>
#'     sf::st_as_sf(coords = c("x", "y"), crs = 4326)
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
    dist_type = "haversine",
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    quiet = FALSE) {

    
    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
    assert_name(dist_type)
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x, y)

    ## get predicate
    st_predicate <- switch(dist_type,
        "planar"    = "ST_Distance",
        "haversine" = "ST_Distance_Sphere",
       # "spheroid"  = "ST_Distance_Spheroid",
        cli::cli_abort(
            "dist_type should be one of <planar> or <haversine>." # or <spheroid>.
            )
        )

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x <- detect_crs(x)
    crs_y <- detect_crs(y)
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

    ## 2.4. check input projection and geometry
    msg_crs_error <- "When using `dist_type=='haversine'`, the input must be in WGS84 (EPSG:4326) coordinates."
    msg_geom_error <- "When using `dist_type=='haversine'`, the input must be POINT geometries."
    if (dist_type == "haversine") {

        ## get CRS from connection if it's NULL from before
        if (is.null(crs_x)) crs_x <- duckspatial::ddbs_crs(x, target_conn)
        if (is.null(crs_y)) crs_y <- duckspatial::ddbs_crs(y, target_conn)
        
        ## abort when CRS is not latlon
        if (crs_x$input != "EPSG:4326") cli::cli_abort(msg_crs_error)
        if (crs_y$input != "EPSG:4326") cli::cli_abort(msg_crs_error)

        ## abort if the geometry is not point
        geom_type_x <- ddbs_geometry_type(x, conn = target_conn, by_feature = FALSE, quiet = TRUE)
        if (geom_type_x != "POINT") cli::cli_abort(msg_geom_error)
          
        geom_type_y <- ddbs_geometry_type(y, conn = target_conn, by_feature = FALSE, quiet = TRUE)
        if (geom_type_y != "POINT") cli::cli_abort(msg_geom_error)

    }

    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
    
    ## 3.2. create query
    tmp.query <- glue::glue("
        SELECT {st_predicate}(x.{x_geom}, y.{y_geom}) as distance
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

    feedback_query(quiet)
    return(dist_mat)
}



# # return from-to distance data.frame
# query.df <- glue::glue("
#             SELECT * EXCLUDE({y_geom}),
#               ST_Distance(
#                     tbl_x.{x_geom},
#                     tbl_y.{y_geom}
#                   ) AS distance
#             FROM {x_list$query_name} tbl_x, {y_list$query_name} tbl_y
#         ")

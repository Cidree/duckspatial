

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
#' @template overwrite
#' @template quiet
#'
#' @returns a vector, an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
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
ddbs_area <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    # 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

    # 1. Manage connection to DB
    ## 1.1. check if connection is provided, otherwise create a temporary connection
    is_duckdb_conn <- dbConnCheck(conn)
    if (isFALSE(is_duckdb_conn)) {
      conn <- duckspatial::ddbs_create_conn()
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
    }
    ## 1.2. get query list of table names
    x_list <- get_query_list(x, conn)

    # 2. Get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 3. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_Area({x_geom}) as area,
            FROM {x_list$query_name};
          ")

          data_vec <- DBI::dbGetQuery(conn, tmp.query)
          return(data_vec[, 1])
    }

    ## 4. if name is not NULL (i.e. no data frame returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_Area({x_geom}) AS {new_column},
            {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute area query
        DBI::dbExecute(conn, tmp.query)
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
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 6. convert to SF and return result
    data_sf <- convert_to_sf_wkb(
        data       = data_tbl,
        crs        = crs,
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
#' @template overwrite
#' @template quiet
#'
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
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
ddbs_length <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {
  
    deprecate_crs(crs_column, crs)

    # 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

    # 1. Manage connection to DB
    ## 1.1. check if connection is provided, otherwise create a temporary connection
    is_duckdb_conn <- dbConnCheck(conn)
    if (isFALSE(is_duckdb_conn)) {
      conn <- duckspatial::ddbs_create_conn()
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
    }
    ## 1.2. get query list of table names
    x_list <- get_query_list(x, conn)

    # 2. Get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 3. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_Length({x_geom}) as length,
            FROM {x_list$query_name}
          ")

          data_vec <- DBI::dbGetQuery(conn, tmp.query)
          return(data_vec[, 1])
    }

    ## 4. if name is not NULL (i.e. no data frame returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_Length({x_geom}) AS {new_column},
            {x_geom}
            FROM {x_list$query_name}
        ")
        ## execute length query
        DBI::dbExecute(conn, tmp.query)
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
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 6. convert to SF and return result
    data_sf <- convert_to_sf_wkb(
        data       = data_tbl,
        crs        = crs,
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
        quiet = FALSE) {

    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
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

    # check input projection and geometry
    msg_crs_error <- "When using `dist_type=='haversine'`, the input must be in WGS84 (EPSG:4326) coordinates."
    msg_geom_error <- "When using `dist_type=='haversine'`, the input must be POINT geometries."
    if (dist_type=="haversine") {

        if (inherits(x, "sf")) {

            if (sf::st_crs(x)$input != "EPSG:4326"){ cli::cli_abort(msg_crs_error) }

            geom_type <- sf::st_geometry_type(x) |> unique()
            if(geom_type != "POINT"){ cli::cli_abort(msg_geom_error)}
            }


        if (inherits(y, "sf")) {

            if (sf::st_crs(y)$input != "EPSG:4326"){ cli::cli_abort(msg_crs_error) }

            geom_type <- sf::st_geometry_type(y) |> unique()
            if(geom_type != "POINT"){ cli::cli_abort(msg_geom_error)}
        }

    }

    # 1. Manage connection to DB
    ## 1.1. check if connection is provided, otherwise create a temporary connection
    is_duckdb_conn <- dbConnCheck(conn)
    if (isFALSE(is_duckdb_conn)) {
        conn <- duckspatial::ddbs_create_conn()
        on.exit(duckdb::dbDisconnect(conn), add = TRUE)
    }

    ## 1.2. get query list of table names
    x_list <- get_query_list(x, conn)
    y_list <- get_query_list(y, conn)
    assert_crs(conn, x_list$query_name, y_list$query_name)

    ## 2. get name of geometry columns
    x_geom <- get_geom_name(conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    y_geom <- get_geom_name(conn, y_list$query_name)
    assert_geometry_column(y_geom, y_list)

    # 3. Get data frame
    ## 3.1. create query
    tmp.query <- glue::glue("
        SELECT {st_predicate}(x.{x_geom}, y.{y_geom}) as distance
        FROM {x_list$query_name} x
        CROSS JOIN {y_list$query_name} y
    ")

    ## 3.2. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## convert to matrix
    # get number of rows
    nrowx <- get_nrow(conn, x_list$query_name)
    nrowy <- get_nrow(conn, y_list$query_name)

    ## convert results to matrix -> to list
    ## return matrix if sparse = FALSE
    dist_mat  <- matrix(data_tbl[["distance"]],
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

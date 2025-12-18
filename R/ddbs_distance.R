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
#' @param name A character string of length one specifying the name of the table,
#'        or a character string of length two specifying the schema and table
#'        names. If it's `NULL` (the default), it will return the result as an
#'        \code{sf} object.
#' @template crs
#' @template overwrite
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
#' ## option 3: passing the names of tables in a duckdb db and output as db table
#' ddbs_distance(
#'     conn = conn,
#'     x = "points",
#'     y = "points",
#'     dist_type = "haversine",
#'     name = "dist_table"
#'     )
#'
#' DBI::dbReadTable(conn, "dist_table")
#'
#' }
ddbs_distance <- function(
        x,
        y,
        dist_type = "haversine",
        conn = NULL,
        name = NULL,
        crs = NULL,
        crs_column = "crs_duckspatial",
        overwrite = FALSE,
        quiet = FALSE) {

    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x, y)

    dist_verb <- switch(dist_type,
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


    # 2. Prepare params for query
    ## 2.1. get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    y_geom <- get_geom_name(conn, y_list$query_name)
    y_rest <- get_geom_name(conn, y_list$query_name, rest = TRUE)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)

    # 6666666666666666666
    # ## error if crs_column not found
    # assert_crs_column(crs_column, x_rest)

    ## 2. create the base query
    # return only the distance column
    tmp.query <- glue::glue("
            WITH temp_tbl AS(
            SELECT
              {dist_verb}(
                 tbl_x.{x_geom},
                 tbl_y.{y_geom}
               ) AS distance
            FROM {x_list$query_name} tbl_x,
                 {y_list$query_name} tbl_y
            )
            SELECT distance
            FROM temp_tbl
        ")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    feedback_query(quiet)
    return(data_tbl)
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


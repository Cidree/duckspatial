#' Performs spatial joins of two geometries
#'
#' Performs spatial joins of two geometries, and returns a \code{sf} object
#' or creates a new table in a DuckDB database.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @param join A geometry predicate function. Defaults to `"intersects"`. See
#'        the details for other options.
#' @template conn_null
#' @param name A character string of length one specifying the name of the table,
#'        or a character string of length two specifying the schema and table
#'        names. If it's `NULL` (the default), it will return the result as an
#'        \code{sf} object.
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns an sf object or TRUE (invisibly) for table creation
#'
#' @template spatial_join_predicates
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # load packages
#' library(duckspatial)
#' library(sf)
#'
#' # read polygons data
#' countries_sf <- sf::st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#'
#' # create points data
#' n <- 100
#' points_sf <- data.frame(
#'     id = 1:n,
#'     x = runif(n, min = -180, max = 180),
#'     y = runif(n, min = -90, max = 90)
#' ) |>
#'     sf::st_as_sf(coords = c("x", "y"), crs = 4326)
#'
#'
#'
#' # option 1: passing sf objects
#' output1 <- duckspatial::ddbs_join(
#'     x = points_sf,
#'     y = countries_sf,
#'     join = "within"
#' )
#'
#' plot(output1["CNTR_NAME"])
#'
#'
#' ## option 2: passing the names of tables in a duckdb db
#'
#' # creates a duckdb
#' conn <- duckspatial::ddbs_create_conn()
#'
#' # write sf to duckdb
#' ddbs_write_vector(conn, points_sf, "points", overwrite = TRUE)
#' ddbs_write_vector(conn, countries_sf, "countries", overwrite = TRUE)
#'
#' # spatial join
#' output2 <- ddbs_join(
#'     conn = conn,
#'     x = "points",
#'     y = "countries",
#'     join = "within"
#' )
#'
#' plot(output2["CNTR_NAME"])
#'
#' }
ddbs_join <- function(
    x,
    y,
    join = "intersects",
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
    ## 2.1. select predicate
    sel_pred <- get_st_predicate(join)
    ## 2.2. get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    y_geom <- get_geom_name(conn, y_list$query_name)
    y_rest <- get_geom_name(conn, y_list$query_name, rest = TRUE)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
    ## error if crs_column not found
    assert_crs_column(crs_column, x_rest)

    # ## Create an rtree index on the x and y tables
    # # https://duckdb.org/docs/stable/core_extensions/spatial/r-tree_indexes
    # x_has_rtree <- has_rtree_index(conn, tbl_name = x_list$query_name)
    # index_name <- paste0("idx_", x_list$query_name)
    #
    # if (isFALSE(x_has_rtree)) {
    #     DBI::dbExecute(
    #         conn,
    #         glue::glue(
    #             "CREATE INDEX {index_name} ON {x_list$query_name} USING RTREE ({x_geom});"
    #             )
    #         )
    #     }
    #
    # y_has_rtree <- has_rtree_index(conn, y_list$query_name)
    #
    # if(isFALSE(y_has_rtree)){
    #     DBI::dbExecute(
    #         conn,
    #         glue::glue(
    #             "CREATE INDEX my_idy ON {y_list$query_name} USING RTREE ({y_geom});"
    #         )
    #     )
    # }


    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT {paste0('tbl_y.', y_rest, collapse = ', ')},
                   tbl_x.{x_geom} AS {x_geom}
            FROM {x_list$query_name} tbl_x, {y_list$query_name} tbl_y
            WHERE {sel_pred}(tbl_x.{x_geom}, tbl_y.{y_geom})
            ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0('tbl_x.', x_rest, collapse = ', ')},
                   {paste0('tbl_y.', y_rest, collapse = ', ')},
                   tbl_x.{x_geom} AS {x_geom}
            FROM {x_list$query_name} tbl_x, {y_list$query_name} tbl_y
            WHERE {sel_pred}(tbl_x.{x_geom}, tbl_y.{y_geom})

        ")
        }

        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    ## 4. create the base query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT {paste0('tbl_y.', y_rest, collapse = ', ')},
                   ST_AsText(tbl_x.{x_geom}) AS {x_geom}
            FROM {x_list$query_name} tbl_x, {y_list$query_name} tbl_y
            WHERE {sel_pred}(tbl_x.{x_geom}, tbl_y.{y_geom})
        ")

    } else {
        tmp.query <- glue::glue("
            SELECT {paste0('tbl_x.', x_rest, collapse = ', ')},
                   {paste0('tbl_y.', y_rest, collapse = ', ')},
                   ST_AsText(tbl_x.{x_geom}) AS {x_geom}
            FROM {x_list$query_name} tbl_x, {y_list$query_name} tbl_y
            WHERE {sel_pred}(tbl_x.{x_geom}, tbl_y.{y_geom})
        ")

    }

    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF and return result
    data_sf <- convert_to_sf(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}



# has_rtree_index <- function(conn,  tbl_name){
#
#     temp_df <- DBI::dbGetQuery(
#         conn,
#         glue::glue("
#             SELECT *
#             FROM duckdb_indexes()
#             WHERE table_name = '{tbl_name}';
#           ")
#     )
#
#     check <- grepl(" RTREE ", temp_df$sql)
#     check <- ifelse(isTRUE(check), TRUE, FALSE)
#     return(check)
# }


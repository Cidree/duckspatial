


#' Spatial Filter
#'
#' Filters data spatially based on a spatial predicate
#'
#' @template x
#' @param y Y table with geometry column within the DuckDB database
#' @template predicate
#' @template conn_null
#' @template name
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns An sf object or TRUE (invisibly) for table creation
#'
#' @template spatial_join_predicates
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## filter countries touching argentina
#' ddbs_filter(conn = conn, "countries", "argentina", predicate = "touches")
#' 
#' ## filter without using a connection
#' ddbs_filter(countries_sf, argentina_sf, predicate = "touches")
#' }
ddbs_filter <- function(
    x,
    y,
    predicate = "intersects",
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

    # 2. Prepare params for query
    ## 2.1. select predicate
    sel_pred <- get_st_predicate(predicate)
    ## 2.2. get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    y_geom <- get_geom_name(conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
    ## error if crs_column not found
    assert_crs_column(crs_column, x_rest)

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, v1.{x_geom} AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
            WHERE {sel_pred}(v2.{y_geom}, v1.{x_geom})
        ")
        ## execute filter query
        DBI::dbExecute(conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    ## 4. Get data frame
    data_tbl <- DBI::dbGetQuery(
        conn, glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_AsText(v1.{x_geom}) AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
            WHERE {sel_pred}(v2.{y_geom}, v1.{x_geom})
        ")
    )

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





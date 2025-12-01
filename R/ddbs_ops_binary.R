
#' Calculates the intersection of two geometries
#'
#' Calculates the intersection of two geometries, and return a \code{sf} object
#' or creates a new table
#'
#' @template x
#' @param y A table with geometry column within the DuckDB database
#' @template conn_null
#' @template name
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns an sf object or TRUE (invisibly) for table creation
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
#' ## intersection inside the connection
#' ddbs_intersection("countries", "argentina", conn)
#' 
#' ## intersection without using a connection
#' ddbs_intersection(countries_sf, argentina_sf)
#' }
ddbs_intersection <- function(
    x,
    y,
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

    ## 2. get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    y_geom <- get_geom_name(conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Intersection(v1.{x_geom}, v2.{y_geom}) AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_Intersection(v1.{x_geom}, v2.{y_geom}) AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data fram
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Intersection(v1.{x_geom}, v2.{y_geom})) AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_AsText(ST_Intersection(v1.{x_geom}, v2.{y_geom})) AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
    }
    ## 4.2. retrieve results of the query
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






#' Calculates the difference of two geometries
#'
#' Calculates the geometric difference of two geometries, and returns a \code{sf}
#' object or creates a new table
#'
#' @template x
#' @param y A table with geometry column within the DuckDB database
#' @template conn_null
#' @template name
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns An sf object or TRUE (invisibly) for table creation
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
#' ## difference with a connection
#' ddbs_difference("countries", "argentina", conn)
#' 
#' ## difference without a connection
#' ddbs_difference(countries_sf, argentina_sf)
#' }
ddbs_difference <- function(x,
                            y,
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
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    y_geom <- get_geom_name(conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Difference(
                ST_MakeValid(v1.{x_geom}),
                ST_MakeValid(v2.{y_geom}))
            AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
        ")
        } else {
            tmp.query <- glue::glue("
                SELECT {paste0('v1.', x_rest, collapse = ', ')},
                       ST_Difference(
                         ST_MakeValid(v1.{x_geom}),
                         ST_MakeValid(v2.{y_geom})
                       ) AS {x_geom}
                FROM {x_list$query_name} v1, {y_list$query_name} v2
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))

        ## elminate empty
        DBI::dbExecute(conn, glue::glue("
          DELETE FROM {name_list$query_name}
          WHERE ST_IsEmpty({x_geom})
        "))

        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Difference(
                ST_MakeValid(v1.{x_geom}),
                ST_MakeValid(v2.{y_geom})))
            AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_AsText(ST_Difference(
                ST_MakeValid(v1.{x_geom}),
                ST_MakeValid(v2.{y_geom})))
            AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
        ")
    }
    ## 4.2. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF
    data_sf <- convert_to_sf(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    ## remove empty features
    data_sf <- data_sf[!sf::st_is_empty(data_sf), ]

    ## return result
    feedback_query(quiet)
    return(data_sf)


}

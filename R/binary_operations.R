
#' Calculates the intersection of two geometries
#'
#' Calculates the intersection of two geometries, and return a \code{sf} object
#' or creates a new table
#'
#' @template conn
#' @param x A table with geometry column within the DuckDB database. Data is returned
#' from this object
#' @param y A table with geometry column within the DuckDB database
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
#' ## database setup
#' conn <- dbConnect(duckdb())
#' ddbs_install(conn)
#' ddbs_load(conn)
#'
#' ## read data
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## intersection
#' ddbs_intersection(conn, "countries", "argentina")
#' }
ddbs_intersection <- function(conn,
                              x,
                              y,
                              name = NULL,
                              crs = NULL,
                              crs_column = "crs_duckspatial",
                              overwrite = FALSE,
                              quiet = FALSE) {

    ## 1. check conn
    dbConnCheck(conn)

    ## 2. get name of geometry column
    ## get convient names for x and y
    x_list <- get_query_name(x)
    y_list <- get_query_name(y)
    ## get name
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    y_geom <- get_geom_name(conn, y_list$query_name)
    if (length(x_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{x_list$query_name}>.")
    if (length(y_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{y_list$query_name}>.")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        if (overwrite) {
            DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name_list$query_name};"))

            if (isFALSE(quiet)) {
                cli::cli_alert_info("Table <{name_list$query_name}> dropped")
            }
        }

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

        if (isFALSE(quiet)) {
            cli::cli_alert_success("Query successful")
        }

        return(invisible(TRUE))
    }

    ## 4. create the base query
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
    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF and return result
    data_sf <- convert_to_sf(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    if (isFALSE(quiet)) cli::cli_alert_success("Query successful")
    return(data_sf)
}






#' Calculates the difference of two geometries
#'
#' Calculates the geometric difference of two geometries, and returns a \code{sf}
#' object or creates a new table
#'
#' @template conn
#' @param x A table with geometry column within the DuckDB database. Data is returned
#' from this object
#' @param y A table with geometry column within the DuckDB database
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
#' ## database setup
#' conn <- dbConnect(duckdb())
#' ddbs_install(conn)
#' ddbs_load(conn)
#'
#' ## read data
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## diffrence
#' ddbs_difference(conn, "countries", "argentina")
#' }
ddbs_difference <- function(conn,
                            x,
                            y,
                            name = NULL,
                            crs = NULL,
                            crs_column = "crs_duckspatial",
                            overwrite = FALSE,
                            quiet = FALSE) {

    ## 1. check conn
    dbConnCheck(conn)

    ## 2. get name of geometry column
    ## get convient names for x and y
    x_list <- get_query_name(x)
    y_list <- get_query_name(y)
    ## get name
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    y_geom <- get_geom_name(conn, y_list$query_name)
    if (length(x_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{x_list$query_name}>.")
    if (length(y_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{y_list$query_name}>.")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        if (overwrite) {
            DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name_list$query_name};"))

            if (isFALSE(quiet)) {
                cli::cli_alert_info("Table <{name_list$query_name}> dropped")
            }
        }

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

        if (isFALSE(quiet)) {
            cli::cli_alert_success("Query successful")
        }

        return(invisible(TRUE))
    }

    ## 4. create the base query
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
    ## send the query
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
    if (isFALSE(quiet)) cli::cli_alert_success("Query successful")
    return(data_sf)

    
}

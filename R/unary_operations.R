


#' Creates a buffer around geometries
#'
#' Calculates the buffer of geometries from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @param conn a connection object to a DuckDB database
#' @param x a table with a geometry column within the DuckDB database
#' @param distance a numeric value specifying the buffer distance. Units correspond to
#' the coordinate system of the geometry (e.g. degrees or meters)
#' @param name a character string of length one specifying the name of the table,
#' or a character string of length two specifying the schema and table names. If it's
#' NULL (the default), it will return the result as an \code{sf} object
#' @param crs the coordinates reference system of the data. Specify if the data
#' doesn't have a \code{crs_column}, and you know the CRS
#' @param crs_column a character string of length one specifying the column
#' storing the CRS (created automatically by \code{\link{ddbs_write_vector}}). Set
#' to NULL if absent
#' @param overwrite whether to overwrite the existing table if it exists. Ignored
#' when \code{name} is NULL
#'
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
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
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## buffer
#' ddbs_buffer(conn, "argentina", distance = 1)
#' }
ddbs_buffer <- function(conn,
                        x,
                        distance,
                        name = NULL,
                        crs = NULL,
                        crs_column = "crs_duckspatial",
                        overwrite = FALSE) {

    ## 1. check conn
    dbConnCheck(conn)

    ## 2. get name of geometry column
    ## get convient names
    x_list <- get_query_name(x)
    ## get name
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    if (length(x_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{x_list$query_name}>.")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        if (overwrite) {
            DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name_list$query_name};"))
            cli::cli_alert_info("Table <{name_list$query_name}> dropped")
        }

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Buffer({x_geom}, {distance}) as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Buffer({x_geom}, {distance}) as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        cli::cli_alert_success("Query successful")
        return(invisible(TRUE))
    }

    ## 4. create the base query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Buffer({x_geom}, {distance})) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_AsText(ST_Buffer({x_geom}, {distance})) as {x_geom} FROM {x_list$query_name};
        ")
    }
    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF
    if (is.null(crs)) {
        if (is.null(crs_column)) {
            data_sf <- data_tbl |>
                sf::st_as_sf(wkt = x_geom)
        } else {
            data_sf <- data_tbl |>
                sf::st_as_sf(wkt = x_geom, crs = data_tbl[1, crs_column])
            data_sf <- data_sf[, -which(names(data_sf) == crs_column)]
        }

    } else {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = x_geom, crs = crs)
    }

    cli::cli_alert_success("Query successful")
    return(data_sf)
}





#' Calculates the centroid of geometries
#'
#' Calculates the centroids of geometries from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @param conn a connection object to a DuckDB database
#' @param x a table with a geometry column within the DuckDB database
#' @param name a character string of length one specifying the name of the table,
#' or a character string of length two specifying the schema and table names. If it's
#' NULL (the default), it will return the result as an \code{sf} object
#' @param crs the coordinates reference system of the data. Specify if the data
#' doesn't have a \code{crs_column}, and you know the CRS
#' @param crs_column a character string of length one specifying the column
#' storing the CRS (created automatically by \code{\link{ddbs_write_vector}}). Set
#' to NULL if absent
#' @param overwrite whether to overwrite the existing table if it exists. Ignored
#' when \code{name} is NULL
#'
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
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
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## centroid
#' ddbs_centroid(conn, "argentina")
#' }
ddbs_centroid <- function(conn,
                            x,
                            name = NULL,
                            crs = NULL,
                            crs_column = "crs_duckspatial",
                            overwrite = FALSE) {

    ## 1. check conn
    dbConnCheck(conn)

    ## 2. get name of geometry column
    ## get convient names
    x_list <- get_query_name(x)
    ## get name
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    if (length(x_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{x_list$query_name}>.")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        if (overwrite) {
            DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name_list$query_name};"))
            cli::cli_alert_info("Table <{name_list$query_name}> dropped")
        }

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Centroid({x_geom}}) as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Centroid({x_geom}) as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        cli::cli_alert_success("Query successful")
        return(invisible(TRUE))
    }

    ## 4. create the base query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Centroid({x_geom})) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_AsText(ST_Centroid({x_geom})) as {x_geom} FROM {x_list$query_name};
        ")
    }
    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF
    if (is.null(crs)) {
        if (is.null(crs_column)) {
            data_sf <- data_tbl |>
                sf::st_as_sf(wkt = x_geom)
        } else {
            data_sf <- data_tbl |>
                sf::st_as_sf(wkt = x_geom, crs = data_tbl[1, crs_column])
            data_sf <- data_sf[, -which(names(data_sf) == crs_column)]
        }

    } else {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = x_geom, crs = crs)
    }

    cli::cli_alert_success("Query successful")
    return(data_sf)
}


## dbConnCheck

#' Check if a supported DuckDB connection
#'
#' @template conn
#'
#' @keywords internal
#' @returns TRUE (invisibly) for successful import
dbConnCheck <- function(conn) {
    if (inherits(conn, "duckdb_connection")) {
        return(invisible(TRUE))
    } else {
        cli::cli_abort("'conn' must be connection object: <duckdb_connection> from `duckdb`")
    }
}

#' Get column names in a DuckDB database
#'
#' @template conn
#' @param x name of the table
#' @param rest whether to return geometry column name, of the rest of the columns
#'
#' @keywords internal
#' @returns name of the geometry column of a table
get_geom_name <- function(conn, x, rest = FALSE) {
    info_tbl <- DBI::dbGetQuery(conn, glue::glue("PRAGMA table_info('{x}');"))
    if (rest) info_tbl[!info_tbl$type == "GEOMETRY", "name"] else info_tbl[info_tbl$type == "GEOMETRY", "name"]
}


#' Get names for the query
#'
#' @param name table name
#'
#' @keywords internal
#' @returns list with fixed names
get_query_name <- function(name) {
    if (length(name) == 2) {
        table_name <- name[2]
        schema_name <- name[1]
        query_name <- paste0(name, collapse = ".")
    } else {
        table_name   <- name
        schema_name <- "main"
        query_name <- name
    }
    list(
        table_name = table_name,
        schema_name = schema_name,
        query_name = query_name
    )
}




#' Converts from data frame to sf
#' 
#' Converts a table that has been read from DuckDB into an sf object
#'
#' @param data a tibble or data frame
#' @template crs
#' @param x_geom name of geometry
#'
#' @keywords internal
#' @returns sf
convert_to_sf <- function(data, crs, crs_column, x_geom) {
    if (is.null(crs)) {
        if (is.null(crs_column)) {
            data_sf <- data |>
                sf::st_as_sf(wkt = x_geom)
        } else {
            data_sf <- data |>
                sf::st_as_sf(wkt = x_geom, crs = data[1, crs_column])
            data_sf <- data_sf[, -which(names(data_sf) == crs_column)]
        }

    } else {
        data_sf <- data |>
            sf::st_as_sf(wkt = x_geom, crs = crs)
    }

}

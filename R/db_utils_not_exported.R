
## dbConnCheck

#' Check if a supported DuckDB connection
#'
#' @param conn A DuckDB connection
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
#' @param conn A DuckDB connection
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

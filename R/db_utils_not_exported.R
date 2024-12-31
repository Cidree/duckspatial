
## dbConnCheck

##' Check if a supported PostgreSQL connection
##'
##' @param conn A PostgreSQL connection
##'
##' @keywords internal
dbConnCheck <- function(conn) {
    if (inherits(conn, "duckdb_connection")) {
        return(invisible(TRUE))
    } else {
        cli::cli_abort("'conn' must be connection object: <duckdb_connection> from `duckdb`")
    }
}

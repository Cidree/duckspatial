

#' Check and create schema
#'
#' @param conn a connection object to a DuckDB database
#' @param name a character string with the name of the schema to be created
#'
#' @returns TRUE (invisibly) for successful schema creation
#' @export
#'
#' @examples
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#'
#' ## connect to in memory database
#' conn <- dbConnect(duckdb::duckdb())
#'
#' ## create a new schema
#' ddbs_create_schema(conn, "new_schema")
#'
#' ## check schemas
#' dbGetQuery(conn, "SELECT * FROM information_schema.schemata;")
#'
#' ## disconnect from db
#' dbDisconnect(conn)
#'
ddbs_create_schema <- function(conn, name) {

    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    ## Check if schema already exists
    namechar  <- DBI::dbQuoteString(conn,name)
    tmp.query <- paste0("SELECT EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = ",
                        namechar, ");")
    schema    <- DBI::dbGetQuery(conn, tmp.query)[1, 1]
    ## If it exists return TRUE, otherwise, create the schema
    if (schema) {
        cli::cli_abort("Schema <{name}> already exists.")
    } else {
        DBI::dbExecute(
            conn,
            glue::glue("CREATE SCHEMA {name};")
        )
        cli::cli_alert_success("Schema {name} created")
    }
    return(invisible(TRUE))

}

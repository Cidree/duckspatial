

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




#' Check CRS of a table
#'
#' @param conn a connection object to a DuckDB database
#' @param name a character string of length one specifying the name of the table,
#' or a character string of length two specifying the schema and table names.
#' @param crs_column a character string of length one specifying the column
#' storing the CRS (created automatically by \code{\link{ddbs_write_vector}})
#'
#' @returns CRS object
#' @export
#'
#' @examples
#' 1 + 1 # TODO
#'
ddbs_crs <- function(conn, name, crs_column = "crs_duckspatial") {

    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    ## convenient names of table and/or schema.table
    if (length(name) == 2) {
        table_name <- name[2]
        schema_name <- name[1]
        query_name <- paste0(name, collapse = ".")
    } else {
        table_name   <- name
        schema_name <- "main"
        query_name <- name
    }
    ## Check if table name exists
    if (!table_name %in% DBI::dbListTables(conn))
        cli::cli_abort("The provided name is not present in the database.")
    ## check if geometry column is present
    crs_data  <- DBI::dbGetQuery(
        conn, glue::glue("SELECT {crs_column} FROM {query_name} LIMIT 1;")
    ) |> as.character()

    # 2. Return CRS
    return(sf::st_crs(crs_data))
}

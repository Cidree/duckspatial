

ddbs_create_schema <- function(conn, name, quiet = FALSE) {

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
        return(glue::glue("Schema {name} already exists."))
    } else {
        dbExecute(
            conn,
            glue::glue("CREATE SCHEMA {name};")
        )
        if (!quiet) message(glue::glue("Schema {name} created."))
    }
}

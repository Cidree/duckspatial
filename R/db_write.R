

#' Title
#'
#' @param conn
#' @param data
#' @param name
#' @param quiet
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
ddbs_write_vector <- function(conn, data, name, quiet = FALSE, overwrite = FALSE) {

    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    ## Check if table name already exists
    if (name %in% DBI::dbListTables(conn) & !overwrite)
        stop("The provided name is already present in the database. Please, use `overwrite = TRUE` or choose a different name.")

    # 2. Write as geojson
    temp_file <- tempfile(fileext = ".geojson")
    sf::write_sf(data, temp_file)

    # 3. Write into the database
    if (overwrite) {
        dbExecute(
            conn,
            glue::glue("DROP TABLE IF EXISTS {name};")
        )
        message(glue::glue("Table {name} dropped"))
    }
    dbExecute(
        conn,
        glue::glue("CREATE TABLE {name} AS SELECT * FROM ST_Read('{temp_file}')")
    )

    # 4. Remove tempfile
    file.remove(temp_file)

    # 5. Message
    if (!quiet) {
        message(glue::glue("Table {name} successfully imported"))
        message("Note that SRID information is not stored in the database. These features may be added in the future.")
    }

}

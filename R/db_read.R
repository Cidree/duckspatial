

ddbs_read_vector <- function(conn, name, geom = "geom", crs = NULL, quiet = FALSE) {

    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    ## Check if table name exists
    if (!name %in% DBI::dbListTables(conn))
        stop("The provided name is not present in the database.")
    ## Check if geom columns name exists
    col_names <- DBI::dbListFields(conn, name)
    if (!geom %in% col_names) {
        col_names_colapsed <- paste(col_names, collapse = ", ")
        stop(glue::glue("The geometry column is not named <{geom}>. The column names in the table {name} are: {col_names_colapsed}"))
    }

    # 2. Retrieve data
    ## Get column names except geom
    no_geom_cols <- setdiff(
        DBI::dbListFields(conn, name),
        geom
    ) |> paste(collapse = ", ")
    ## Retrieve data as data frame
    data_tbl <- DBI::dbGetQuery(
        conn,
        glue::glue(
            "SELECT
      {no_geom_cols},
      ST_AsText({geom}) AS {geom}
      FROM {name}"
        )
    )
    ## Convert to sf
    if (is.null(crs)) {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = geom)
    } else {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = geom, crs = crs)
    }
    message(glue::glue("Table {name} successfully imported. Note that SRID is not currently stored in the database."))
    return(data_sf)

}

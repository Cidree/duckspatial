

#' Write an SF Object to a DuckDB Database
#'
#' This function writes a Simple Features (SF) object into a DuckDB database as a new table.
#' The table is created in the specified schema of the DuckDB database.
#'
#' @param conn a connection object to a DuckDB database
#' @param data a \code{sf} object to write to the DuckDB database
#' @param name a character string of length one specifying the name of the table,
#' or a character string of length two specifying the schema and table names.
#' @param overwrite whether to overwrite the existing table if it exists
#'
#' @returns TRUE (invisibly) for successful import
#' @export
#'
#' @examples
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' ## connect to in memory database
#' conn <- dbConnect(duckdb::duckdb())
#'
#' ## install the spatial exntesion
#' ddbs_install(conn)
#' ddbs_load(conn)
#'
#' ## create random points
#' random_points <- data.frame(
#'   id = 1:5,
#'   x = runif(5, min = -180, max = 180),  # Random longitude values
#'   y = runif(5, min = -90, max = 90)     # Random latitude values
#' )
#'
#' ## convert to sf
#' sf_points <- st_as_sf(random_points, coords = c("x", "y"), crs = 4326)
#'
#' ## insert data into the database
#' ddbs_write_vector(conn, sf_points, "points")
#'
#' ## read data back into R
#' ddbs_read_vector(conn, "points", crs = 4326)
#'
#' ## disconnect from db
#' dbDisconnect(conn)

ddbs_write_vector <- function(conn, data, name, overwrite = FALSE) {
    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    ## Check if table name already exists
    if (length(name) == 2) {
        table_name <- name[2]
        schema_name <- name[1]
        query_name <- paste0(name, collapse = ".")
    } else {
        table_name   <- name
        schema_name <- "main"
        query_name <- name
    }
    if (table_name %in% DBI::dbListTables(conn) & !overwrite)
        cli::cli_abort("The provided name is already present in the database. Please, use `overwrite = TRUE` or choose a different name.")

    # 2. Handle unsupported geometries
    unsupported_types <- c("GEOMETRYCOLLECTION")
    geom_types <- unique(sf::st_geometry_type(data))
    if (any(geom_types %in% unsupported_types)) {
        cli::cli_abort("Unsupported geometry types found: {paste(geom_types[geom_types %in% unsupported_types], collapse = ', ')}")
    }

    # 3. Handle overwrite
    if (overwrite) {
        DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {query_name};"))
        cli::cli_alert_info("Table <{query_name}> dropped")
    }

    # 4. Prepare data for writing - import as data frame with geom as binary
    ## Get geometry column name
    geom_name <- setdiff(names(data), names(sf::st_drop_geometry(data)))
    ## Extract geometry as binary and append to data frame
    wkb_data <- sf::st_as_binary(sf::st_geometry(data), EWKB = TRUE)
    data_df <- as.data.frame(data)
    data_df[[geom_name]] <- wkb_data  # Ensure raw data is preserved

    ## Write data into DuckDB
    DBI::dbWriteTable(conn, DBI::Id(schema = schema_name, table = table_name), data_df, overwrite = overwrite, field.types = c(geom_name = "BLOB"))
    ## Convert to spatial
    DBI::dbExecute(conn, glue::glue("
        ALTER TABLE {query_name}
        ALTER COLUMN {geom_name} SET DATA TYPE GEOMETRY USING ST_GeomFromWKB({geom_name});
    "))


    # 5. User feedback
    cli::cli_alert_success("Table {name} successfully imported")
    cli::cli_alert_info("Note that SRID information is not stored in the database. These features may be added in the future.")
    return(invisible(TRUE))
}

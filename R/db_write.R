
#' Write an SF Object to a DuckDB Database
#'
#' This function writes a Simple Features (SF) object into a DuckDB database as a new table.
#' The table is created in the specified schema of the DuckDB database.
#'
#' @param conn a connection object to a DuckDB database
#' @param data a \code{sf} object to write to the DuckDB database, or a local file
#' @param name a character string of length one specifying the name of the table,
#' or a character string of length two specifying the schema and table names.
#' @param overwrite whether to overwrite the existing table if it exists
#'
#' @returns TRUE (invisibly) for successful import
#' @export
#'
#' @examples
#' \donttest{
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
#' }
ddbs_write_vector <- function(conn, data, name, overwrite = FALSE) {

    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    ## convenient names of table and/or schema.table
    name_list <- get_query_name(name)
    ## Check if table name already exists
    if (name_list$query_name %in% DBI::dbListTables(conn) & !overwrite)
        cli::cli_abort("The provided name is already present in the database. Please, use `overwrite = TRUE` or choose a different name.")

    # 2. Handle overwrite
    if (overwrite) {
        DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name_list$query_name};"))
        cli::cli_alert_info("Table <{name_list$query_name}> dropped")
    }

    ## 3. insert data
    if (inherits(data, "sf")) {
        # 3. Handle unsupported geometries (TOO SLOW)
        # unsupported_types <- c("GEOMETRYCOLLECTION")
        # geom_types <- unique(sf::st_geometry_type(data))
        # if (any(geom_types %in% unsupported_types)) {
        #     cli::cli_abort("Unsupported geometry types found: {paste(geom_types[geom_types %in% unsupported_types], collapse = ', ')}")
        # }

        # 4. Prepare data for writing - import as data frame with geom as binary
        ## Get geometry column name
        geom_name <- setdiff(names(data), names(sf::st_drop_geometry(data)))
        ## Extract geometry as binary and append to data frame
        wkb_data <- sf::st_as_binary(sf::st_geometry(data), EWKB = TRUE)
        data_df <- as.data.frame(data)
        data_df[[geom_name]] <- wkb_data  # Ensure raw data is preserved

        ## Write data into DuckDB
        # duckdb::duckdb_register(conn, "temp_view", data_df, experimental = TRUE) # check later
        DBI::dbWriteTable(conn, DBI::Id(schema = name_list$schema_name, table = name_list$table_name), data_df, field.types = c(geom_name = "BLOB"))
        # DBI::dbExecute(conn, glue::glue("
        #     CREATE TABLE {name_list$query_name} AS
        #     SELECT {paste0(names(data_df), collapse = ', ')}
        #     FROM temp_view
        # "))
        ## Convert to spatial
        DBI::dbExecute(conn, glue::glue("
            ALTER TABLE {name_list$query_name}
            ALTER COLUMN {geom_name} SET DATA TYPE GEOMETRY USING ST_GeomFromWKB({geom_name});
        "))
        # duckdb::duckdb_unregister(conn, "temp_view") |> on.exit()
        ## CRS
        ## get data CRS
        data_crs <- sf::st_crs(data, parameters = TRUE)
        ## create new column with CRS as default value
        DBI::dbExecute(conn, glue::glue("
            ALTER TABLE {name_list$query_name}
            ADD COLUMN crs_duckspatial VARCHAR DEFAULT '{data_crs$srid}';
        "))

    } else {
        ## check file extension
        # file_ext <- sub(".*\\.", "", data)
        # if (file_ext == "parquet") {
        #     ## insert data
        #     DBI::dbExecute(
        #         conn,
        #         glue::glue("CREATE TABLE {name_list$query_name} AS SELECT * FROM read_parquet('{data}')")
        #     )
        #     ## specify geometry column
        #     ## - try to get geom column name
        #     metadata_df <- DBI::dbGetQuery(conn, glue::glue("DESCRIBE {name_list$query_name}"))
        #     geom_name <- metadata_df$column_name[grepl("STRUCT", metadata_df$column_type)]
        #     DBI::dbExecute(conn, glue::glue("
        #         ALTER TABLE {name_list$query_name}
        #         ALTER COLUMN {geom_name} SET DATA TYPE GEOMETRY USING ST_GeomFromWKB({geom_name});
        #     "))
        #     ## manage CRS
        #
        # } else {
            ## insert data
            DBI::dbExecute(
                conn,
                glue::glue("CREATE TABLE {name_list$query_name} AS SELECT * FROM ST_Read('{data}')")
            )
            ## get CRS
            meta_list <- DBI::dbGetQuery(conn, glue::glue("SELECT * FROM ST_READ_META('{data}')"))
            auth_name <- meta_list$layers[[1]]$geometry_fields[[1]]$crs$auth_name
            auth_code <- meta_list$layers[[1]]$geometry_fields[[1]]$crs$auth_code
            srid <- paste0(auth_name, ":", auth_code)
            ## create new column with CRS as default value
            DBI::dbExecute(conn, glue::glue("
            ALTER TABLE {name_list$query_name}
            ADD COLUMN crs_duckspatial VARCHAR DEFAULT '{srid}';
        "))
        # }


    }


    # 6. User feedback
    cli::cli_alert_success("Table {name} successfully imported")
    return(invisible(TRUE))

}



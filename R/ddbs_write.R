#' Write an SF Object to a DuckDB Database
#'
#' This function writes a Simple Features (SF) object into a DuckDB database as a new table.
#' The table is created in the specified schema of the DuckDB database.
#'
#' @template conn
#' @param data A \code{sf} object to write to the DuckDB database, or the path to
#'        a local file that can be read with `ST_READ`
#' @template name
#' @template overwrite
#' @param temp_view If `TRUE`, registers the `sf` object as a temporary Arrow-backed database 'view' using `ddbs_register_vector` instead of creating a persistent table. This is much faster but the view will not persist. Defaults to `FALSE`.
#' @template quiet
#'
#' @returns TRUE (invisibly) for successful import
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
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
ddbs_write_vector <- function(
    conn,
    data,
    name,
    overwrite = FALSE,
    temp_view = FALSE,
    quiet = FALSE
) {
    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)

    ## Handle temp_view
    if (temp_view) {
        return(ddbs_register_vector(conn, data, name, overwrite, quiet))
    }

    # Handle duckspatial_df/tbl_lazy - try cross-connection import first
    if (inherits(data, "duckspatial_df") || inherits(data, "tbl_lazy")) {
        source_conn <- get_conn_from_input(data)
        
        # Check if same connection (no import needed, just materialize)
        same_conn <- !is.null(source_conn) && identical(source_conn, conn)
        
        if (!same_conn && !is.null(source_conn)) {
            # Cross-connection: try efficient import strategies first
            # Use a temp name for the import view, we'll convert to table after
            temp_import_name <- paste0("temp_import_", gsub("-", "_", uuid::UUIDgenerate()))
            
            import_result <- tryCatch({
                import_view_to_connection(conn, source_conn, data, temp_import_name)
            }, error = function(e) NULL)
            
            if (!is.null(import_result)) {
                # Successful import - convert temp view to permanent table
                ## convenient names of table and/or schema.table
                name_list <- get_query_name(name)
                ## get schema.table available in the database
                tables_df <- ddbs_list_tables(conn)
                db_tables <- paste0(tables_df$table_schema, ".", tables_df$table_name) |>
                    sub(pattern = "^main\\.", replacement = "")
                ## Check if table name already exists
                if (name_list$query_name %in% db_tables & !overwrite)
                    cli::cli_abort("The provided name is already present in the database. Please, use `overwrite = TRUE` or choose a different name.")
                
                # Handle overwrite
                overwrite_table(name_list$query_name, conn, quiet, overwrite)
                
                # Create permanent table from the imported view
                DBI::dbExecute(conn, glue::glue(
                    "CREATE TABLE {name_list$query_name} AS SELECT * FROM {import_result$name}"
                ))
                
                # Cleanup temp view
                tryCatch(
                    DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {import_result$name}")),
                    error = function(e) NULL
                )
                import_result$cleanup()
                
                # Add CRS column if the source had CRS info
                if (inherits(data, "duckspatial_df")) {
                    crs_info <- ddbs_crs(data)
                    if (!is.null(crs_info$input) && !is.na(crs_info$input)) {
                        tryCatch({
                            DBI::dbExecute(conn, glue::glue("
                                ALTER TABLE {name_list$query_name}
                                ADD COLUMN crs_duckspatial VARCHAR DEFAULT '{crs_info$input}';
                            "))
                        }, error = function(e) NULL)
                    }
                }
                
                if (isFALSE(quiet)) {
                    cli::cli_alert_success("Table {name_list$query_name} imported via {import_result$method}")
                }
                return(invisible(TRUE))
            }
        }
        
        # Fallback: collect to sf and re-upload (slow but always works)
        if (inherits(data, "duckspatial_df")) {
             data <- ddbs_collect(data, as = "sf")
        } else {
             data <- dplyr::collect(data) |> sf::st_as_sf()
        }
    }

    ## convenient names of table and/or schema.table
    name_list <- get_query_name(name)
    ## get schema.table available in the database
    tables_df <- ddbs_list_tables(conn)
    db_tables <- paste0(tables_df$table_schema, ".", tables_df$table_name) |>
        sub(pattern = "^main\\.", replacement = "")
    ## Check if table name already exists
    if (name_list$query_name %in% db_tables & !overwrite)
        cli::cli_abort("The provided name is already present in the database. Please, use `overwrite = TRUE` or choose a different name.")

    # 2. Handle overwrite
    overwrite_table(name_list$query_name, conn, quiet, overwrite)

    ## 3. insert data
    if (inherits(data, "sf")) {
        # Remove existing crs_duckspatial column if present (e.g. from previous read)
        if ("crs_duckspatial" %in% names(data)) {
            data <- dplyr::select(data, -dplyr::any_of("crs_duckspatial"))
        }

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

        if (is.null(data_crs$srid) || is.na(data_crs$srid)) {
            cli::cli_alert_warning("No CRS found in the input data. The table will be created without CRS information.")
        } else {
            ## create new column with CRS as default value
            DBI::dbExecute(conn, glue::glue("
            ALTER TABLE {name_list$query_name}
            ADD COLUMN crs_duckspatial VARCHAR DEFAULT '{data_crs$srid}';
        "))
        }

    } else if (!is.character(data) || length(data) != 1) {
        cli::cli_abort("{.arg data} must be an {.cls sf} object, a {.cls duckspatial_df}, or a file path string.")
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
    if (isFALSE(quiet)) {
        cli::cli_alert_success("Table {name_list$query_name} successfully imported")
        }

    return(invisible(TRUE))

}

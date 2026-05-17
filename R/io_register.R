#' Register an SF Object as an Arrow Table in DuckDB
#'
#' This function registers a Simple Features (SF) object as a temporary Arrow-backed
#' view in a DuckDB database. This is a zero-copy operation and is significantly
#' faster than `ddbs_write_table` for workflows that do not require data to be
#' permanently materialized in the database.
#'
#' @inheritParams ddbs_write_table
#' @returns TRUE (invisibly) on successful registration.
#' @export
#' @examples
#' \dontrun{
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' conn <- ddbs_create_conn("memory")
#'
#' nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#'
#' ddbs_register_table(conn, nc, "nc_arrow_view")
#'
#' dbGetQuery(conn, "SELECT COUNT(*) FROM nc_arrow_view;")
#'
#' ddbs_stop_conn(conn)
#'}
ddbs_register_table <- function(
    conn,
    data,
    name,
    overwrite = FALSE,
    quiet = FALSE
) {
    # 1. Checks
    dbConnCheck(conn)
    name_list <- get_query_name(name)
    view_name <- name_list$query_name

    # Handle duckspatial_df/tbl_lazy by collecting to sf first
    data_sf <- if (inherits(data, "duckspatial_df")) {
        ddbs_collect(data, as = "sf")
    } else if (inherits(data, "tbl_lazy")) {
        dplyr::collect(data) |> sf::st_as_sf()
    } else if (inherits(data, "sf")) {
        data
    } else if (is.character(data) && length(data) == 1) {
        sf::st_read(data, quiet = TRUE)
    } else {
        cli::cli_abort(
            "{.arg data} must be an {.cls sf} object, {.cls duckspatial_df}, or a readable file path."
        )
    }

    tables_df <- ddbs_list_tables(conn)
    db_tables <- paste0(tables_df$table_schema, ".", tables_df$table_name) |>
        sub(pattern = "^main\\.", replacement = "")
    name_exists <- view_name %in% db_tables
    
    # Prefix only the table component if it's schema-qualified to avoid 
    # broken names like __raw_schema.table. Also, duckdb_register_arrow 
    # doesn't support schema-qualified names, so we use a single component.
    raw_view_name <- paste0("__raw_", gsub("\\.", "_", view_name))

    arrow_views <- try(
        duckdb::duckdb_list_arrow(conn),
        silent = TRUE
    )
    if (inherits(arrow_views, "try-error")) {
        arrow_views <- character(0)
    }
    
    arrow_exists <- view_name %in% arrow_views
    raw_exists <- raw_view_name %in% arrow_views

    if ((name_exists || arrow_exists || raw_exists) && !overwrite) {
        cli::cli_abort(
            "The provided view (or table) name is already present in the database. Please, use `overwrite = TRUE` or choose a different name."
        )
    }

    if (overwrite && (name_exists || arrow_exists || raw_exists)) {
        if (name_exists) {
            match_idx <- which(db_tables == view_name)[1]
            table_type <- tables_df$table_type[match_idx]
            drop_stmt <- if (
                !is.na(table_type) && identical(table_type, "VIEW")
            ) {
                glue::glue("DROP VIEW IF EXISTS {view_name};")
            } else {
                glue::glue("DROP TABLE IF EXISTS {view_name};")
            }
            DBI::dbExecute(conn, drop_stmt)
            if (isFALSE(quiet)) {
                cli::cli_alert_info("Existing object {view_name} dropped")
            }
        }
        
        # Also clean up any potential hidden raw arrow view
        if (raw_exists) {
            try(duckdb::duckdb_unregister_arrow(conn, raw_view_name), silent = TRUE)
        }
        
        if (arrow_exists) {
            try(
                duckdb::duckdb_unregister_arrow(conn, view_name),
                silent = TRUE
            )
        }
    }

    # Try to register geoarrow extensions when available
    try(
        DBI::dbExecute(conn, "CALL register_geoarrow_extensions();"),
        silent = TRUE
    )

    # 2. Register table
  
    ## First, split data and geometry
    df <- sf::st_drop_geometry(data_sf)
    wkb <- wk::as_wkb(sf::st_geometry(data_sf))

    ## Get original geometry column name and CRS
    geom_name <- attr(data_sf, "sf_column")
    crs_obj <- sf::st_crs(data_sf)
    
    # DuckDB accepts WKT in GEOMETRY('<crs>'), but GeoArrow metadata consumed by
    # DuckDB cannot serialize WKT here. Keep the two CRS representations separate.
    if (!is.na(crs_obj)) {
        if (!is.na(crs_obj$epsg)) {
            crs_input <- paste0("EPSG:", crs_obj$epsg)
        } else {
            crs_input <- crs_obj$wkt
        }
        geoarrow_crs <- if (!is.na(crs_obj$epsg)) crs_input else crs_obj$proj4string
        if (length(geoarrow_crs) == 0 || is.na(geoarrow_crs) || identical(geoarrow_crs, "")) {
            geoarrow_crs <- NULL
        }
    } else {
        crs_input <- NULL
        geoarrow_crs <- NULL
    }

    ## Estimate chunk size from a sample. This is needed to avoid OOM errors
    ## when creating the Arrow table for very large datasets. The chunk size is
    ## determined based on the size of the first 1000 rows
    n <- length(wkb)
    sample_n <- min(1000L, n)
    sample_bytes <- sum(vapply(unclass(wkb[seq_len(sample_n)]), length, integer(1)))
    avg_bytes_per_feature <- sample_bytes / sample_n

    ## Target ~500MB per chunk (safely under ~2GB limit)
    target_bytes <- 500L * 1024L^2
    chunk_size   <- max(1000L, floor(target_bytes / avg_bytes_per_feature))
    idx          <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

    ## Create:
    ## - Arrow table: when the dataset is small (<500MB)
    ## - Arrow Batch: when the dataset is large (>500MB)
    if (length(idx) == 1L) {
        ## Create a single Arrow table
        arrow_table <- {
            df[[geom_name]] <- geoarrow::as_geoarrow_vctr(
                wkb,
                schema = geoarrow::geoarrow_wkb(crs = geoarrow_crs)
            )
            arrow::Table$create(df)
        }
    } else {
        ## Create an Arrow RecordBatchReader for chunked processing
        batches <- lapply(idx, function(i) {
            chunk <- df[i, , drop = FALSE]
            chunk[[geom_name]] <- geoarrow::as_geoarrow_vctr(
                wkb[i],
                schema = geoarrow::geoarrow_wkb(crs = geoarrow_crs)
            )
            arrow::record_batch(chunk)
        })
        
        ## Create the table as a batch
        schema <- batches[[1]]$schema
        arrow_table <- arrow::RecordBatchReader$create(
            batches = batches, 
            schema = schema
        )
    }

    ## Register the raw Arrow table under a hidden name
    tryCatch({
        duckdb::duckdb_register_arrow(conn, raw_view_name, arrow_table)
        
        ## Wrap it in a user-facing typed view
        q_geom <- DBI::dbQuoteIdentifier(conn, geom_name)
        
        # If view_name is qualified, DuckDB doesn't allow TEMP VIEW in other schemas.
        # We use a regular VIEW if qualified, or a TEMP VIEW if not.
        view_type <- if (name_list$schema_name != "main") "VIEW" else "TEMP VIEW"

        if (!is.null(crs_input)) {
            # Escape single quotes in WKT for SQL safety
            safe_crs <- gsub("'", "''", crs_input)
            DBI::dbExecute(conn, glue::glue(
                "CREATE OR REPLACE {view_type} {view_name} AS ",
                "SELECT * EXCLUDE {q_geom}, ",
                "({q_geom}::GEOMETRY('{safe_crs}')) AS {q_geom} ",
                "FROM {raw_view_name}"
            ))
        } else {
            # No CRS, just create a direct view casting to generic GEOMETRY
            DBI::dbExecute(conn, glue::glue(
                "CREATE OR REPLACE {view_type} {view_name} AS ",
                "SELECT * EXCLUDE {q_geom}, ",
                "({q_geom}::GEOMETRY) AS {q_geom} ",
                "FROM {raw_view_name}"
            ))
        }
    }, error = function(e) {
        # Cleanup on failure to avoid stale registrations
        try(duckdb::duckdb_unregister_arrow(conn, raw_view_name), silent = TRUE)
        stop(e)
    })

    ## User feedback
    if (isFALSE(quiet)) {
        cli::cli_alert_success("Temporary view {view_name} registered")
    }

    invisible(TRUE)
}

# ddbs_register_table <- function(
#     conn,
#     data,
#     name,
#     overwrite = FALSE,
#     quiet = FALSE
# ) {
#     # 1. Checks
#     dbConnCheck(conn)
#     name_list <- get_query_name(name)
#     view_name <- name_list$query_name

#     # Handle duckspatial_df/tbl_lazy by collecting to sf first
#     data_sf <- if (inherits(data, "duckspatial_df")) {
#         ddbs_collect(data, as = "sf")
#     } else if (inherits(data, "tbl_lazy")) {
#         dplyr::collect(data) |> sf::st_as_sf()
#     } else if (inherits(data, "sf")) {
#         data
#     } else if (is.character(data) && length(data) == 1) {
#         sf::st_read(data, quiet = TRUE)
#     } else {
#         cli::cli_abort(
#             "{.arg data} must be an {.cls sf} object, {.cls duckspatial_df}, or a readable file path."
#         )
#     }

#     tables_df <- ddbs_list_tables(conn)
#     db_tables <- paste0(tables_df$table_schema, ".", tables_df$table_name) |>
#         sub(pattern = "^main\\.", replacement = "")
#     name_exists <- view_name %in% db_tables
#     arrow_views <- try(
#         duckdb::duckdb_list_arrow(conn),
#         silent = TRUE
#     )
#     arrow_exists <- if (inherits(arrow_views, "try-error")) {
#         FALSE
#     } else {
#         view_name %in% arrow_views
#     }

#     if ((name_exists || arrow_exists) && !overwrite) {
#         cli::cli_abort(
#             "The provided view (or table) name is already present in the database. Please, use `overwrite = TRUE` or choose a different name."
#         )
#     }

#     if (overwrite && (name_exists || arrow_exists)) {
#         if (name_exists) {
#             match_idx <- which(db_tables == view_name)[1]
#             table_type <- tables_df$table_type[match_idx]
#             drop_stmt <- if (
#                 !is.na(table_type) && identical(table_type, "VIEW")
#             ) {
#                 glue::glue("DROP VIEW IF EXISTS {view_name};")
#             } else {
#                 glue::glue("DROP TABLE IF EXISTS {view_name};")
#             }
#             DBI::dbExecute(conn, drop_stmt)
#             if (isFALSE(quiet)) {
#                 cli::cli_alert_info("Existing object {view_name} dropped")
#             }
#         }
#         if (arrow_exists) {
#             try(
#                 duckdb::duckdb_unregister_arrow(conn, view_name),
#                 silent = TRUE
#             )
#         }
#     }

#     # Try to register geoarrow extensions when available
#     try(
#         DBI::dbExecute(conn, "CALL register_geoarrow_extensions();"),
#         silent = TRUE
#     )

#     # 2. Register table
#     df <- sf::st_drop_geometry(data_sf)
#     wkb <- wk::as_wkb(sf::st_geometry(data_sf))

#     # Get original geometry column name
#     geom_col_name <- attr(data_sf, "sf_column")

#     # Use geoarrow to create a geoarrow vector from WKB
#     # Assign to original geometry column name instead of hardcoded "geometry"
#     df[[geom_col_name]] <- geoarrow::as_geoarrow_vctr(
#         wkb,
#         schema = geoarrow::geoarrow_wkb()
#     )

#     # Get the CRS (however, right now it doesnt recognize the CRS and defaults to GEOMETRY 
#     # without SRID, which is not ideal)
#     # data_crs <- get_geometry_type_duckdb(data_sf)
#     data_crs <- ddbs_crs(data_sf)

#     # arrow_table <- tryCatch({
#     #    arrow::Table$create(df)
#     # }, error = function(e) {
#     #    # Fallback to standard WKB (binary) if geoarrow fails
#     #    # (e.g. "NotImplemented: MakeBuilder: cannot construct builder for type geoarrow.wkb")
#     #    df[[geom_col_name]] <- wkb
#     #    arrow::Table$create(df)
#     # })
#     arrow_table <- tryCatch({
#         # Attach CRS to the geometry column via geoarrow type
#         df[[geom_col_name]] <- as_geoarrow_vctr(
#             df[[geom_col_name]],
#             type = geoarrow_wkb(crs = data_crs$input)
#         )
#         arrow::Table$create(df)
#         }, error = function(e) {
#         # Fallback: use raw WKB and embed CRS metadata manually in the field
#         df[[geom_col_name]] <- wkb
#         arrow::Table$create(df)
#     })

#     duckdb::duckdb_register_arrow(conn, view_name, arrow_table)

#     if (isFALSE(quiet)) {
#         cli::cli_alert_success("Temporary view {view_name} registered")
#     }

#     invisible(TRUE)
# }




#' Register an SF Object as an Arrow Table in DuckDB
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `ddbs_register_vector()` was renamed to \code{\link{ddbs_register_table}}.
#'
#' @inheritParams ddbs_register_table
#' @returns TRUE (invisibly) on successful registration.
#' @export
#' @keywords internal
ddbs_register_vector <- function(
    conn,
    data,
    name,
    overwrite = FALSE,
    quiet = FALSE) {
    
    lifecycle::deprecate_soft(
        when    = "1.0.0",
        what    = "ddbs_register_vector()",
        with    = "ddbs_register_table()"
    )
    
    ddbs_register_table(
        conn = conn,
        data = data,
        name = name,
        overwrite = overwrite,
        quiet = quiet
    )
}

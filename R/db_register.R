#' Register an SF Object as an Arrow Table in DuckDB
#'
#' This function registers a Simple Features (SF) object as a temporary Arrow-backed
#' view in a DuckDB database. This is a zero-copy operation and is significantly
#' faster than `ddbs_write_vector` for workflows that do not require data to be
#' permanently materialized in the database.
#'
#' @inheritParams ddbs_write_vector
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
#' ddbs_register_vector(conn, nc, "nc_arrow_view")
#'
#' dbGetQuery(conn, "SELECT COUNT(*) FROM nc_arrow_view;")
#'
#' ddbs_stop_conn(conn, shutdown = TRUE)
#'}
ddbs_register_vector <- function(
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

    data_sf <- if (inherits(data, "sf")) {
        data
    } else if (is.character(data) && length(data) == 1) {
        sf::st_read(data, quiet = TRUE)
    } else {
        cli::cli_abort(
            "{.arg data} must be an {.cls sf} object or a readable file path."
        )
    }

    tables_df <- ddbs_list_tables(conn)
    db_tables <- paste0(tables_df$table_schema, ".", tables_df$table_name) |>
        sub(pattern = "^main\\.", replacement = "")
    name_exists <- view_name %in% db_tables
    arrow_views <- try(
        duckdb::duckdb_list_arrow(conn),
        silent = TRUE
    )
    arrow_exists <- if (inherits(arrow_views, "try-error")) {
        FALSE
    } else {
        view_name %in% arrow_views
    }

    if ((name_exists || arrow_exists) && !overwrite) {
        cli::cli_abort(
            "The provided view (or table) name is already present in the database. Please, use `overwrite = TRUE` or choose a different name."
        )
    }

    if (overwrite && (name_exists || arrow_exists)) {
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
    df <- sf::st_drop_geometry(data_sf)
    wkb <- wk::as_wkb(sf::st_geometry(data_sf))

    # Get original geometry column name
    geom_col_name <- attr(data_sf, "sf_column")

    # Use geoarrow to create a geoarrow vector from WKB
    # Assign to original geometry column name instead of hardcoded "geometry"
    df[[geom_col_name]] <- geoarrow::as_geoarrow_vctr(
        wkb,
        schema = geoarrow::geoarrow_wkb()
    )

    # Add CRS column
    data_crs <- sf::st_crs(data_sf, parameters = TRUE)
    crs_value <- if (!is.null(data_crs$srid) && nchar(data_crs$srid) > 0) {
        data_crs$srid
    } else {
        data_crs$Wkt
    }
    df$crs_duckspatial <- crs_value

    arrow_table <- arrow::Table$create(df)

    duckdb::duckdb_register_arrow(conn, view_name, arrow_table)

    if (isFALSE(quiet)) {
        cli::cli_alert_success("Temporary view {view_name} registered")
    }

    invisible(TRUE)
}

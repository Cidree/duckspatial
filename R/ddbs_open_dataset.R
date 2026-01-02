#' Open spatial dataset lazily via DuckDB
#'
#' Reads spatial data directly from disk using DuckDB's spatial extension
#' and returns a \code{duckspatial_df} object for lazy processing.
#'
#' @param path Path to spatial file. Supports GeoJSON, GeoPackage, Shapefile,
#'   FlatGeoBuf, and other GDAL-supported formats.
#' @param crs Coordinate reference system. Can be an EPSG code (e.g., 4326),
#'   a CRS string, or an \code{sf} crs object. If \code{NULL} (default),
#'   attempts to auto-detect from the file using DuckDB's ST_Read_Meta.
#' @param layer Layer name or index to read. Default is NULL (first layer).
#' @param geom_col Name of the geometry column. Default is \code{"geom"} which
#'   is what DuckDB's ST_Read returns.
#' @param conn DuckDB connection to use. If NULL, uses the default connection.
#' @param spatial_filter Optional WKB geometry to filter spatially.
#' @param spatial_filter_box Optional bounding box (as BOX_2D) to filter spatially.
#'
#' @returns A \code{duckspatial_df} object - a lazy spatial data frame backed
#'   by DuckDB via dbplyr.
#'
#' @details
#' This function uses DuckDB's native \code{ST_Read} function to read spatial
#' data directly, bypassing the need for the sf package to load data. CRS is
#' automatically detected from the file metadata using \code{ST_Read_Meta}.
#'
#' Supported formats include: ESRI Shapefile (.shp), GeoPackage (.gpkg),
#' GeoJSON (.json, .geojson), FlatGeoBuf (.fgb), and others supported by GDAL.
#'
#' @seealso \code{\link{as_duckspatial_df}}, \code{\link{ddbs_collect}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#'
#' # Read shapefile - CRS auto-detected
#' nc <- ddbs_open_dataset(system.file("shape/nc.shp", package = "sf"))
#' print(nc)
#'
#' # Read GeoJSON
#' countries <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", package = "duckspatial")
#' )
#'
#' # Use with duckspatial functions
#' result <- countries |>
#'   ddbs_buffer(distance = 0.1) |>
#'   ddbs_collect()
#' }
ddbs_open_dataset <- function(path, 
                               crs = NULL, 
                               layer = NULL,
                               geom_col = "geom",
                               conn = NULL,
                               spatial_filter = NULL,
                               spatial_filter_box = NULL) {

  # Get or create connection
  if (is.null(conn)) {
    conn <- ddbs_default_conn()
  }
  
  # Ensure spatial extension is loaded
  ddbs_install(conn, quiet = TRUE)
  ddbs_load(conn, quiet = TRUE)
  
  # 1. Auto-detect CRS using helper
  if (is.null(crs)) {
      crs <- get_file_crs(path, conn)
  } else if (!inherits(crs, "crs")) {
      crs <- sf::st_crs(crs)
  }

  # 2. Resolve geometry column if not provided
  if (is.null(geom_col)) {
    geom_col <- "geom" # Default for ST_Read
  }
  
  # 2. Build ST_Read options (this part remains as it was before the change)
  st_read_opts <- c()
  if (!is.null(layer)) {
    st_read_opts <- c(st_read_opts, glue::glue("layer = '{layer}'"))
  }
  
  opts_str <- if (length(st_read_opts) > 0) {
    paste0(", ", paste(st_read_opts, collapse = ", "))
  } else {
    ""
  }
  
  # 3. Create temp view using ST_Read
  # With dplyr_reconstruct properly implemented, dbplyr preserves class and attributes.
  # The new collect() method extracts SQL and wraps with ST_AsWKB at collection time.
  # No need for pre-computed WKB columns anymore!
  
  view_name <- ddbs_temp_view_name()
  
  # Detect geometry column name from file
  cols_query <- glue::glue("DESCRIBE SELECT * FROM ST_Read('{path}'{opts_str})")
  cols_df <- tryCatch({
    DBI::dbGetQuery(conn, cols_query)
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(cols_df)) {
    ctype <- if("column_type" %in% names(cols_df)) cols_df$column_type else cols_df$data_type
    geom_cols <- cols_df$column_name[grepl("GEOMETRY", ctype, ignore.case = TRUE)]
    if (length(geom_cols) > 0) geom_col <- geom_cols[1]
  }
  
  # Simple view - just SELECT * from ST_Read
  view_query <- glue::glue("
    CREATE OR REPLACE TEMPORARY VIEW {view_name} AS 
    SELECT * FROM ST_Read('{path}'{opts_str})
  ")

  DBI::dbExecute(conn, view_query)
  
  # 4. Return as duckspatial_df
  # 4. Return as duckspatial_df
  # Use standard tbl() which returns a tbl_duckdb_connection (tbl_lazy)
  duck_tbl <- dplyr::tbl(conn, view_name)
  
  # Pass view_name as source_table so get_query_list can use it directly
  new_duckspatial_df(duck_tbl, crs = crs, geom_col = geom_col, source_table = view_name)
}

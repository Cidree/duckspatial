#' Create temporary spatial file for testing
#'
#' This helper creates a temporary spatial file in an isolated directory
#' that is automatically cleaned up when the calling function exits.
#' Useful for tests involving file-based spatial formats (e.g., shapefiles)
#' that create multiple associated files.
#'
#' @param data A `duckspatial_df`, `tbl_lazy`, or `sf` object to write
#' @param format Output format (e.g., "geojson", "gpkg", "shp", "fgb").
#'   Defaults to "geojson".
#' @param conn DuckDB connection. If NULL, attempts to infer from data.
#' @param envir Environment for cleanup context. Defaults to parent.frame()
#'   to ensure cleanup happens in the test context.
#' @param ... Additional arguments passed to ddbs_write_dataset()
#'
#' @return Character string path to the created file
#'
#' @examples
#' \dontrun{
#' test_that("example test", {
#'   conn <- ddbs_temp_conn()
#'   ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"))
#'   
#'   # Create temp file - automatically cleaned up after test
#'   temp_file <- ddbs_create_temp_spatial_file(ds, format = "shp", conn = conn)
#'   expect_true(file.exists(temp_file))
#' })
#' }
#' @noRd
ddbs_create_temp_spatial_file <- function(
  data,
  format = "geojson",
  conn = NULL,
  envir = parent.frame(),
  ...
) {
  # Create isolated temporary directory with automatic cleanup
  temp_dir <- withr::local_tempdir(.local_envir = envir)
  
  # Determine file extension from format
  ext <- if (format %in% c("parquet", "csv")) {
    format
  } else {
    # For GDAL formats, use common extension mappings
    switch(tolower(format),
      "geojson" = "geojson",
      "gpkg" = "gpkg",
      "shp" = "shp",
      "esri shapefile" = "shp",
      "fgb" = "fgb",
      "flatgeobuf" = "fgb",
      "kml" = "kml",
      "gpx" = "gpx",
      "gml" = "gml",
      "sqlite" = "sqlite",
      tolower(format) # fallback to format as-is
    )
  }
  
  # Generate file path
  file_path <- file.path(temp_dir, paste0("test_data.", ext))
  
  # Write the dataset
  ddbs_write_dataset(
    data = data,
    path = file_path,
    format = format,
    conn = conn,
    quiet = TRUE,
    ...
  )
  
  return(file_path)
}

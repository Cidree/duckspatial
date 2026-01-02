
#' Detect file format from path or URL
#'
#' @param path Character string used as path or URL
#' @return Character string: "parquet", "sf", or other extension
#' @noRd
get_file_format <- function(path) {
  # 1. Handle S3 URIs -> assume parquet default if no extension
  if (grepl("^s3://", path)) {
    # Check if it has an extension
    ext <- tools::file_ext(path)
    if (ext == "") return("parquet")
  }
  
  # 2. Get extension
  ext <- tolower(tools::file_ext(path))
  
  # 3. Check for specific extensions
  if (ext %in% c("parquet")) {
    return("parquet")
  }
  if (ext == "shp") {
     return("shp")
  }
  if (grepl("osm\\.pbf$", path) || ext == "osm.pbf") {
     return("osm.pbf")
  }
  
  # 4. Check for known spatial extensions -> "sf" (handled by ST_Read)
  # This list aligns with formats commonly handled by GDAL/ST_Read
  spatial_exts <- c("shp", "gpkg", "fgb", "json", "geojson", "kml", "gpx")
  if (ext %in% spatial_exts) {
    return("sf")
  }
  
  # 5. Default fallback
  if (ext == "") {
    # If no extension, default to parquet as it's the primary analytical format
    return("parquet")
  }
  
  # Return the extension purely for information/fallback usage
  return(ext)
}

#' Get CRS from Parquet metadata
#' 
#' @param path Path to parquet file
#' @param conn DuckDB connection
#' @return crs object or NULL
#' @noRd
get_parquet_crs <- function(path, conn) {
  # Try to read GeoParquet metadata from KV metadata
  # We use SQL-side JSON extraction to avoid R dependencies
  
  tryCatch({
    # 1. Ensure json extension is available for subsequent parsing
    ddbs_install(conn, extension = "json", quiet = TRUE)
    ddbs_load(conn, extension = "json", quiet = TRUE)
    
    # 2. Extract CRS PROJJSON using a single SQL query
    # We decode key and value blobs, cast value to JSON, 
    # and extract crs for the primary column.
    query <- glue::glue("
      WITH geo_meta AS (
        SELECT decode(value)::JSON as meta
        FROM parquet_kv_metadata('{path}')
        WHERE decode(key) = 'geo'
      )
      SELECT 
        meta->'columns'->(meta->>'primary_column')->>'crs' as crs
      FROM geo_meta
      LIMIT 1
    ")
    
    res <- DBI::dbGetQuery(conn, query)
    
    if (nrow(res) == 0 || is.na(res$crs[1]) || res$crs[1] == "null") {
      return(NULL)
    }
    
    # 3. Convert PROJJSON string to sf CRS object
    return(sf::st_crs(res$crs[1]))
    
  }, error = function(e) {
    # If anything fails (file not found, bad format, no json extension), warn and return NULL
    cli::cli_warn("Failed to extract GeoParquet CRS: {e$message}")
    NULL
  })
}

#' Map extension to common GDAL driver names
#' @noRd
get_driver_map <- function() {
  list(
    shp = "ESRI Shapefile",
    gpkg = "GPKG",
    fgb = "FlatGeoBuf",
    json = "GeoJSON",
    geojson = "GeoJSON",
    kml = "KML",
    gml = "GML",
    gpx = "GPX",
    sqlite = "SQLite",
    csv = "CSV",
    tab = "MapInfo File",
    mif = "MapInfo File",
    geojsonl = "GeoJSON",
    mvt = "MVT",
    dgn = "DGN",
    gdb = "OpenFileGDB", # or "FileGDB" depending on build, OpenFileGDB is standard in GDAL now
    gxt = "Geoconcept",
    xml = "GML" # generic
  )
}

#' Validate extension support against ST_Drivers
#' @noRd
validate_driver_availability <- function(path, conn) {
  # 1. Get extension
  ext <- tolower(tools::file_ext(path))
  if (ext == "" || ext == "parquet") return(TRUE) # Parquet is native
  
  # 2. Map to driver(s)
  driver_map <- get_driver_map()
  expected_driver <- driver_map[[ext]]
  
  if (is.null(expected_driver)) {
     # Unknown extension: we don't block it, just warn or pass?
     # User asked to "stop if format does not work".
     # If we don't know the extension, we can't check support.
     # GDAL might still open it. Let's pass.
     return(TRUE)
  }
  
  # 3. Check against ST_Drivers
  # Cache this? For now, fetch is fast enough (in-memory table usually)
  available_drivers <- tryCatch({
       DBI::dbGetQuery(conn, "SELECT short_name, long_name, can_open FROM ST_Drivers()")
  }, error = function(e) return(NULL))
  
  if (is.null(available_drivers)) return(TRUE)
  
  # Match short_name or long_name
  # expected_driver is "ESRI Shapefile" etc.
  
  # Check if our expected driver is in the list AND can_open is TRUE
  # Using partial matching or exact? short_name is usually exact ID.
  
  # Special fix: "GeoJSON" might appear as "GeoJSON" short name.
  
  match_idx <- which(
    trimws(tolower(available_drivers$short_name)) == tolower(expected_driver) | 
    trimws(tolower(available_drivers$long_name)) == tolower(expected_driver)
  )
  
  if (length(match_idx) == 0) {
      cli::cli_abort(c(
          "Format {.val {ext}} requires driver {.val {expected_driver}}, but it is not available in the loaded DuckDB spatial extension.",
          "i" = "Run {.code ddbs_drivers(conn)} to see available drivers."
      ))
  }
  
  if (!available_drivers$can_open[match_idx[1]]) {
       cli::cli_abort(c(
          "Driver {.val {expected_driver}} is present but does not support opening files.",
           "i" = "Format: {.val {ext}}"
      ))
  }
  
  return(TRUE)
}

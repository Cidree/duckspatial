
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
    # If no extension, we return "unknown" to trigger auto-detection logic
    return("unknown")
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
    # Native / Common
    parquet = "parquet",
    csv = "CSV",
    
    # GDAL Spatial
    shp = "ESRI Shapefile",
    gpkg = "GPKG",
    fgb = "FlatGeoBuf",
    json = "GeoJSON",
    geojson = "GeoJSON",
    kml = "KML",
    gml = "GML",
    gpx = "GPX",
    sqlite = "SQLite",
    tab = "MapInfo File",
    mif = "MapInfo File",
    geojsonl = "GeoJSON",
    mvt = "MVT",
    dgn = "DGN",
    gdb = "OpenFileGDB", 
    gxt = "Geoconcept",
    xml = "GML"
  )
}

#' Get driver name from format/extension
#' @noRd
get_driver_name <- function(fmt) {
  map <- get_driver_map()
  if (fmt %in% names(map)) {
    return(map[[fmt]])
  }
  return(NULL)
}

#' Check if format implies spatial data
#' @noRd
is_spatial_format <- function(fmt) {
  # GDAL formats usually imply spatial data.
  spatial_formats <- c("shp", "gpkg", "fgb", "json", "geojson", "kml", "gpx")
  
  # Check if extension is one of these
  if (fmt %in% spatial_formats) return(TRUE)
  
  # Check driver map (excluding native formats which we handle separately or aren't strictly spatial)
  map <- get_driver_map()
  map$parquet <- NULL
  map$csv <- NULL
  
  driver <- get_driver_name(fmt)
  if (!is.null(driver) && driver %in% unlist(map)) return(TRUE)
  
  return(FALSE)
}

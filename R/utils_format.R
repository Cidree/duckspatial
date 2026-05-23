
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
  if (ext %in% c("duckdb", "db", "ddb")) {
    return("duckdb")
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

duckdb_file_extensions <- function() {
  c("duckdb", "db", "ddb")
}

has_duckdb_file_extension <- function(path) {
  if (!is.character(path) || length(path) != 1) {
    return(FALSE)
  }
  tolower(tools::file_ext(path)) %in% duckdb_file_extensions()
}

duckdb_file_info <- function(path) {
  result <- list(
    exists = FALSE,
    size = NA_real_,
    valid = FALSE,
    empty = FALSE
  )

  if (!is.character(path) || length(path) != 1 || grepl("^[[:alpha:]][[:alnum:].+-]*://", path)) {
    return(result)
  }

  if (!file.exists(path)) {
    return(result)
  }

  info <- file.info(path)
  result$exists <- TRUE
  result$size <- info$size
  result$empty <- isTRUE(result$size == 0)

  if (is.na(result$size) || isTRUE(info$isdir) || result$size < 12) {
    return(result)
  }

  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  bytes <- readBin(con, "raw", n = 12)

  result$valid <- length(bytes) >= 12 && identical(bytes[9:12], charToRaw("DUCK"))
  result
}

#' Get CRS from Parquet metadata
#' 
#' @param path Path to parquet file
#' @param conn DuckDB connection
#' @return crs object or NULL
#' @noRd
get_parquet_crs <- function(path, conn) {
  # Try to read GeoParquet metadata from KV metadata
  # We extract the raw blob and parse in R to be more robust across platforms
  
  tryCatch({
    safe_path <- DBI::dbQuoteString(conn, path)
    
    # 1. Extract the raw 'geo' metadata value
    # We use decode(key) to handle blob keys if present
    query <- glue::glue("
      SELECT value
      FROM parquet_kv_metadata({safe_path})
      WHERE decode(key) = 'geo'
      LIMIT 1
    ")    
    res <- DBI::dbGetQuery(conn, query)
    
    if (nrow(res) == 0 || is.null(res$value[[1]])) {
      return(NULL)
    }
    
    # 2. Parse JSON in R
    # DuckDB returns BLOB as a raw vector in a list
    meta_str <- rawToChar(res$value[[1]])
    meta <- jsonlite::fromJSON(meta_str, simplifyVector = FALSE)
    
    # 3. Extract CRS for the primary column
    primary_col <- meta$primary_column
    if (is.null(primary_col) || !primary_col %in% names(meta$columns)) {
       return(NULL)
    }
    
    crs_data <- meta$columns[[primary_col]]$crs
    if (is.null(crs_data)) {
      return(NULL)
    }
    
    # 4. Convert to sf CRS object
    # If it's a list (PROJJSON), try to extract EPSG or convert to string
    if (is.list(crs_data)) {
      # Try to extract EPSG code for better compatibility
      # Standard PROJJSON has id: {authority: "...", code: ...}
      authority <- crs_data$id$authority
      code <- crs_data$id$code
      
      if (!is.null(authority) && !is.null(code)) {
        return(sf::st_crs(paste0(authority, ":", code)))
      }
      
      # Handle cases where id might be a list of one element
      if (is.list(crs_data$id) && length(crs_data$id) > 0) {
        authority <- crs_data$id[[1]]$authority
        code <- crs_data$id[[1]]$code
        if (!is.null(authority) && !is.null(code)) {
          return(sf::st_crs(paste0(authority, ":", code)))
        }
      }
      
      # Fallback to JSON string for sf::st_crs
      crs_data <- jsonlite::toJSON(crs_data, auto_unbox = TRUE)
    }
    
    return(sf::st_crs(crs_data))
    
  }, error = function(e) {
    # If anything fails (file not found, bad format), warn and return NULL
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

#' Open spatial dataset lazily via DuckDB
#'
#' Reads spatial data directly from disk using DuckDB's spatial extension or 
#' native Parquet reader, returning a \code{duckspatial_df} object for lazy processing.
#'
#' @param path Path to spatial file. Supports Parquet (`.parquet`, with optional GeoParquet metadata),
#'   GeoJSON, GeoPackage, Shapefile, FlatGeoBuf, OSM PBF, and other GDAL-supported formats.
#' @param crs Coordinate reference system. Can be an EPSG code (e.g., 4326),
#'   a CRS string, or an \code{sf} crs object. If \code{NULL} (default),
#'   attempts to auto-detect from the file.
#' @param layer Layer name or index to read (ST_Read only). Default is NULL (first layer).
#' @param geom_col Name of the geometry column. Default is \code{NULL}, which attempts 
#'   auto-detection.
#' @param conn DuckDB connection to use. If NULL, uses the default connection.
#' 
#' @param parquet_binary_as_string Logical. (Parquet) If TRUE, load binary columns as strings.
#' @param parquet_file_row_number Logical. (Parquet) If TRUE, include a `file_row_number` column.
#' @param parquet_filename Logical. (Parquet) If TRUE, include a `filename` column.
#' @param parquet_hive_partitioning Logical. (Parquet) If TRUE, interpret path as Hive partitioned.
#' @param parquet_union_by_name Logical. (Parquet) If TRUE, unify columns by name.
#' @param parquet_encryption_config List/Struct. (Parquet) Encryption configuration (advanced).
#' 
#' @param read_shp_mode Mode for reading Shapefiles. "ST_ReadSHP" (default, fast native reader) or "GDAL" (ST_Read).
#' @param read_osm_mode Mode for reading OSM PBF files. "GDAL" (default, ST_Read) or "ST_ReadOSM" (fast native reader, no geometry).
#' @param shp_encoding Encoding for Shapefiles when using "ST_ReadSHP" (e.g., "UTF-8", "ISO-8859-1").
#' 
#' @param gdal_spatial_filter Optional WKB geometry (as raw vector or hex string) to filter spatially (ST_Read only).
#' @param gdal_spatial_filter_box Optional bounding box (as numeric vector \code{c(minx, miny, maxx, maxy)}) 
#'   (ST_Read only).
#' @param gdal_keep_wkb Logical. If TRUE, return WKB blobs instead of GEOMETRY type (ST_Read only).
#' @param gdal_max_batch_size Integer. Maximum batch size for reading (ST_Read only).
#' @param gdal_sequential_layer_scan Logical. If TRUE, scan layers sequentially (ST_Read only).
#' @param gdal_sibling_files Character vector. List of sibling files (ST_Read only).
#' @param gdal_allowed_drivers Character vector. List of allowed GDAL drivers (ST_Read only).
#' @param gdal_open_options Character vector. Driver-specific open options (ST_Read only).
#'
#' @returns A \code{duckspatial_df} object.
#'
#' @export
ddbs_open_dataset <- function(path, 
                                   # Common
                                   crs = NULL, 
                                   layer = NULL,
                                   geom_col = NULL,
                                   conn = NULL,
                                   
                                   # Parquet Options
                                   parquet_binary_as_string = NULL,
                                   parquet_file_row_number = NULL,
                                   parquet_filename = NULL,
                                   parquet_hive_partitioning = NULL,
                                   parquet_union_by_name = NULL,
                                   parquet_encryption_config = NULL,
                                   
                                   # Dedicated Reader Modes
                                   read_shp_mode = c("ST_ReadSHP", "GDAL"),
                                   read_osm_mode = c("GDAL", "ST_ReadOSM"),
                                   shp_encoding = NULL,
                                   
                                   # GDAL / ST_Read Options
                                   gdal_spatial_filter = NULL,
                                   gdal_spatial_filter_box = NULL,
                                   gdal_keep_wkb = NULL,
                                   gdal_max_batch_size = NULL,
                                   gdal_sequential_layer_scan = NULL,
                                   gdal_sibling_files = NULL,
                                   gdal_allowed_drivers = NULL,
                                   gdal_open_options = NULL) {
  
  # Get or create connection
  if (is.null(conn)) {
    conn <- ddbs_default_conn()
  }
  
  # Ensure spatial extension
  ddbs_install(conn, quiet = TRUE)
  ddbs_load(conn, quiet = TRUE)
  
  # Determine format
  fmt <- get_file_format(path)
  
  # Resolve modes
  read_shp_mode <- match.arg(read_shp_mode)
  read_osm_mode <- match.arg(read_osm_mode)
  
  # Check for dedicated readers dispatch
  is_dedicated_shp <- (fmt == "shp" && read_shp_mode == "ST_ReadSHP")
  is_dedicated_osm <- (fmt == "osm.pbf" || (grepl("\\.osm\\.pbf$", path) && read_osm_mode == "ST_ReadOSM"))
  
  # Validate driver availability ONLY if NOT using a dedicated reader (and not Parquet)
  if (fmt != "parquet" && !is_dedicated_shp && !is_dedicated_osm) {
    validate_driver_availability(path, conn)
  }
  
  # -- CRS DETECTION --
  if (is.null(crs)) {
    if (fmt == "parquet") {
        crs <- get_parquet_crs(path, conn) 
    } else {
        crs <- get_file_crs(path, conn)
    }
  } else if (!inherits(crs, "crs")) {
      crs <- sf::st_crs(crs)
  }

  view_name <- ddbs_temp_view_name()
  
  # -- QUERY CONSTRUCTION --
  
  # Helper to format args for SQL
  fmt_arg <- function(x, quote = TRUE) {
     if (is.null(x)) return("NULL")
     if (is.character(x) && quote) return(paste0("'", x, "'"))
     if (is.logical(x)) return(ifelse(x, "TRUE", "FALSE"))
     if (is.numeric(x)) return(as.character(x))
     return("NULL") 
  }
  
  fmt_list_arg <- function(x) {
     if (is.null(x)) return("NULL")
     if (length(x) == 0) return("[]")
     items <- paste0("'", x, "'", collapse = ", ")
     paste0("[", items, "]")
  }
  
  if (fmt == "parquet") {
      # WARN on mismatched arguments
      if (!is.null(layer) || !is.null(gdal_spatial_filter) || !is.null(gdal_spatial_filter_box) ||
          !is.null(gdal_max_batch_size) || !is.null(gdal_sequential_layer_scan) || 
          !is.null(gdal_sibling_files) || !is.null(gdal_allowed_drivers) || !is.null(gdal_open_options)) {
          cli::cli_warn("Arguments specific to ST_Read (gdal_*) and 'layer' are ignored for Parquet files.")
      }
      
      # Build read_parquet args
      p_args <- c()
      if (!is.null(parquet_binary_as_string)) p_args <- c(p_args, glue::glue("binary_as_string := {fmt_arg(parquet_binary_as_string)}"))
      if (!is.null(parquet_file_row_number)) p_args <- c(p_args, glue::glue("file_row_number := {fmt_arg(parquet_file_row_number)}"))
      if (!is.null(parquet_filename)) p_args <- c(p_args, glue::glue("filename := {fmt_arg(parquet_filename)}"))
      if (!is.null(parquet_hive_partitioning)) p_args <- c(p_args, glue::glue("hive_partitioning := {fmt_arg(parquet_hive_partitioning)}"))
      if (!is.null(parquet_union_by_name)) p_args <- c(p_args, glue::glue("union_by_name := {fmt_arg(parquet_union_by_name)}"))
      
      # Todo: encryption_config defaults? Using NULL for now. 
      
      p_args_str <- ""
      if (length(p_args) > 0) {
          p_args_str <- paste0(", ", paste(p_args, collapse = ", "))
      }
      
      scan_query <- glue::glue("read_parquet('{path}'{p_args_str})")
      view_query <- glue::glue("CREATE OR REPLACE TEMPORARY VIEW {view_name} AS SELECT * FROM {scan_query}")
      
      # Resolve geometry column
      if (is.null(geom_col)) {
         try_cols <- tryCatch({
             DBI::dbGetQuery(conn, glue::glue("DESCRIBE {scan_query}"))
         }, error = function(e) NULL)
         
         if (!is.null(try_cols)) {
             possibles <- c("geometry", "geom", "wkb_geometry")
             found <- try_cols$column_name[try_cols$column_name %in% possibles]
             if (length(found) > 0) geom_col <- found[1]
             else geom_col <- "geometry"
         } else {
             geom_col <- "geometry"
         }
      }
      
  } else if (is_dedicated_shp) {
      # Dedicated ST_ReadSHP path
      if (!is.null(shp_encoding)) {
          # ST_ReadSHP('file', encoding := 'encoding')
          view_query <- glue::glue("CREATE OR REPLACE TEMPORARY VIEW {view_name} AS SELECT * FROM ST_ReadSHP('{path}', encoding := '{shp_encoding}')")
      } else {
          view_query <- glue::glue("CREATE OR REPLACE TEMPORARY VIEW {view_name} AS SELECT * FROM ST_ReadSHP('{path}')")
      }
      geom_col <- "geom" # Standard for ST_ReadSHP

  } else if (is_dedicated_osm) {
       # Dedicated ST_ReadOSM path
       # Returns raw data, NO geometry column usually
       view_query <- glue::glue("CREATE OR REPLACE TEMPORARY VIEW {view_name} AS SELECT * FROM ST_ReadOSM('{path}')")
       geom_col <- NA_character_ # Signal no geometry

  } else {
      # Standard ST_Read (GDAL)
      # WARN on mismatched arguments
      if (!is.null(parquet_binary_as_string) || !is.null(parquet_file_row_number) || 
          !is.null(parquet_filename) || !is.null(parquet_hive_partitioning) || 
          !is.null(parquet_union_by_name) || !is.null(parquet_encryption_config)) {
          cli::cli_warn("Arguments specific to Parquet (parquet_*) are ignored for this format.")
      }
      
      # Warn if shp_encoding is passed but we are not in ST_ReadSHP mode
      if (!is.null(shp_encoding)) {
          cli::cli_warn("Argument `shp_encoding` is ignored when `read_shp_mode` is not 'ST_ReadSHP'.")
      }

      # ST_Read ARGS
      sp_filter_sql <- "NULL" 
      if (!is.null(gdal_spatial_filter)) {
          if (is.raw(gdal_spatial_filter)) {
              hex <- paste0(as.character(gdal_spatial_filter), collapse = "")
              sp_filter_sql <- paste0("x'", hex, "'")
          } else if (is.character(gdal_spatial_filter)) {
               sp_filter_sql <- paste0("x'", gdal_spatial_filter, "'")
          }
      }
      
      sp_box_sql <- "NULL"
      if (!is.null(gdal_spatial_filter_box)) {
          if (is.numeric(gdal_spatial_filter_box) && length(gdal_spatial_filter_box) == 4) {
              sp_box_sql <- glue::glue("ROW({gdal_spatial_filter_box[1]}, {gdal_spatial_filter_box[2]}, {gdal_spatial_filter_box[3]}, {gdal_spatial_filter_box[4]})::BOX_2D")
          }
      }
      
      args_list <- c()
      if (!is.null(gdal_keep_wkb)) args_list <- c(args_list, glue::glue("keep_wkb := {fmt_arg(gdal_keep_wkb)}"))
      if (!is.null(gdal_max_batch_size)) args_list <- c(args_list, glue::glue("max_batch_size := {fmt_arg(gdal_max_batch_size)}"))
      if (!is.null(gdal_sequential_layer_scan)) args_list <- c(args_list, glue::glue("sequential_layer_scan := {fmt_arg(gdal_sequential_layer_scan)}"))
      if (!is.null(layer)) args_list <- c(args_list, glue::glue("layer := {fmt_arg(layer)}"))
      if (!is.null(gdal_sibling_files)) args_list <- c(args_list, glue::glue("sibling_files := {fmt_list_arg(gdal_sibling_files)}"))
      
      if (sp_filter_sql != "NULL") args_list <- c(args_list, glue::glue("spatial_filter := {sp_filter_sql}"))
      if (sp_box_sql != "NULL") args_list <- c(args_list, glue::glue("spatial_filter_box := {sp_box_sql}"))
      
      if (!is.null(gdal_allowed_drivers)) args_list <- c(args_list, glue::glue("allowed_drivers := {fmt_list_arg(gdal_allowed_drivers)}"))
      if (!is.null(gdal_open_options)) args_list <- c(args_list, glue::glue("open_options := {fmt_list_arg(gdal_open_options)}"))
      
      args_str <- ""
      if (length(args_list) > 0) {
          args_str <- paste0(", ", paste(args_list, collapse = ", "))
      }
      
      query_str <- glue::glue("ST_Read('{path}'{args_str})")
      
      if (is.null(geom_col)) {
         geom_col <- "geom" 
         try_cols <- tryCatch({
             DBI::dbGetQuery(conn, glue::glue("DESCRIBE SELECT * FROM {query_str}"))
         }, error = function(e) NULL)
         
         if (!is.null(try_cols)) {
             ctype <- if("column_type" %in% names(try_cols)) try_cols$column_type else try_cols$data_type
             # Look for standard geometry types
             geom_cols <- try_cols$column_name[grepl("GEOMETRY|WKB_BLOB", ctype, ignore.case = TRUE)]
             if (length(geom_cols) > 0) geom_col <- geom_cols[1]
         }
      }

      view_query <- glue::glue("CREATE OR REPLACE TEMPORARY VIEW {view_name} AS SELECT * FROM {query_str}")
  }

  DBI::dbExecute(conn, view_query)
  
  duck_tbl <- dplyr::tbl(conn, view_name)
  new_duckspatial_df(duck_tbl, crs = crs, geom_col = geom_col, source_table = view_name)
}

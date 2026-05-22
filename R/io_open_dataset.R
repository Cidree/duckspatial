#' Open spatial dataset lazily via DuckDB
#'
#' Reads spatial data directly from disk using DuckDB's spatial extension or 
#' native Parquet reader, returning a \code{duckspatial_df} object for lazy processing.
#'
#' @param path Path to spatial file. Supports Parquet (`.parquet`, with optional GeoParquet metadata),
#'   GeoJSON, GeoPackage, Shapefile, FlatGeoBuf, OSM PBF, DuckDB database files
#'   (`.duckdb`, `.db`, and `.ddb`), and other GDAL-supported formats.
#' @param crs Coordinate reference system. Can be an EPSG code (e.g., 4326),
#'   a CRS string, or an \code{sf} crs object. If \code{NULL} (default),
#'   attempts to auto-detect from the file, including native DuckDB CRS metadata
#'   and duckspatial-managed column-comment metadata for compatibility
#'   `.duckdb` files.
#' @param layer Layer name or index to read (ST_Read). For DuckDB database
#'   files, this is required and specifies the table name to read. Default is
#'   NULL (first layer for ST_Read).
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
#' @references 
#' This function is inspired by the dataset opening logic in the 
#' \code{duckdbfs} package (\url{https://github.com/cboettig/duckdbfs}).
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
  
  ddbs_assert_duckdb_crs_support()

  # Capture the call for error reporting
  fn_call <- rlang::current_call()
  crs_override <- crs
  
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
  is_dedicated_osm <- (fmt == "osm.pbf" || grepl("\\.osm\\.pbf$", path)) && read_osm_mode == "ST_ReadOSM"
  
  # Support opening DuckDB files natively for explicit DuckDB file extensions.
  # Magic bytes are still the source of truth for whether a candidate is valid.
  duckdb_ext <- has_duckdb_file_extension(path)
  duckdb_info <- duckdb_file_info(path)
  if (duckdb_ext) {
    if (!duckdb_info$exists) {
      cli::cli_abort("File {.file {path}} does not exist.")
    }

    if (!isTRUE(duckdb_info$valid)) {
      if (isTRUE(duckdb_info$empty) || tolower(tools::file_ext(path)) %in% c("duckdb", "ddb")) {
        cli::cli_abort(c(
          "File {.file {path}} is not a valid DuckDB database.",
          "x" = "The file is missing the DuckDB magic bytes."
        ))
      }
      # Non-DuckDB .db files are common in the wild. Let GDAL/ST_Read attempt
      # them instead of treating the extension alone as authoritative.
    } else {
      if (is.null(layer)) {
        cli::cli_abort(c(
          "You must specify the {.arg layer} (table name) when opening a DuckDB database file.",
          "i" = "Example: {.code ddbs_open_dataset('{path}', layer = 'my_table')}"
        ))
      }

      local_conn <- withCallingHandlers(
        ddbs_create_conn(path),
        duckspatial_storage_mismatch = function(w) {
          invokeRestart("muffleWarning")
        }
      )

      result <- tryCatch({
        name_list <- get_query_name(layer)
        if (!table_exists(local_conn, name_list$table_name, name_list$schema_name)) {
          cli::cli_abort("Table {.val {layer}} is not present in DuckDB database {.file {path}}.")
        }
        as_duckspatial_df(layer, conn = local_conn, crs = crs_override, geom_col = geom_col)
      }, error = function(e) {
        ddbs_stop_conn(local_conn)
        stop(e)
      })

      return(result)
    }
  }
    
  # Helper for temporary table construction
  # As for duckdb v1.5 we cannot work with views as the geometry type is not recognized
  # and we need to alter it (column modification is not allowed in views)
  create_temp_table <- function(name, from) {
      glue::glue("
            CREATE OR REPLACE TEMPORARY VIEW {name} AS 
            SELECT * FROM {from};
        ")
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
  
  # Strategy for "unknown" format (auto-detection):
  # 1. Attempt Native Parquet first (efficient, but extension-less parquet fails ST_Read)
  # 2. Fallback to ST_Read (generic GDAL)
  
  force_parquet <- (fmt == "parquet")
  is_likely_parquet <- force_parquet
  
  if (fmt == "unknown") {
      # Probing: check if it's parquet
      # We check by trying to read the first row with read_parquet
      # We use tryCatch to detect failure (bad magic bytes)
      is_parquet_check <- tryCatch({
          DBI::dbGetQuery(conn, glue::glue("SELECT 1 FROM read_parquet('{path}') LIMIT 1"))
          TRUE
      }, error = function(e) FALSE)
      
      if (is_parquet_check) {
          is_likely_parquet <- TRUE
      }
  }

  if (is_likely_parquet) {
      # WARN on mismatched arguments if user provided GDAL specifics
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
      
      p_args_str <- ""
      if (length(p_args) > 0) {
          p_args_str <- paste0(", ", paste(p_args, collapse = ", "))
      }
      
      scan_query <- glue::glue("read_parquet('{path}'{p_args_str})")
      
      # Resolve geometry column
      try_cols <- tryCatch({
          DBI::dbGetQuery(conn, glue::glue("DESCRIBE SELECT * FROM {scan_query}"))
      }, error = function(e) NULL)

      if (is.null(geom_col)) {
         if (!is.null(try_cols)) {
             geom_col <- ddbs_describe_geometry_col(try_cols)
         } else {
             geom_col <- NULL
         }
      }

      # Intercept GeoArrow structs (native Arrow encoding) which DuckDB cannot parse
      if (!is.null(geom_col) && !is.null(try_cols)) {
          col_type <- try_cols$column_type[try_cols$column_name == geom_col]
          if (length(col_type) > 0 && grepl("STRUCT", toupper(col_type[1]))) {
              cli::cli_abort(c(
                  "The geometry column {.val {geom_col}} uses a native Arrow/GeoArrow struct encoding that DuckDB's spatial extension cannot parse here.",
                  "x" = "This file uses a GeoParquet/GeoArrow geometry encoding that {.pkg duckspatial} cannot open through DuckDB yet.",
                  "i" = "To work around this, rewrite the geometry column to WKB GeoParquet, for example using:",
                  " " = "  {.code duckspatial::ddbs_write_dataset(data, path)}",
                  " " = "  # OR if using geoarrow:",
                  " " = "  {.code data${geom_col} <- geoarrow::as_geoarrow_vctr(data${geom_col}, schema = geoarrow::geoarrow_wkb())}",
                  " " = "  {.code arrow::write_parquet(data, path)}"
              ))
          }
      }
      
      view_query <- create_temp_table(
        name = view_name,
        from = scan_query
      )
      
  } else if (is_dedicated_shp) {      
      geom_col <- "geom" # Standard for ST_ReadSHP
      # Dedicated ST_ReadSHP path
      if (!is.null(shp_encoding)) {
        view_query <- create_temp_table(
            name = view_name,
            from = glue::glue("ST_ReadSHP('{path}', encoding := '{shp_encoding}')")
        )
      } else {
        view_query <- create_temp_table(
            name = view_name,
            from = glue::glue("ST_ReadSHP('{path}')")
        )
      }

  } else if (is_dedicated_osm) {
       # Dedicated ST_ReadOSM path
        geom_col <- NA_character_ # Signal no geometry
        view_query <- create_temp_table(
            name = view_name,
            from = glue::glue("ST_ReadOSM('{path}')")
        )

  } else {
      # Standard ST_Read (GDAL)
      # WARN on mismatched arguments
      if (!is.null(parquet_binary_as_string) || !is.null(parquet_file_row_number) || 
          !is.null(parquet_filename) || !is.null(parquet_hive_partitioning) || 
          !is.null(parquet_union_by_name) || !is.null(parquet_encryption_config)) {
          cli::cli_warn("Arguments specific to Parquet (parquet_*) are ignored for this format.")
      }
      
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
         try_cols <- tryCatch({
             DBI::dbGetQuery(conn, glue::glue("DESCRIBE SELECT * FROM {query_str}"))
         }, error = function(e) {
             # Catch ST_Read errors gracefully and provide user-friendly message
             msg <- e$message
             
             # Check if it's a format recognition error
             if (grepl("not recognized as a supported file format", msg, ignore.case = TRUE) || 
                 grepl("No extension found", msg, ignore.case = TRUE) ||
                 grepl("Could not open", msg, ignore.case = TRUE)) {
                 
                 # Extract the GDAL error message if present
                 gdal_match <- regexpr("GDAL Error \\([0-9]+\\): .*", msg)
                 gdal_msg <- if (gdal_match > 0) {
                     regmatches(msg, gdal_match)
                 } else {
                     "File format not recognized"
                 }
                 
                 # Clean, user-friendly error with technical details preserved
                 cli::cli_abort(c(
                     "Unable to open file {.path {basename(path)}}",
                     "x" = "The file format could not be detected or is not supported.",
                     "i" = "Supported formats include: Parquet, GeoJSON, GeoPackage, Shapefile, FlatGeoBuf, and other GDAL formats.",
                     "i" = "GDAL error: {gdal_msg}"
                 ), call = fn_call)
             }
             
             # For other errors, just return NULL to continue
             NULL
         })
         
         if (!is.null(try_cols)) {
             geom_col <- ddbs_describe_geometry_col(try_cols)
         }
      }
      
      view_query <- create_temp_table(
        name = view_name,
        from = query_str
      )

  }

  # -- EXECUTE VIEW CREATION --
  tryCatch({
    # We use dbExecute here to ensure the view is created successfully.
    # This is where most file-opening and format errors will surface.
    DBI::dbExecute(conn, view_query)
  }, error = function(e) {
    msg <- e$message
    
    # 1. Format recognition error (DuckDB or GDAL)
    if (grepl("not recognized as a supported file format|No extension found", msg, ignore.case = TRUE)) {
        # Extract the GDAL error message if present
        gdal_match <- regexpr("GDAL Error \\([0-9]+\\): .*", msg)
        gdal_msg <- if (gdal_match > 0) regmatches(msg, gdal_match) else "File format not recognized"
        
        cli::cli_abort(c(
            "Unable to open file {.path {basename(path)}}",
            "x" = "The file format could not be detected or is not supported.",
            "i" = "Supported formats include: Parquet, GeoJSON, GeoPackage, Shapefile, FlatGeoBuf, and other GDAL formats.",
            "i" = "GDAL error: {gdal_msg}"
        ), call = fn_call)
    }
    
    # 2. File not found error (IO Error)
    if (grepl("Cannot open file|No such file|IO Error.*No such file", msg, ignore.case = TRUE)) {
        cli::cli_abort(c(
            "Unable to open file {.path {basename(path)}}",
            "x" = "The file does not exist or cannot be accessed.",
            "i" = "Please check if the file path is correct and the file is not moved or deleted."
        ), call = fn_call)
    }
    
    # 3. Generic error fallback
    cli::cli_abort(c(
        "Failed to open dataset at {.path {path}}",
        "x" = msg
    ), call = fn_call)
  })
    
    # Get lazy table reference
    duck_tbl <- dplyr::tbl(conn, view_name)
    
    view_cols <- tryCatch(
      DBI::dbGetQuery(conn, glue::glue("DESCRIBE {view_name}")),
      error = function(e) NULL
    )
    
    # Resolve geometry column: uses user-supplied name if valid, 
    # otherwise falls back to auto-detection heuristic.
    geom_col <- ddbs_describe_geometry_col(view_cols, geom_col)

    # Return already if there's no geometry
    if (is.null(geom_col)) return(duck_tbl)

    crs <- ddbs_open_dataset_crs(
      crs = crs_override,
      conn = conn,
      view_name = view_name,
      geom_col = geom_col,
      path = path,
      fmt = if (is_likely_parquet) "parquet" else fmt
    )
    
    # Return duckspatial if there's geometry col
    result <- new_duckspatial_df(
      duck_tbl, 
      crs = crs, 
      geom_col = geom_col, 
      source_table = view_name,
      source_conn = conn
    )    

    return(result)
}

#' Detect a geometry column from DuckDB DESCRIBE output
#'
#' @keywords internal
#' @noRd
ddbs_describe_geometry_col <- function(desc, geom_col = NULL) {
  if (is.null(desc) || nrow(desc) == 0) {
    return(NULL)
  }
  
  col_type <- if ("column_type" %in% names(desc)) desc$column_type else desc$data_type
  
  is_geometry_type <- grepl("^GEOMETRY(\\(|$)", col_type, ignore.case = TRUE)
  is_wkb_type <- grepl("WKB_BLOB", col_type, ignore.case = TRUE)
  is_blob_type <- grepl("^BLOB$", col_type, ignore.case = TRUE)
  is_struct_type <- grepl("^STRUCT", col_type, ignore.case = TRUE)
  is_spatial_type <- is_geometry_type | is_wkb_type | is_blob_type | is_struct_type

  # 1. If user supplied a geom_col, validate it
  if (!is.null(geom_col) && !is.na(geom_col)) {
    match_idx <- which(desc$column_name == geom_col)
    if (length(match_idx) > 0 && is_spatial_type[match_idx[1]]) {
      return(geom_col)
    }
  }

  # 2. Auto-detection heuristics
  
  # 2.1. First priority: Native GEOMETRY types
  geom_cols <- desc$column_name[is_geometry_type]
  if (length(geom_cols) > 0) {
    return(geom_cols[1])
  }

  # 2.2. Second priority: Spatial column names with compatible types (WKB/BLOB/STRUCT)
  known <- c("geom", "geometry", "wkb_geometry")
  found <- desc$column_name[desc$column_name %in% known & is_spatial_type]
  if (length(found) > 0) {
    return(found[1])
  }

  # 2.3. Third priority: Known spatial formats fallback (WKB_BLOB)
  # Do not treat arbitrary BLOB columns as geometry unless they were 
  # explicitly selected by name above or exposed as native GEOMETRY.
  wkb_cols <- desc$column_name[is_wkb_type]
  if (length(wkb_cols) > 0) {
    return(wkb_cols[1])
  }

  NULL
}

#' Resolve CRS for an opened DuckDB-backed dataset
#'
#' @keywords internal
#' @noRd
ddbs_open_dataset_crs <- function(crs, conn, view_name, geom_col, path, fmt) {
  if (!is.null(crs)) {
    return(if (inherits(crs, "crs")) crs else sf::st_crs(crs))
  }

  crs_from_duckdb <- tryCatch({
    q_geom <- DBI::dbQuoteIdentifier(conn, geom_col)
    res <- DBI::dbGetQuery(conn, glue::glue(
      "SELECT ST_CRS({q_geom}) AS crs ",
      "FROM {view_name} ",
      "WHERE {q_geom} IS NOT NULL ",
      "LIMIT 1"
    ))

    if (nrow(res) == 0 || is.na(res$crs[1]) || identical(res$crs[1], "")) {
      NULL
    } else {
      sf::st_crs(res$crs[1])
    }
  }, error = function(e) NULL)

  if (!is.null(crs_from_duckdb) && !is.na(crs_from_duckdb)) {
    return(crs_from_duckdb)
  }

  if (!identical(fmt, "parquet")) {
    crs_from_gdal_meta <- withCallingHandlers(
      tryCatch(get_file_crs(path, conn), error = function(e) NULL),
      warning = function(w) {
        if (grepl("Cannot open|No such file|not recognized|missing value where", w$message, ignore.case = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    )

    if (!is.null(crs_from_gdal_meta) && !is.na(crs_from_gdal_meta)) {
      return(crs_from_gdal_meta)
    }
  }

  if (identical(fmt, "parquet")) {
    crs_from_geoparquet <- suppressWarnings(
      tryCatch(get_parquet_crs(path, conn), error = function(e) NULL)
    )

    if (!is.null(crs_from_geoparquet) && !is.na(crs_from_geoparquet)) {
      return(crs_from_geoparquet)
    }
  }

  warning("CRS could not be auto-detected; returning NA CRS.", call. = FALSE)
  sf::st_crs(NA)
}

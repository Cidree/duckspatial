#' Write spatial dataset to disk
#'
#' Writes spatial data to disk using DuckDB's `COPY` command. Supports Parquet (native) 
#' and various GDAL spatial formats. Format is auto-detected from file extension for 
#' common formats, or can be specified explicitly via `gdal_driver`.
#'
#' @param data A `duckspatial_df`, `tbl_lazy` (DuckDB), or `sf` object.
#' @param path Path to output file.
#' @param gdal_driver GDAL driver name for writing spatial formats. If `NULL` (default),
#'   the driver is auto-detected from the file extension for common formats:
#'   - `.geojson`, `.json` → "GeoJSON"
#'   - `.shp` → "ESRI Shapefile"  
#'   - `.gpkg` → "GPKG"
#'   - `.fgb` → "FlatGeobuf"
#'   - `.kml` → "KML"
#'   - `.gpx` → "GPX"
#'   - `.gml` → "GML"
#'   - `.sqlite` → "SQLite"
#'   
#'   For **non-standard file extensions** (e.g., `.dat`, `.xyz`) or to **explicitly override** 
#'   auto-detection, specify the exact driver name as it appears in `ddbs_drivers()$short_name`. 
#'   Examples: `gdal_driver = "GeoJSON"`, `gdal_driver = "ESRI Shapefile"`.
#'   
#'   **Note**: If you specify a driver that doesn't match the file extension (e.g., 
#'   `path = "output.shp"` with `gdal_driver = "GeoJSON"`), a warning will be issued but 
#'   your explicit driver choice will be honored (creating a GeoJSON file with `.shp` extension).
#'   
#'   The function validates that the specified driver is available and writable on your 
#'   system. Note: `.parquet` and `.csv` files use native DuckDB writers and do not 
#'   require a GDAL driver.
#' @template conn_null
#' @param overwrite Logical. If `TRUE`, overwrites existing file.
#' @param crs Output CRS (e.g., "EPSG:4326"). Passed to GDAL as `SRS` option. Ignored for Parquet.
#' @param options Named list of additional options passed to `COPY`.
#' @param partitioning Character vector of columns to partition by (Parquet/CSV only).
#' @param parquet_compression Compression codec for Parquet.
#' @param parquet_row_group_size Row group size for Parquet.
#' @param layer_creation_options GDAL layer creation options.
#' @template quiet
#'
#' @return The `path` invisibly.
#' 
#' @seealso [ddbs_drivers()] to list all available GDAL drivers and formats.
#' 
#' @references 
#' This function is inspired by and builds upon the logic found in the 
#' \code{duckdbfs} package (\url{https://github.com/cboettig/duckdbfs}), 
#' particularly its \code{write_dataset} and \code{write_geo} functions.
#' For advanced features like cloud storage (S3) support, the 
#' \code{duckdbfs} package is highly recommended.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#' 
#' # Read example data
#' path <- system.file("spatial/countries.geojson", package = "duckspatial")
#' ds <- ddbs_open_dataset(path)
#' 
#' # Auto-detect format from extension
#' ddbs_write_dataset(ds, "output.geojson")
#' ddbs_write_dataset(ds, "output.gpkg")
#' ddbs_write_dataset(ds, "output.parquet")
#' 
#' # Explicit GDAL driver for non-standard extension
#' ddbs_write_dataset(ds, "mydata.dat", gdal_driver = "GeoJSON")
#' 
#' # See available drivers on your system
#' drivers <- ddbs_drivers()
#' writable <- drivers[drivers$can_create == TRUE, ]
#' head(writable)
#' 
#' # CRS override
#' ddbs_write_dataset(ds, "output_3857.geojson", crs = "EPSG:3857")
#' 
#' # Overwrite existing file
#' ddbs_write_dataset(ds, "output.gpkg", overwrite = TRUE)
#' }
ddbs_write_dataset <- function(
    data,
    path,
    gdal_driver = NULL,
    conn = NULL,
    overwrite = FALSE,
    crs = NULL,
    options = list(),
    partitioning = if (inherits(data, c("tbl_lazy", "duckspatial_df"))) dplyr::group_vars(data) else NULL,
    parquet_compression = NULL,
    parquet_row_group_size = NULL,
    layer_creation_options = NULL,
    quiet = FALSE
) {
  
  # 1. Resolve connection
  if (is.null(conn)) {
    conn <- extract_connection(data)
    if (is.null(conn)) conn <- ddbs_default_conn()
  }
  
  # 2. Ensure DuckDB spatial extension
  ddbs_install(conn, quiet = TRUE)
  ddbs_load(conn, quiet = TRUE)
  
  # 3. Format detection and driver resolution
  ext <- tolower(tools::file_ext(path))
  
  # Determine if native format
  is_parquet <- ext == "parquet"
  is_csv <- ext == "csv"
  is_native <- is_parquet || is_csv
  
  # For GDAL formats, resolve driver
  driver_name <- NULL
  if (!is_native) {
    # Step 1: Try auto-detection from extension
    driver_from_ext <- get_driver_from_extension(ext)
    
    # Step 2: Use explicit driver if provided
    if (!is.null(gdal_driver)) {
      driver_name <- gdal_driver
      
      # Warn if driver and extension mismatch
      if (!is.null(driver_from_ext) && driver_from_ext != driver_name) {
        expected_ext <- get_extension_for_driver(driver_name)
        if (!is.null(expected_ext)) {
          cli::cli_warn(c(
            "Extension/driver mismatch detected.",
            "i" = "File extension {.val .{ext}} typically maps to driver {.val {driver_from_ext}}.",
            "i" = "You specified driver {.val {driver_name}}, which typically uses {.val .{expected_ext}}.",
            "i" = "Proceeding with your explicit driver choice."
          ))
        }
      }
    } else if (!is.null(driver_from_ext)) {
      # Use auto-detected driver
      driver_name <- driver_from_ext
    } else {
      # Unknown extension, no driver provided
      cli::cli_abort(c(
        "Cannot determine GDAL driver for extension {.val .{ext}}.",
        "i" = "Please specify the driver explicitly using {.arg gdal_driver}:",
        " " = "  ddbs_write_dataset(data, path, gdal_driver = \"GeoJSON\")",
        "i" = "Run {.code ddbs_drivers()} to see all available drivers."
      ))
    }
    
    # Step 3: Validate driver exists and is writable
    available_drivers <- tryCatch({
      ddbs_drivers(conn)
    }, error = function(e) {
      cli::cli_warn("Could not query available GDAL drivers. Proceeding without validation.")
      NULL
    })
    
    if (!is.null(available_drivers)) {
      # Check if driver exists
      driver_info <- available_drivers[available_drivers$short_name == driver_name, ]
      
      if (nrow(driver_info) == 0) {
        # Driver not found - provide helpful error
        writable_drivers <- available_drivers[available_drivers$can_create == TRUE, ]
        cli::cli_abort(c(
          "GDAL driver {.val {driver_name}} is not available on this system.",
          "i" = "Run {.code ddbs_drivers()} to see all available drivers.",
          "i" = "Available writable drivers include:",
          " " = paste("  -", head(writable_drivers$short_name, 10), collapse = "\n")
        ))
      } else if (!driver_info$can_create[1]) {
        # Driver exists but is read-only
        writable_drivers <- available_drivers[available_drivers$can_create == TRUE, ]
        cli::cli_abort(c(
          "GDAL driver {.val {driver_name}} is read-only and cannot be used for writing.",
          "i" = "Run {.code ddbs_drivers()} and check the {.field can_create} column.",
          "i" = "Available writable drivers include:",
          " " = paste("  -", head(writable_drivers$short_name, 10), collapse = "\n")
        ))
      }
    }
  }
  
  # 4. Check for existing file
  if (file.exists(path)) {
    if (!overwrite) {
      cli::cli_abort("File {.path {path}} already exists. Use {.arg overwrite = TRUE} to replace.")
    } else {
      # For GDAL formats, explicit delete is safer/more consistent
      if (!is_native) {
        unlink(path)
      }
      # For native (Parquet/CSV), we can use OVERWRITE_OR_IGNORE option below
    }
  }

  # 4b. Auto-detect CRS if not provided
  # For GDAL formats, it's crucial to pass SRS if possible so output has projection.
  # For Parquet, DuckDB might not automatically write CRS metadata unless enabled/configured,
  # but passing SRS usually doesn't hurt or is ignored by native COPY (check docs?). 
  # Actually native Parquet COPY doesn't take SRS. We only pass checks for GDAL.
  
  if (is.null(crs) && !is_native) {
      # Try to detect from data
      obj_crs <- tryCatch(ddbs_crs(data, conn = conn), error = function(e) NULL)
      if (!is.null(obj_crs) && !is.na(obj_crs)) {
          # Prefer Authority Code (EPSG:xxxx) if available, else WKT
          # sf crs object
          epsg <- obj_crs$epsg
          if (!is.null(epsg) && !is.na(epsg)) {
              crs <- paste0("EPSG:", epsg)
          } else {
              crs <- obj_crs$wkt
          }
      }
  }
  
  # 5. Logic to generate the SQL source
  # We want the 'source' to be either a table name or (subquery)
  view_name <- NULL
  sql_source <- NULL
  
  if (inherits(data, c("duckspatial_df", "tbl_lazy", "tbl_duckdb_connection"))) {
    # Efficient table name extraction or subquery construction
    r_name <- dbplyr::remote_name(data)
    is_simple_table <- !is.null(r_name) && is.character(r_name)
    
    if (is_simple_table) {
       sql_source <- r_name
    } else {
       sql_source <- paste0("(", dbplyr::sql_render(data, con = conn), ")")
    }
    
    # Strict Remote Validation: introspect types using DESCRIBE
    # We must ensure there is a GEOMETRY column.
    dtypes <- tryCatch({
        DBI::dbGetQuery(conn, glue::glue("DESCRIBE SELECT * FROM {sql_source} LIMIT 0"))
    }, error = function(e) {
        # Fallback if describe fails (complex queries?), though usually works
        NULL
    })
    
    has_geometry_type <- FALSE
    if (!is.null(dtypes) && "column_type" %in% names(dtypes)) {
        has_geometry_type <- any(dtypes$column_type == "GEOMETRY")
    }
    
    
    if (!has_geometry_type) {
         # Double check if maybe it's named 'geometry' but not typed yet (rare in valid spatial tables)
         # Using DuckDB spatial, it should be GEOMETRY.
         cli::cli_abort("Input DuckDB table/query does not contain a spatial column of type 'GEOMETRY'.")
    }

    # Sanitize column names for specific drivers (e.g. FID in GeoPackage)
    if (!is.null(driver_name) && driver_name == "GPKG" && !is.null(dtypes)) {
         cols_in_table <- dtypes$column_name
         if (any(toupper(cols_in_table) == "FID")) {
             q_cols_list <- lapply(cols_in_table, function(col) {
                 q <- DBI::dbQuoteIdentifier(conn, col)
                 if (toupper(col) == "FID") return(paste0(q, " AS FID_original"))
                 return(q)
             })
             cols_sql <- paste(unlist(q_cols_list), collapse = ", ")
             sql_source <- glue::glue("(SELECT {cols_sql} FROM {sql_source})")
             cli::cli_alert_warning("Column 'FID' renamed to 'FID_original' to avoid conflict with GeoPackage primary key.")
         }
    }
    
  } else if (inherits(data, c("sf", "data.frame"))) {
    
    # Strict Local Validation: Must be 'sf'
    if (!inherits(data, "sf")) {
       cli::cli_abort("Input local data must be an 'sf' object. Plain data.frames are not supported.")
    }
    
    view_name <- ddbs_temp_view_name()
    
    tryCatch({
       # Ensure view doesn't exist (robust check)
       tryCatch(duckdb::duckdb_unregister(conn, view_name), error = function(e) NULL)
       DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {view_name}"))
       
       if (inherits(data, "sf")) {
           # Explicitly convert to WKB to ensure consistent DuckDB typing (BLOB)
           wkb_col <- attr(data, "sf_column")
           data[[wkb_col]] <- sf::st_as_binary(data[[wkb_col]])
           
           # Register the WKB-fied data frame
           duckdb::duckdb_register(conn, view_name, data)
           
           # Cast Blob geometry to GEOMETRY type for spatial formats
           gcol <- attr(data, "sf_column")
           col_names <- setdiff(names(data), gcol)
           
           # Use proper quoting
           q_gcol <- DBI::dbQuoteIdentifier(conn, gcol)
           
           if (length(col_names) > 0) {
               # Check for potentially conflicting columns (e.g. FID in GeoPackage)
               if (!is.null(driver_name) && driver_name == "GPKG" && any(toupper(col_names) == "FID")) {
                   # Rename conflicting FID column
                   q_cols_list <- lapply(col_names, function(col) {
                       q <- DBI::dbQuoteIdentifier(conn, col)
                       if (toupper(col) == "FID") return(paste0(q, " AS FID_original"))
                       return(q)
                   })
                   cols_sql <- paste(unlist(q_cols_list), collapse = ", ")
                   cli::cli_alert_warning("Column 'FID' renamed to 'FID_original' to avoid conflict with GeoPackage primary key.")
               } else {
                   q_cols <- DBI::dbQuoteIdentifier(conn, col_names)
                   cols_sql <- paste(q_cols, collapse = ", ")
               }
              subquery <- glue::glue("SELECT {cols_sql}, ST_GeomFromWKB({q_gcol}) AS {q_gcol} FROM {view_name}")
           } else {
              subquery <- glue::glue("SELECT ST_GeomFromWKB({q_gcol}) AS {q_gcol} FROM {view_name}")
           }
           sql_source <- paste0("(", subquery, ")")
           
       } else {
           duckdb::duckdb_register(conn, view_name, data)
           sql_source <- view_name
       }
       
    }, error = function(e) {
       cli::cli_abort("Failed to register local data frame: {e$message}")
    })
    
  } else {
    cli::cli_abort("Unsupported input type: {.cls {class(data)}}")
  }
  
  # 6. Build COPY options
  copy_ops <- list()
  
  # Format
  if (is_parquet) {
      copy_ops$FORMAT <- "PARQUET"
  } else if (is_csv) {
      copy_ops$FORMAT <- "CSV"
  } else {
      # GDAL
      copy_ops$FORMAT <- "GDAL"
      copy_ops$DRIVER <- driver_name
      
      # Default layer creation options for GDAL if not provided
      if (is.null(layer_creation_options)) {
          layer_creation_options <- "WRITE_BBOX=YES"
      }
      
      # Pass SRS if available
      if (!is.null(crs)) {
          copy_ops$SRS <- crs
      }
  }
  
  # Options: Overwrite (Native only)
  if (is_native && overwrite) {
      copy_ops$OVERWRITE_OR_IGNORE <- TRUE
  }
  
  # Options: Parquet Compression
  if (!is.null(parquet_compression)) {
      copy_ops$COMPRESSION <- parquet_compression
  }
  
  # Options: Row Group Size
  if (!is.null(parquet_row_group_size)) {
      copy_ops$ROW_GROUP_SIZE <- as.integer(parquet_row_group_size)
  }
  
  # Options: Partitioning (Native only)
  if (!is.null(partitioning) && length(partitioning) > 0) {
     p_cols <- paste(partitioning, collapse = ", ")
     copy_ops$PARTITION_BY <- paste0("(", p_cols, ")")
  }
  
  # Options: Layer Creation (GDAL only)
  if (!is.null(layer_creation_options)) {
      copy_ops$LAYER_CREATION_OPTIONS <- layer_creation_options
  }
  
  # Merge Generic Options
  if (length(options) > 0) {
      copy_ops <- utils::modifyList(copy_ops, options)
  }
  
  # 7. Execute COPY
  
  # Helper to format options
  fmt_opts <- function(ops) {
      parts <- vapply(names(ops), function(n) {
          val <- ops[[n]]
          if (is.logical(val)) {
              val_str <- ifelse(val, "TRUE", "FALSE")
          } else if (is.numeric(val)) {
              val_str <- as.character(val)
          } else {
              # String: handle single quotes escaping if needed, though rare in options
              # PARTITION_BY handled specially (starts with parens)
              if (grepl("^\\(", val)) {
                  val_str <- val 
              } else {
                  val_str <- paste0("'", val, "'")
              }
          }
          paste0(n, " ", val_str)
      }, character(1))
      paste(parts, collapse = ", ")
  }
  
  opt_str <- fmt_opts(copy_ops)
  query <- glue::glue("COPY {sql_source} TO '{path}' ({opt_str})")
  
  tryCatch({
      DBI::dbExecute(conn, query)
  }, finally = {
      # Cleanup temp view if we created one
      if (!is.null(view_name)) {
          duckdb::duckdb_unregister(conn, view_name)
      }
  })
  
  if (!quiet) {
      cli::cli_alert_success("Written to {.path {path}}")
  }
  
  invisible(path)
}

#' Helper to extract connection from object
#' @noRd
extract_connection <- function(x) {
    if (inherits(x, c("tbl_duckdb_connection", "duckspatial_df"))) {
        return(dbplyr::remote_con(x))
    }
    if (inherits(x, "tbl_lazy")) {
        # Check if src is duckdb
        con <- dbplyr::remote_con(x)
        if (inherits(con, "duckdb_connection")) return(con)
    }
    NULL
}

#' Map file extension to GDAL driver name
#' @noRd
get_driver_from_extension <- function(ext) {
  ext_map <- list(
    "geojson" = "GeoJSON",
    "json" = "GeoJSON",
    "shp" = "ESRI Shapefile",
    "gpkg" = "GPKG",
    "fgb" = "FlatGeobuf",
    "kml" = "KML",
    "gpx" = "GPX",
    "gml" = "GML",
    "sqlite" = "SQLite",
    "tab" = "MapInfo File",
    "mif" = "MapInfo File"
  )
  ext_map[[tolower(ext)]]
}

#' Get extension for a driver (inverse mapping)
#' @noRd
get_extension_for_driver <- function(driver) {
  driver_to_ext <- list(
    "GeoJSON" = "geojson",
    "ESRI Shapefile" = "shp",
    "GPKG" = "gpkg",
    "FlatGeobuf" = "fgb",
    "KML" = "kml",
    "GPX" = "gpx",
    "GML" = "gml",
    "SQLite" = "sqlite",
    "MapInfo File" = "tab"
  )
  driver_to_ext[[driver]]
}

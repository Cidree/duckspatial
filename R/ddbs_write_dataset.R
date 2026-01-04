#' Write spatial dataset to disk
#'
#' Writes a `duckspatial_df`, `tbl_lazy` (DuckDB), or `sf` object to a file 
#' using DuckDB's `COPY` command. Supports Parquet (native) and various spatial 
#' formats via GDAL (GeoJSON, GeoPackage, etc.).
#'
#' @param data A `duckspatial_df`, `tbl_lazy` (DuckDB), `sf` object, or `data.frame`.
#' @param path Path to output file.
#' @param format Output format. If `NULL` (default), inferred from file extension.
#'   Supported values include "parquet", "csv", "geojson", "gpkg", "shp", "fgb", etc.
#' @param conn DuckDB connection. If `NULL`, attempts to infer from `data` or uses default connection.
#' @param overwrite Logical. If `TRUE`, overwrites existing file.
#'   For Parquet/CSV, this uses DuckDB's `OVERWRITE_OR_IGNORE` flag.
#'   For GDAL formats, the file is explicitly deleted before writing to ensure consistent behavior.
#' @param crs Output Coordinate Reference System (e.g., "EPSG:4326"). 
#'   If provided, passed to GDAL as `SRS` option (overrides data CRS). 
#'   Ignored for Parquet.
#' @param options Named list of additional options passed to `COPY`.
#' @param partitioning Character vector of column names to partition by (Parquet/CSV only).
#'   If `NULL` (default), uses `dplyr::group_vars(data)` if available.
#' @param parquet_compression Compression codec for Parquet (e.g., "ZSTD", "SNAPPY", "GZIP", "UNCOMPRESSED").
#' @param parquet_row_group_size Row group size for Parquet (integer).
#' @param layer_creation_options GDAL layer creation options (e.g., "WRITE_BBOX=YES").
#' @param quiet Logical. if `TRUE`, suppresses success message.
#'
#' @return The `path` invisibly.
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
#' # Write to Parquet
#' tmp_parquet <- tempfile(fileext = ".parquet")
#' ddbs_write_dataset(ds, tmp_parquet)
#' 
#' # Write to GeoJSON with CRS override
#' tmp_json <- tempfile(fileext = ".geojson")
#' ddbs_write_dataset(ds, tmp_json, crs = "EPSG:3857")
#' 
#' # Write to GeoPackage with overwrite
#' tmp_gpkg <- tempfile(fileext = ".gpkg")
#' ddbs_write_dataset(ds, tmp_gpkg, overwrite = TRUE)
#' }
ddbs_write_dataset <- function(
    data,
    path,
    format = NULL,
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
  
  # 3. Format detection
  if (is.null(format)) {
    # Use extension directly for writing to preserve format details
    format <- tolower(tools::file_ext(path))
  }
  format <- tolower(format)
  
  # If generic "sf" given, revert to extension
  if (format == "sf") {
      format <- tolower(tools::file_ext(path))
  }
  
  # Resolve driver for GDAL formats
  driver_name <- get_driver_name(format)
  is_parquet  <- format == "parquet"
  is_csv      <- format == "csv"
  is_native   <- is_parquet || is_csv
  
  if (!is_native && is.null(driver_name)) {
     # Try to guess driver from extension if format didn't match
     ext <- tolower(tools::file_ext(path))
     driver_name <- get_driver_name(ext)
     
     if (is.null(driver_name)) {
         cli::cli_abort("Could not determine GDAL driver for format {.val {format}} or extension {.val {ext}}. Please specify format/driver explicitly.")
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
              q_cols <- DBI::dbQuoteIdentifier(conn, col_names)
              cols_sql <- paste(q_cols, collapse = ", ")
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

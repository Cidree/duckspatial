
## dbConnCheck

#' Check if a supported DuckDB connection
#'
#' @template conn
#'
#' @keywords internal
#' @returns TRUE (invisibly) for successful import
dbConnCheck <- function(conn) {  # nocov start
    if (inherits(conn, "duckdb_connection")) {
        return(invisible(TRUE))

    } else if (is.null(conn)) { return(invisible(FALSE))

    } else {
        cli::cli_abort("'conn' must be connection object: <duckdb_connection> from `duckdb`")
    }
}  # nocov end

#' Normalize spatial input for processing
#'
#' Standardizes all input types before spatial operations:
#' - sf objects: passed through unchanged
#' - duckspatial_df: passed through unchanged
#' - tbl_duckdb_connection: coerced to duckspatial_df (with source_table)
#' - character: verified to exist in connection
#'
#' @param x Spatial input (sf, duckspatial_df, tbl_duckdb_connection, or character)
#' @param conn DuckDB connection (required for character table names)
#' @return Normalized input ready for get_query_list()
#' @keywords internal
#' @noRd
prepare_spatial_input <- function(x, conn = NULL) {
  # 1. sf: pass through
  if (inherits(x, "sf")) return(x)
  
  # 2. duckspatial_df: already normalized
  if (inherits(x, "duckspatial_df")) return(x)
  
  # 3. tbl_duckdb_connection: coerce to duckspatial_df
  if (inherits(x, "tbl_duckdb_connection")) {
    return(as_duckspatial_df(x))
  }
  
  # 4. character: verify table/view exists
  if (is.character(x)) {
    if (is.null(conn)) {
      cli::cli_abort("{.arg conn} required when using character table names.")
    }
    if (!DBI::dbExistsTable(conn, x)) {
      cli::cli_abort("Table or view {.val {x}} does not exist in connection.")
    }
    return(x)
  }
  
  # Unsupported type - let downstream handle/error
  x
}

#' Get DuckDB connection from an object
#'
#' Extracts the connection from duckspatial_df, tbl_lazy, or validates a direct connection.
#'
#' @param x A duckspatial_df, tbl_lazy, duckdb_connection, or NULL
#' @return A duckdb_connection or NULL
#' @keywords internal
get_conn_from_input <- function(x) {  # nocov start
  if (is.null(x)) return(NULL)
  
  if (inherits(x, "duckdb_connection")) return(x)
  
  if (inherits(x, c("duckspatial_df", "tbl_lazy"))) {
    return(dbplyr::remote_con(x))
  }
  
  NULL
}  # nocov end

#' Compare two CRS objects for equality
#'
#' Properly compares CRS objects, handling different representations of the same CRS.
#'
#' @param crs1 First CRS object
#' @param crs2 Second CRS object
#' @return Logical indicating if CRS are equal
#' @keywords internal
crs_equal <- function(crs1, crs2) {  # nocov start
  # If either is NULL, skip check
  if (is.null(crs1) || is.null(crs2)) return(TRUE)
  
  # Normalize to sf crs objects
  crs1 <- sf::st_crs(crs1)
  crs2 <- sf::st_crs(crs2)
  
  # If both are NA, they're equal
  if (is.na(crs1) && is.na(crs2)) return(TRUE)
  
  # If one is NA and other isn't, they're different
  if (is.na(crs1) || is.na(crs2)) return(FALSE)
  
  # Compare EPSG codes if both have them
  if (!is.na(crs1$epsg) && !is.na(crs2$epsg)) {
    return(crs1$epsg == crs2$epsg)
  }
  
  # Fallback to WKT comparison
  identical(crs1$wkt, crs2$wkt)
}  # nocov end

#' Import a view/table from one connection to another
#'
#' Enables cross-connection operations by importing views using one of three strategies.
#'
#' @param target_conn Target DuckDB connection
#' @param source_conn Source DuckDB connection  
#' @param source_object duckspatial_df, tbl_lazy, or tbl_duckdb_connection from source_conn
#' @param target_name Name for view in target connection (auto-generated if NULL)
#' @return List with imported view name and cleanup function
#' @keywords internal
import_view_to_connection <- function(target_conn, source_conn, source_object, target_name = NULL) {
  
  if (is.null(target_name)) {
    target_name <- paste0("imported_", gsub("-", "_", uuid::UUIDgenerate()))
  }
  
  # Track cleanup actions
  cleanup_actions <- list()
  
  # STRATEGY 1: SQL recreation (same DB, direct view reference)
  if (inherits(source_object, c("duckspatial_df", "tbl_duckdb_connection", "tbl_lazy"))) {
    source_table <- dbplyr::remote_name(source_object)
    
    if (!is.null(source_table) && !inherits(source_table, "sql")) {
      source_table_clean <- gsub('^"|"$', "", as.character(source_table))
      
      view_sql <- tryCatch({
        q_sql <- glue::glue(
          "SELECT sql FROM duckdb_views() WHERE view_name = {DBI::dbQuoteString(source_conn, source_table_clean)}"
        )
        result <- DBI::dbGetQuery(source_conn, q_sql)
        if (nrow(result) > 0) result$sql else NULL
      }, error = function(e) NULL)
      
      if (!is.null(view_sql) && length(view_sql) > 0) {
        source_pat_clean <- paste0("VIEW\\s+\"", source_table_clean, "\"")
        source_pat <- paste0("VIEW\\s+", source_table)
        
        if (grepl(source_pat_clean, view_sql, ignore.case = TRUE)) {
          new_sql <- sub(source_pat_clean, paste0("VIEW ", target_name), view_sql, ignore.case = TRUE)
        } else {
          new_sql <- sub(source_pat, paste0("VIEW ", target_name), view_sql, ignore.case = TRUE)
        }
        new_sql <- sub("CREATE VIEW", "CREATE OR REPLACE TEMPORARY VIEW", new_sql, ignore.case = TRUE)
        
        tryCatch({
          DBI::dbExecute(target_conn, new_sql)
          cli::cli_inform("Imported view using SQL recreation (zero overhead)")
          return(list(name = target_name, method = "sql_recreation", cleanup = function() NULL))
        }, error = function(e) {
          cli::cli_alert_info("Strategy 1 (SQL recreation) failed: {conditionMessage(e)}")
        })
      }
    }
  }
  
  # STRATEGY 2: SQL render (same DB, lazy query)
  if (inherits(source_object, c("tbl_lazy", "tbl_duckdb_connection"))) {
    query_sql <- tryCatch({
      as.character(dbplyr::sql_render(source_object, con = source_conn))
    }, error = function(e) NULL)
    
    if (!is.null(query_sql)) {
      view_query <- glue::glue("CREATE OR REPLACE TEMPORARY VIEW {target_name} AS {query_sql}")
      
      tryCatch({
        DBI::dbExecute(target_conn, view_query)
        cli::cli_inform("Imported view using SQL query (zero overhead)")
        return(list(name = target_name, method = "sql_render", cleanup = function() NULL))
      }, error = function(e) {
        cli::cli_alert_info("Strategy 2 (SQL render) failed: {conditionMessage(e)}")
      })
    }
  }
  
  # STRATEGY 3: ATTACH file-based source DB (READ_ONLY)
  # Try multiple ways to get dbdir since dbGetInfo may return NULL
  source_dbdir <- tryCatch({
    info <- DBI::dbGetInfo(source_conn)
    dbdir <- info$dbdir
    if (is.null(dbdir) || length(dbdir) == 0) {
      # Fallback: try to query from DuckDB itself
      res <- DBI::dbGetQuery(source_conn, "SELECT current_database()")
      NULL # Still can't get file path from this
    }
    dbdir
  }, error = function(e) NULL)
  
  if (!is.null(source_dbdir) && source_dbdir != ":memory:" && file.exists(source_dbdir)) {
    source_table <- tryCatch(dbplyr::remote_name(source_object), error = function(e) NULL)
    
    if (!is.null(source_table) && !inherits(source_table, "sql")) {
      attach_alias <- paste0("src_", gsub("-", "_", uuid::UUIDgenerate()))
      source_table_clean <- gsub('^"|"$', "", as.character(source_table))
      
      tryCatch({
        DBI::dbExecute(target_conn, glue::glue("ATTACH '{source_dbdir}' AS {attach_alias} (READ_ONLY)"))
        
        view_query <- glue::glue("
          CREATE OR REPLACE TEMPORARY VIEW {target_name} AS
          SELECT * FROM {attach_alias}.{source_table_clean}
        ")
        DBI::dbExecute(target_conn, view_query)
        
        cli::cli_inform("Imported view using ATTACH (zero-copy, READ_ONLY)")
        return(list(
          name = target_name, 
          method = "attach",
          cleanup = function() {
            tryCatch(DBI::dbExecute(target_conn, glue::glue("DETACH {attach_alias}")), error = function(e) NULL)
          }
        ))
      }, error = function(e) {
        cli::cli_alert_info("Strategy 3 (ATTACH) failed: {conditionMessage(e)}")
        tryCatch(DBI::dbExecute(target_conn, glue::glue("DETACH IF EXISTS {attach_alias}")), error = function(e) NULL)
      })
    }
  }
  
  # STRATEGY 4: Nanoarrow streaming (cross-DB)
  # Materialize into target to avoid Arrow lifecycle issues
  if (inherits(source_object, c("tbl_lazy", "tbl_duckdb_connection", "duckspatial_df"))) {
    query_sql <- tryCatch({
      as.character(dbplyr::sql_render(source_object, con = source_conn))
    }, error = function(e) NULL)
    
    if (!is.null(query_sql)) {
      tryCatch({
        res <- DBI::dbSendQuery(source_conn, query_sql, arrow = TRUE)
        
        reader <- duckdb::duckdb_fetch_arrow(res)
        stream <- nanoarrow::as_nanoarrow_array_stream(reader)
        
        # Register temporarily, then materialize into a view
        temp_arrow_name <- paste0("temp_arrow_", gsub("-", "_", uuid::UUIDgenerate()))
        duckdb::duckdb_register_arrow(target_conn, temp_arrow_name, stream)
        
        # Create permanent view from Arrow data (materializes)
        DBI::dbExecute(target_conn, glue::glue(
          "CREATE OR REPLACE TEMPORARY VIEW {target_name} AS SELECT * FROM {temp_arrow_name}"
        ))
        
        # Cleanup Arrow registration
        DBI::dbClearResult(res)
        tryCatch(duckdb::duckdb_unregister_arrow(target_conn, temp_arrow_name), error = function(e) NULL)
        
        cli::cli_inform("Imported via nanoarrow streaming (zero R materialization)")
        return(list(name = target_name, method = "nanoarrow", cleanup = function() NULL))
      }, error = function(e) {
        cli::cli_alert_info("Strategy 4 (nanoarrow) failed: {conditionMessage(e)}")
      })
    }
  }
  
  # STRATEGY 5: Collect + register (last resort)
  df <- dplyr::collect(source_object)
  
  if (inherits(df, "sf")) {
    duckspatial::ddbs_write_vector(target_conn, df, target_name, temp_view = TRUE)
    cli::cli_warn("Imported via collection (materialized to R, then uploaded)")
    return(list(name = target_name, method = "collect_and_write", data = df, cleanup = function() NULL))
  } else if (is.data.frame(df)) {
    # For non-spatial data, use duckdb_register (zero-copy from R)
    duckdb::duckdb_register(target_conn, target_name, df)
    cli::cli_warn("Imported via duckdb_register (collected to R, zero-copy to target)")
    return(list(name = target_name, method = "duckdb_register", data = df, cleanup = function() NULL))
  } else {
    cli::cli_abort("Import failed: Cannot import object of class {.cls {class(df)}}.")
  }
}

#' Get column names in a DuckDB database
#'
#' @template conn
#' @param x name of the table
#' @param rest whether to return geometry column name, of the rest of the columns
#'
#' @keywords internal
#' @returns name of the geometry column of a table
get_geom_name <- function(conn, x, rest = FALSE, collapse = FALSE, table_id = NULL) {  # nocov start

    # check if the table exists (via DESCRIBE which works for temp views too)
    info_tbl <- try(DBI::dbGetQuery(conn, glue::glue("DESCRIBE {x};")), silent = TRUE)
    
    if (inherits(info_tbl, "try-error")) {
        cli::cli_abort("The table <{x}> does not exist.")
    }
    other_cols <- if (rest) {
        info_tbl[!info_tbl$column_type == "GEOMETRY", "column_name"]
    } else {
        info_tbl[info_tbl$column_type == "GEOMETRY", "column_name"]
    }

    # collapse columns with quoted names
    if (isTRUE(collapse)) {
      if (length(other_cols) > 0) {
        prefix <- if (is.null(table_id)) "" else paste0(table_id, ".")
        other_cols <- paste0(prefix, '"', other_cols, '"', collapse = ', ')
        other_cols <- paste0(other_cols, ", ") # trailing comma for SELECT {rest} something
      } else {
        other_cols <- ""
      }
    }

    return(other_cols)
}  # nocov end


#' Get names for the query
#'
#' @param name table name
#'
#' @keywords internal
#' @returns list with fixed names
get_query_name <- function(name) {  # nocov start
    if (length(name) == 2) {
        table_name <- name[2]
        schema_name <- name[1]
        query_name <- paste0(name, collapse = ".")
    } else {
        table_name   <- name
        schema_name <- "main"
        query_name <- name
    }
    list(
        table_name = table_name,
        schema_name = schema_name,
        query_name = query_name
    )
} # nocov end





#' Get names for the query
#'
#' @param x sf, duckspatial_df, tbl_lazy, or character
#' @template conn_null
#'
#' @keywords internal
#' @noRd
#' @returns list with fixed names
# IMPORTANT: This function returns a cleanup function instead of using on.exit() internally.
# 
# Why? R's on.exit() runs when the function containing it exits, NOT when the caller exits.
# If we used on.exit() here, the temporary views would be dropped as soon as get_query_list()
# returns, BEFORE the caller can execute their SQL query that references the view.
#
# The caller MUST register the cleanup function with on.exit() in their own scope:
#   result <- get_query_list(x, conn)
#   on.exit(result$cleanup(), add = TRUE)
#   # ... use result$query_name ...
#   # ... use result$query_name ...
get_query_list <- function(x, conn) {

  cleanup <- function() NULL  # default no-op
  
  if (inherits(x, "sf")) {

    ## generate a unique temporary view name
    temp_view_name <- paste0(
      "temp_view_",
      gsub("-", "_", uuid::UUIDgenerate())
    )

    # Write table with the unique name
    duckspatial::ddbs_write_vector(
      conn      = conn,
      data      = x,
      name      = temp_view_name,
      quiet     = TRUE,
      temp_view = TRUE
    )

    cleanup <- function() {
      # Try DROP VIEW first
      tryCatch(
        DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {temp_view_name};")),
        error = function(e) NULL
      )
      # Also try unregistering arrow (needed for temp_view=TRUE from ddbs_write_vector)
      tryCatch(
        duckdb::duckdb_unregister_arrow(conn, temp_view_name),
        error = function(e) NULL
      )
    }

    x_list <- get_query_name(temp_view_name)

  } else if (inherits(x, "duckspatial_df")) {
    
    # 1. OPTIMIZATION: Check if we have a direct source table/view
    source_table <- attr(x, "source_table")
    if (!is.null(source_table)) {
      result <- get_query_name(source_table)
      result$cleanup <- function() NULL
      return(result)
    }
    
    # 2. SQL render (efficient lazy evaluation)
    # NOTE: Replaced inefficient collect-and-upload fallback with SQL render.
    # Regression test: tests/testthat/test-regression-inefficient-fallback.R
    # duckspatial_df inherits from tbl_lazy, so sql_render always works
    temp_view_name <- paste0(
      "temp_view_",
      gsub("-", "_", uuid::UUIDgenerate())
    )
    
    query_sql <- dbplyr::sql_render(x, con = conn)
    
    view_query <- glue::glue("
      CREATE OR REPLACE TEMPORARY VIEW {temp_view_name} AS 
      {query_sql}
    ")
    DBI::dbExecute(conn, view_query)
    
    cleanup <- function() {
      tryCatch(
        DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {temp_view_name};")),
        error = function(e) NULL
      )
    }
    
    x_list <- get_query_name(temp_view_name)
    x_list$cleanup <- cleanup
    return(x_list)

  } else if (inherits(x, "tbl_lazy")) {
    
    ## For dbplyr tbl_lazy, we can use sql_render
    temp_view_name <- paste0(
      "temp_view_",
      gsub("-", "_", uuid::UUIDgenerate())
    )
    
    # Get the SQL query from the lazy table
    query_sql <- dbplyr::sql_render(x)
    
    # Create temp view from the query
    view_query <- glue::glue("
      CREATE OR REPLACE TEMPORARY VIEW {temp_view_name} AS 
      {query_sql}
    ")
    DBI::dbExecute(conn, view_query)
    
    cleanup <- function() {
      tryCatch(
        DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {temp_view_name};")),
        error = function(e) NULL
      )
    }
    
    x_list <- get_query_name(temp_view_name)

  } else {
    x_list <- get_query_name(x)
  }

  # Add cleanup function to result
  x_list$cleanup <- cleanup
  return(x_list)

}






#' Converts from data frame to sf
#'
#' Converts a table that has been read from DuckDB into an sf object
#'
#' @param data a tibble or data frame
#' @template crs
#' @param x_geom name of geometry
#'
#' @keywords internal
#' @returns sf
convert_to_sf <- function(data, crs, crs_column, x_geom) { # nocov start
    if (is.null(crs)) {
        if (is.null(crs_column)) {
            data_sf <- data |>
                sf::st_as_sf(wkt = x_geom)
        } else {
            if (crs_column %in% names(data)) {
                data_sf <- data |>
                    sf::st_as_sf(wkt = x_geom, crs = data[1, crs_column])
                data_sf <- data_sf[, -which(names(data_sf) == crs_column)]
            } else {
                cli::cli_alert_warning("No CRS found for the imported table.")
                data_sf <- data |>
                    sf::st_as_sf(wkt = x_geom)
            }
        }

    } else {
        data_sf <- data |>
            sf::st_as_sf(wkt = x_geom, crs = crs)
    }

} # nocov end





#' Gets predicate name
#'
#' Gets a full predicate name from the shorter version
#'
#' @template predicate
#'
#' @keywords internal
#' @returns character
get_st_predicate <- function(predicate) { # nocov start
    switch(predicate,
      "intersects"            = "ST_Intersects",
      "intersects_extent"     = "ST_Intersects_Extent",
      "covers"                = "ST_Covers",
      "touches"               = "ST_Touches",
      "contains"              = "ST_Contains",
      "contains_properly"     = "ST_ContainsProperly",
      "within"                = "ST_Within",
      "within_properly"       = "ST_WithinProperly",
      "disjoint"              = "ST_Disjoint",
      "equals"                = "ST_Equals",
      "overlaps"              = "ST_Overlaps",
      "crosses"               = "ST_Crosses",
      "covered_by"            = "ST_CoveredBy",
      "dwithin"               = "ST_DWithin",
      cli::cli_abort(c(
          "Invalid spatial predicate: {.val {predicate}}",
          "i" = "Valid options: {.val {c('intersects', 'intersects_extent', 'covers', 'touches', 'contains', 'contains_properly', 'within', 'within_properly', 'dwithin', 'disjoint', 'equals', 'overlaps', 'crosses', 'covered_by')}}"
        ))
      )
} # nocov end


#' Converts from data frame to sf using WKB conversion
#'
#' Converts a table that has been read from DuckDB into an sf object.
#'
#' @param data a tibble or data frame
#' @template crs
#' @param x_geom name of geometry column
#'
#' @keywords internal
#' @returns sf
convert_to_sf_wkb <- function(data, crs, crs_column, x_geom) { # nocov start

  # 1. Resolve CRS
  # If CRS is passed explicitly, use it.
  # Otherwise, try to find it in the dataframe column 'crs_column'
  target_crs <- crs
  if (is.null(target_crs)) {
    if (!is.null(crs_column) && crs_column %in% names(data)) {
      # Assume CRS is consistent across the table, take first non-NA
      val <- stats::na.omit(data[[crs_column]])[1]
      if (!is.na(val)) target_crs <- as.character(val)

      # Remove the CRS column from output
      data[[crs_column]] <- NULL
    }
  }

  # Add warning if still no CRS found
  if (is.null(target_crs)) {
    cli::cli_alert_warning("No CRS found for the imported table.")
  }

  # 2. Check Geometry Type and Convert
  geom_data <- data[[x_geom]]

  if (inherits(geom_data, "blob") || is.list(geom_data)) {
    # --- FAST PATH: Binary Data ---

    # Attempt to use wk directly.
    # We use tryCatch because:
    # 1. It handles lists where the first element is NULL (which is.raw() misses)
    # 2. It safely falls back if the list contains non-WKB data
    
    wk_success <- tryCatch({
      # Strip attributes (like 'blob') to ensure it's a clean list for wk
      attributes(geom_data) <- NULL
      
      # OPTIMIZATION: Zero-copy wrap and convert
      wkb_obj <- wk::new_wk_wkb(geom_data)
      data[[x_geom]] <- sf::st_as_sfc(wkb_obj)
      TRUE
    }, error = function(e) {
      FALSE
    })

    if (!wk_success) {
      # --- FALLBACK PATH ---
      # Used if wk failed (e.g., data is native arrow structure or complex list)
      tryCatch({
        ga_vctr <- geoarrow::as_geoarrow_vctr(geom_data)
        data[[x_geom]] <- sf::st_as_sfc(ga_vctr)
      }, error = function(e) {
        # Final fallback: standard sf blob reading
        data[[x_geom]] <- sf::st_as_sfc(structure(geom_data, class = "WKB"))
      })
    }

  } else if (is.character(geom_data)) {
    # --- SLOW PATH: WKT Strings ---
    # Used if the query explicitly used ST_AsText() or older DuckDB versions
    data[[x_geom]] <- sf::st_as_sfc(geom_data)
  }

  # 3. Construct SF Object
  # Use st_as_sf with the pre-converted geometry column
  # We explicitly set the geometry column name to handle cases where x_geom isn't "geometry"
  sf_obj <- sf::st_as_sf(data, sf_column_name = x_geom)

  # 4. Assign CRS if found
  if (!is.null(target_crs)) {
    sf::st_crs(sf_obj) <- sf::st_crs(target_crs)
  }

  return(sf_obj)
} # nocov end






#' Feedback for overwrite argument
#'
#' @param x table name
#' @template conn
#' @template quiet
#' @template overwrite
#'
#' @keywords internal
#' @returns cli message
overwrite_table <- function(x, conn, quiet, overwrite) { # nocov start
  if (overwrite) {
    DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {x};"))
    if (isFALSE(quiet)) cli::cli_alert_info("Table <{x}> dropped")
  }
} # nocov end





#' Feedback for query success
#'
#' @template quiet
#'
#' @keywords internal
#' @returns cli message
feedback_query <- function(quiet) { # nocov start
  if (isFALSE(quiet)) cli::cli_alert_success("Query successful")
} # nocov end




get_nrow <- function(conn, table) { # nocov start
  DBI::dbGetQuery(conn, glue::glue("SELECT COUNT(*) as n FROM {table}"))$n
} # nocov end





reframe_predicate_data <- function(conn, data, x_list, y_list, id_x, id_y, sparse) { # nocov start

  ## get number of rows
  nrowx <- get_nrow(conn, x_list$query_name)
  nrowy <- get_nrow(conn, y_list$query_name)

  ## convert results to matrix -> to list
  ## return matrix if sparse = FALSE
  pred_mat  <- matrix(data$predicate, nrow = nrowx, ncol = nrowy, byrow = TRUE)
  if (isFALSE(sparse)) return(pred_mat)

  pred_list <- apply(pred_mat, 1, function(row) which(row), simplify = FALSE)

  ## return if no matches have been found
  if (length(pred_list) == 0) return(NULL)

  ## rename list if id is provided
  if (!is.null(id_x)) {
    idx_names <- DBI::dbGetQuery(conn, glue::glue("SELECT {id_x} as id FROM {x_list$query_name}"))$id
    names(pred_list) <- idx_names
  }

  ## rename list if id is provided
  if (!is.null(id_y)) {
    idy_names <- DBI::dbGetQuery(conn, glue::glue("SELECT {id_y} as id FROM {y_list$query_name}"))$id
    pred_list <- lapply(pred_list, function(ind) {
      if (length(ind) == 0) return(ind)
      idy_names[ind]
    })
  }

  return(pred_list)

} # nocov end

#' Convert CRS input to DuckDB SQL literal
#'
#' Helper to format numeric EPSG codes, WKT strings, or `sf::st_crs` objects
#' into a SQL literal string compatible with `ST_Transform`.
#'
#' @param x numeric (EPSG), character (WKT/Proj), or `sf` crs object
#'
#' @keywords internal
#' @noRd
#' @returns character string (e.g. "'EPSG:4326'") or "NULL"
crs_to_sql <- function(x) {  # nocov start
  if (is.null(x) || (is.atomic(x) && all(is.na(x)))) return("NULL")

  if (inherits(x, "crs")) {
    if (!is.na(x$epsg)) return(paste0("'EPSG:", x$epsg, "'"))
    if (!is.null(x$wkt)) {
      # Escape single quotes for SQL
      val_clean <- gsub("'", "''", x$wkt)
      return(paste0("'", val_clean, "'"))
    }
    return("NULL")
  }

  if (is.numeric(x)) {
    return(paste0("'EPSG:", as.integer(x), "'"))
  }

  if (is.character(x)) {
    val_clean <- gsub("'", "''", x)
    return(paste0("'", val_clean, "'"))
  }

  return("NULL")
} # nocov end



deprecate_crs <- function(crs_column = "crs_duckspatial", crs = NULL) {

  caller <- deparse(sys.call(-1)[[1]])
  
  if (crs_column != "crs_duckspatial") {
    lifecycle::deprecate_warn(
      when    = "0.9.0",
      what    = paste0(caller, "(crs_column)"),
      details = "Support for CRS will change in the next version and the argument won't be necessary in any function of `duckspatial`.",
      id      = "crs_column"
    )
  }
  
  if (!is.null(crs)) {
    lifecycle::deprecate_warn(
      when    = "0.9.0",
      what    = paste0(caller, "(crs)"),
      details = "Support for CRS will change in the next version and the argument won't be necessary in any function of `duckspatial`.",
      id      = "crs"
    )
  }
}


#' Handle output type for duckspatial functions
#'
#' Converts a data frame/tibble result to the appropriate output type
#' based on the `output` parameter or global options.
#'
#' @param data A data frame/tibble with a WKB geometry column
#' @param conn DuckDB connection
#' @param output Character specifying output type: "duckspatial_df", "sf", or "tibble"
#' @param crs CRS for the output (used for sf conversion)
#' @param crs_column Column name containing CRS info (deprecated)
#' @param x_geom Name of the geometry column
#'
#' @keywords internal
#' @noRd
#' @returns Object of the specified output type
ddbs_handle_output <- function(data, conn, output = NULL, crs = NULL, 
                                crs_column = "crs_duckspatial", x_geom = "geometry") {
  # nocov start
  
  # Resolve output type: parameter > global option > default

  if (is.null(output)) {
    output <- getOption("duckspatial.output_type", "duckspatial_df")
  }
  
  # Validate output type
  valid_outputs <- c("duckspatial_df", "sf", "tibble", "raw", "geoarrow")
  if (!output %in% valid_outputs) {
    cli::cli_abort(
      "{.arg output} must be one of {.val {valid_outputs}}, not {.val {output}}."
    )
  }
  
  # Handle based on output type
  if (output == "tibble") {
    # Remove geometry column and CRS column, return tibble
    data[[x_geom]] <- NULL
    if (crs_column %in% names(data)) {
      data[[crs_column]] <- NULL
    }
    return(tibble::as_tibble(data))
    
  } else if (output == "raw") {
    # Return directly (geometry is already WKB via collect)
    return(tibble::as_tibble(data))
      
  } else if (output == "geoarrow") {
    # Convert WKB to geoarrow_vctr
    geom_data <- data[[x_geom]]
    
    # Needs geoarrow package
    if (!requireNamespace("geoarrow", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg geoarrow} needed for output type 'geoarrow'.")
    }
    
    if (!inherits(geom_data, "geoarrow_vctr")) {
      # Strip blob attributes if present (DuckDB blobs sometimes have extra attrs)
      attributes(geom_data) <- NULL
      col_converted <- tryCatch({
         geoarrow::as_geoarrow_vctr(
            wk::new_wk_wkb(geom_data),
            schema = geoarrow::geoarrow_wkb()
         )
      }, error = function(e) {
         cli::cli_warn("Failed to convert to geoarrow: {conditionMessage(e)}")
         geom_data
      })
      data[[x_geom]] <- col_converted
    }
    return(tibble::as_tibble(data))

  } else if (output == "sf") {
    # Convert to sf object
    data_sf <- convert_to_sf_wkb(
      data       = data,
      crs        = crs,
      crs_column = crs_column,
      x_geom     = x_geom
    )
    return(data_sf)
    
  } else {
    # output == "duckspatial_df"
    # Convert to sf first, then wrap as duckspatial_df
    data_sf <- convert_to_sf_wkb(
      data       = data,
      crs        = crs,
      crs_column = crs_column,
      x_geom     = x_geom
    )
    
    # Get CRS from the sf object
    crs_obj <- sf::st_crs(data_sf)
    
    # Convert sf to duckspatial_df
    result <- as_duckspatial_df(
      x        = data_sf,
      conn     = conn,
      crs      = crs_obj,
      geom_col = x_geom
    )
    
    return(result)
  }
  # nocov end
}

#' Get CRS from a spatial file
#'
#' @param path Path to the file
#' @param conn DuckDB connection
#' @return crs object or NULL
#' @keywords internal
#' @noRd
get_file_crs <- function(path, conn) {
    tryCatch({
      meta_query <- glue::glue("
        SELECT 
          layers[1].geometry_fields[1].crs.auth_name as auth_name,
          layers[1].geometry_fields[1].crs.auth_code as auth_code
        FROM st_read_meta('{path}')
      ")
      meta <- DBI::dbGetQuery(conn, meta_query)
      
      if (!is.na(meta$auth_code) && !is.na(meta$auth_name)) {
        crs_string <- paste0(meta$auth_name, ":", meta$auth_code)
        sf::st_crs(crs_string)
      } else {
        NULL
      }
    }, error = function(e) {
      cli::cli_warn("Could not auto-detect CRS from file: {e$message}")
      NULL
    })
}



#' Get or create default DuckDB connection with spatial extension installed and loaded
#'
#'
#' @param create Logical. If TRUE and no connection exists, create one.
#'   Default is TRUE.
#'
#' @returns A `duckdb_connection` or NULL if no connection exists and
#'   create = FALSE
#'
#' @keywords internal
ddbs_default_conn <- function(create = TRUE) {
  conn <- getOption("duckspatial_conn", NULL)

  # Check if existing connection is still valid

  if (!is.null(conn)) {
    if (!DBI::dbIsValid(conn)) {
      options(duckspatial_conn = NULL)
      conn <- NULL
    }
  }

  # Create new connection if needed
  if (is.null(conn) && create) {
    conn <- ddbs_create_conn(dbdir = "memory")
    options(duckspatial_conn = conn)
  }

  conn
}

#' Generate unique temporary view name
#'
#' Creates a unique name for temporary views to avoid collisions.
#'
#' @returns Character string with unique view name
#' @keywords internal
ddbs_temp_view_name <- function() {
  paste0("temp_view_", gsub("-", "_", uuid::UUIDgenerate()))
}

#' Create an ephemeral DuckDB connection
#'
#' Creates a DuckDB connection that is automatically closed when the calling
#' function exits (either normally or due to an error). For file-based 
#' connections, the database file can also be automatically deleted on cleanup.
#'
#' @param file If TRUE, creates a file-based temporary database instead of 
#'   in-memory. If a character string, uses that as the database file path.
#'   Default is FALSE (in-memory).
#' @param read_only If TRUE and file is provided, opens the connection as 
#'   read-only. Has no effect on in-memory connections. Default is FALSE.
#' @param cleanup If TRUE (default), the connection will be closed (with 
#'   shutdown = TRUE for file-based) and for file-based connections, the 
#'   database file will be deleted.
#' @param envir The environment in which to schedule cleanup. Default is the
#'   parent frame (the caller's environment).
#'
#' @returns A `duckdb_connection` that will be automatically closed on exit.
#'   For file-based connections, also returns the file path as an attribute 
#'   `db_file`.
#' @noRd
#' @keywords internal
ddbs_temp_conn <- function(file = FALSE, read_only = FALSE, cleanup = TRUE, envir = parent.frame()) {
  if (isTRUE(file) || is.character(file)) {
    # File-based connection
    if (is.character(file)) {
      db_file <- file
    } else {
      db_file <- tempfile(fileext = ".duckdb")
    }
    
    conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file, read_only = read_only)
    attr(conn, "db_file") <- db_file
    
    # Cleanup: disconnect and optionally delete file
    withr::defer({
      if (DBI::dbIsValid(conn)) {
        tryCatch(suppressWarnings(DBI::dbDisconnect(conn, shutdown = TRUE)), error = function(e) NULL)
      }
      if (isTRUE(cleanup) && file.exists(db_file)) unlink(db_file)
    }, envir = envir)
  } else {
    # In-memory connection
    conn <- ddbs_create_conn(dbdir = "memory")
    withr::defer(duckdb::dbDisconnect(conn), envir = envir)
  }
  
  conn
}

#' Resolve connections and handle cross-connection imports
#'
#' @param x Input x (sf, duckspatial_df, tbl, character, etc.)
#' @param y Input y
#' @param conn Explicit target connection (optional)
#' @param conn_x Connection for x (optional, resolved if NULL)
#' @param conn_y Connection for y (optional, resolved if NULL)
#' 
#' @return List containing:
#'   - conn: The target connection to use
#'   - x: Updated x (may be new view name if imported)
#'   - y: Updated y (may be new view name if imported)
#'   - cleanup: A function to call on exit to drop temporary views
#' @keywords internal
#' @noRd
resolve_spatial_connections <- function(x, y, conn = NULL, conn_x = NULL, conn_y = NULL) {
    
    cleanup_funs <- list()
    add_cleanup <- function(fn) {
        cleanup_funs <<- c(cleanup_funs, list(fn))
    }
    
    # 1. Resolve source connections
    # If not provided, try to extract from objects
    # Note: Character inputs will return NULL from get_conn_from_input
    source_conn_x <- conn_x %||% get_conn_from_input(x)
    source_conn_y <- conn_y %||% get_conn_from_input(y)
    
    # 2. Determine target connection
    # Priority: explicit conn > conn_x > conn_y > default
    target_conn <- if (!is.null(conn)) {
        conn
    } else if (!is.null(source_conn_x)) {
        source_conn_x
    } else if (!is.null(source_conn_y)) {
        source_conn_y
    } else {
        ddbs_default_conn()
    }
    
    # 2.1 Validate target connection
    if (!DBI::dbIsValid(target_conn)) {
        cli::cli_abort("Target connection is not valid. Please provide a valid DuckDB connection.")
    }
    
    # 2.2 Warn if conn_x and conn_y differ but no explicit conn was provided
    if (is.null(conn) && 
        !is.null(source_conn_x) && 
        !is.null(source_conn_y) && 
        !identical(source_conn_x, source_conn_y)) {
        cli::cli_warn(c(
            "{.arg x} and {.arg y} come from different DuckDB connections.",
            "i" = "Using {.arg x}'s connection as the target. Provide {.arg conn} to override."
        ))
    }
    
    # 3. Handle imports if source connections differ from target
    
    # Check x
    # We only import x if it HAS a source connection that is different from target
    if (!is.null(source_conn_x) && !identical(target_conn, source_conn_x)) {
         # Need to import x
         cli::cli_warn(c(
            "{.arg x} and the target connection are different.",
            "i" = "Importing {.arg x} to the target connection.",
            "i" = "This may require materializing data."
         ))
         
         x_to_import <- x
         if (is.character(x)) {
             x_to_import <- tryCatch({
                 tbl_obj <- dplyr::tbl(source_conn_x, x)
                 suppressWarnings(as_duckspatial_df(tbl_obj))
             }, error = function(e) {
                 tryCatch(dplyr::tbl(source_conn_x, x), error = function(ex) x)
             })
         }
         
         res <- import_view_to_connection(target_conn, source_conn_x, x_to_import)
         x <- res$name
         
         add_cleanup(function() {
             tryCatch(DBI::dbExecute(target_conn, glue::glue("DROP VIEW IF EXISTS {res$name}")), error = function(e) NULL)
             if (is.function(res$cleanup)) tryCatch(res$cleanup(), error = function(e) NULL)
         })
    }
    
    # Check y
    # We only import y if it HAS a source connection that is different from target
    if (!is.null(source_conn_y) && !identical(target_conn, source_conn_y)) {
         # Need to import y
         cli::cli_warn(c(
            "{.arg y} and the target connection are different.",
            "i" = "Importing {.arg y} to the target connection.",
            "i" = "This may require materializing data."
         ))
         
         y_to_import <- y
         if (is.character(y)) {
             y_to_import <- tryCatch({
                 tbl_obj <- dplyr::tbl(source_conn_y, y)
                 suppressWarnings(as_duckspatial_df(tbl_obj))
             }, error = function(e) {
                 tryCatch(dplyr::tbl(source_conn_y, y), error = function(ex) y)
             })
         }
         
         res <- import_view_to_connection(target_conn, source_conn_y, y_to_import)
         y <- res$name
         
         add_cleanup(function() {
             tryCatch(DBI::dbExecute(target_conn, glue::glue("DROP VIEW IF EXISTS {res$name}")), error = function(e) NULL)
             if (is.function(res$cleanup)) tryCatch(res$cleanup(), error = function(e) NULL)
         })
    }
    
    list(
        conn = target_conn,
        x = x,
        y = y,
        cleanup = function() {
            for (fn in cleanup_funs) fn()
        }
    )
}

#' Create a duckspatial lazy spatial data frame
#'
#' Extends tbl_duckdb_connection with spatial metadata (CRS, geometry column).
#'
#' @param x Input: tbl_duckdb_connection, tbl_lazy, or similar dbplyr object
#' @param crs CRS object or string
#' @param geom_col Name of geometry column (default: "geom")
#' @param source_table Name of the source table if applicable
#' @param source_conn Name of the source connection if applicable
#' @param create_view Logical. If TRUE, creates a temporary view for the input query. 
#' Otherwise it generates a temporary table.
#' @return A duckspatial_df object
#' @keywords internal
new_duckspatial_df <- function(
  x, 
  crs = NULL, 
  geom_col = NULL, 
  source_table = NULL,
  source_conn = NULL,
  create_view = FALSE
) {
  # Avoid double wrapping
  if (is_duckspatial_df(x)) return(x)
  
  # This will manage dplyr methods
  # Maybe move to duckspatial_df.tbl_duckdb_connection in the future
  if (inherits(x, "tbl_sql") && is.null(source_table) && !is.null(source_conn)) {

    # Here we won't have a source table, so we will need to create it
    if (create_view) {
      which <- "VIEW"
      source_table <- ddbs_temp_view_name()
    } else {
      which <- "TABLE"
      source_table <- ddbs_temp_table_name()
    }

    # Use sql_render to extract the query
    inner_query <- dbplyr::sql_render(x)

    # Create the table that will be returned as source_table
    # This executes the dplyr verb
    DBI::dbExecute(
      source_conn,
      glue::glue("
        CREATE OR REPLACE TEMP {which} {source_table} AS
        ({inner_query});"
      )
    )
    
    # Handle as a lazy duckdb table in the next step
    x <- dplyr::tbl(source_conn, source_table)
  }
  
  if (!inherits(x, "tbl_sql")) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_sql} (lazy DuckDB table). Use {.fn as_duckspatial_df} for other objects.")
  }

  # If geometry column is not provided, use geom by default
  geom_col <- geom_col %||% "geom"
  
  # Prepend our class
  structure(
    x,
    class = c("duckspatial_df", class(x)),
    sf_column = geom_col, # Keeping attribute name as sf_column for compatibility
    crs = if (inherits(crs, "crs")) crs else sf::st_crs(crs),
    source_table = source_table,
    source_conn = source_conn
  )
}


#' Check if object is a duckspatial_df
#' @param x Object to test
#' @return Logical
#' @export
is_duckspatial_df <- function(x) {
  inherits(x, "duckspatial_df")
}

#' Convert objects to duckspatial_df
#'
#' @description
#' `as_duckspatial_df()` creates a lazy spatial data frame (`duckspatial_df`) from 
#' various inputs. When `x` is a table name (character) or an existing DuckDB 
#' table (`tbl_duckdb_connection`), the function creates a zero-copy representation 
#' of the data directly from the database without loading it into memory. This is 
#' the canonical way to "register" or wrap existing persistent spatial tables.
#' 
#' **CRS Persistence:** `duckspatial` reads native DuckDB 1.5.0+ CRS metadata 
#' and, for compatibility with files written by older versions of `duckspatial`, 
#' CRS metadata stored in column comments. DuckDB files saved in pre-1.5.0 
#' format without `duckspatial`-managed comments will not have CRS information 
#' and will default to `NA` with a warning.
#'
#' @param x Object to convert (sf, tbl_lazy, data.frame, or table name)
#' @param conn DuckDB connection (required for character table names)
#' @param crs CRS object or string. Auto-detected from `sf` objects and 
#'   persistent DuckDB tables.
#' @param geom_col Geometry column name (default: "geom")
#' @param ... Additional arguments passed to methods:
#'   \describe{
#'     \item{\code{coords}}{Character vector of length 2 for point ingestion}
#'     \item{\code{wkt}}{Character name of WKT column for ingestion}
#'     \item{\code{remove}}{Logical. If TRUE (default), coordinate/WKT columns are removed}
#'     \item{\code{na.fail}}{Logical. If TRUE (default), errors on missing values}
#'   }
#' @return A duckspatial_df object
#' @export
as_duckspatial_df <- function(x, conn = NULL, crs = NULL, geom_col = NULL, ...) {
  ddbs_assert_duckdb_crs_support()
  UseMethod("as_duckspatial_df")
}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.duckspatial_df <- function(x, conn = NULL, crs = NULL, 
                                              geom_col = NULL, ...) {
  if (is.null(crs) && is.null(geom_col)) return(x)
  
  # Update metadata if requested
  if (!is.null(crs)) attr(x, "crs") <- sf::st_crs(crs)
  if (!is.null(geom_col)) attr(x, "sf_column") <- geom_col
  
  x
}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.sf <- function(x, conn = NULL, crs = NULL, geom_col = NULL, ...) {
  # Get CRS and geom column from sf object
  if (is.null(crs)) crs <- sf::st_crs(x)
  if (is.null(geom_col)) geom_col <- attr(x, "sf_column")
  
  # Get or create connection
  if (is.null(conn)) {
    conn <- ddbs_default_conn()
  }
  
  # Register sf as temp view
  view_name <- ddbs_temp_view_name()
  ddbs_write_table(
    conn = conn,
    data = x,
    name = view_name,
    quiet = TRUE,
    temp_view = TRUE
  )
  
  # Create lazy table
  lazy_tbl <- dplyr::tbl(conn, view_name)
  result <- new_duckspatial_df(
    lazy_tbl, 
    crs = crs, 
    geom_col = geom_col, 
    source_table = view_name,
    source_conn = conn
  )
  
  return(result)

}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.tbl_duckdb_connection <- function(
  x, 
  conn = NULL, 
  crs = NULL,
  geom_col = NULL, ...
) {
  dots <- list(...)
  if (!is.null(dots$coords) || !is.null(dots$wkt)) {
    return(handle_heterogeneous_ingestion(x, conn, crs, geom_col, ...))
  }

  # Auto-detect CRS if not provided (DuckDB-specific)
  conn <- conn %||% dbplyr::remote_con(x)
  if (is.null(crs) && is.null(dots$coords) && is.null(dots$wkt)) {
    crs <- ddbs_crs(x, conn)
  }
  
  # Auto-detect geometry column
  if (is.null(geom_col)) {
    cols <- colnames(x)
    if ("geometry" %in% cols) {
      geom_col <- "geometry"
    } else if ("geom" %in% cols) {
      geom_col <- "geom"
    } else {
      geom_col <- "geom"
    }
  }
  
  # Extract source table for efficient get_query_list path
  source_table <- tryCatch({
    rem_name <- dbplyr::remote_name(x)
    if (is.null(rem_name)) NULL else as.character(rem_name)
  },
    error = function(e) NULL
  )
  
  # Auto-detect geometry type for collect optimization
  geom_type <- tryCatch({
    desc <- DBI::dbGetQuery(conn, glue::glue("DESCRIBE {dbplyr::sql_render(x)}"))
    desc$column_type[desc$column_name == geom_col]
  }, error = function(e) NULL)
  
  res <- new_duckspatial_df(
    x, 
    crs = crs, 
    geom_col = geom_col, 
    source_table = source_table,
    source_conn = conn
  )
  if (!is.null(geom_type)) attr(res, "geom_type") <- geom_type
  res
}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.tbl_lazy <- function(
  x, 
  conn = NULL, 
  crs = NULL, 
  geom_col = NULL, ...
) {
  dots <- list(...)
  if (!is.null(dots$coords) || !is.null(dots$wkt)) {
    return(handle_heterogeneous_ingestion(x, conn, crs, geom_col, ...))
  }

  # Auto-detect CRS if not provided
  conn <- conn %||% dbplyr::remote_con(x)
  if (is.null(crs) && is.null(dots$coords) && is.null(dots$wkt)) {
    crs <- ddbs_crs(x, conn)
  }
  
  # Auto-detect geometry column
  if (is.null(geom_col)) {
    geom_col <- get_geom_name(conn, x)
  }
  
  # Extract source table for efficient get_query_list path
  source_table <- tryCatch(
    as.character(dbplyr::remote_name(x)),
    error = function(e) NULL
  )
  
  new_duckspatial_df(
    x, 
    crs = crs, 
    geom_col = geom_col, 
    source_table = source_table,
    source_conn = conn
  )
}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.character <- function(
  x, 
  conn = NULL, 
  crs = NULL, 
  geom_col = NULL, ...
) {
  if (is.null(conn)) {
    conn <- ddbs_default_conn(create = FALSE)
    if (is.null(conn)) {
      cli::cli_abort("{.arg conn} must be provided when using table names as input.")
    }
  }

  dots <- list(...)
  if (!is.null(dots$coords) || !is.null(dots$wkt)) {
    return(handle_heterogeneous_ingestion(x, conn, crs, geom_col, ...))
  }
  
  lazy_tbl <- dplyr::tbl(conn, x)
  
  # Auto-detect geometry column
  if (is.null(geom_col)) {
    geom_col <- get_geom_name(conn, x)
  }

  # Auto-detect CRS if not provided
  if (is.null(crs) && is.null(dots$coords) && is.null(dots$wkt)) {
    crs <- ddbs_crs(x, conn)
  }
  
  new_duckspatial_df(
    lazy_tbl, 
    crs = crs, 
    geom_col = geom_col, 
    source_table = x,
    source_conn = conn
  )

}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.data.frame <- function(
  x, 
  conn = NULL, 
  crs = NULL, 
  geom_col = NULL, ...
) {
   # Detect if we have an sfc column that matches geom_col or any sfc column
   is_sfc <- vapply(x, inherits, logical(1), "sfc")
   
   if (any(is_sfc)) {
     # If user provided geom_col, check if it's one of the sfc columns
     if (!is.null(geom_col) && geom_col %in% names(x)) {
        if (!inherits(x[[geom_col]], "sfc")) {
           cli::cli_abort("Column {.val {geom_col}} is not an {.cls sfc} column.")
        }
        # Delegate to sf handler
        return(as_duckspatial_df(sf::st_as_sf(x, sf_column_name = geom_col), conn = conn, crs = crs, ...))
     } else if (inherits(x, "sf")) {
        # Already sf, delegate
        return(as_duckspatial_df.sf(x, conn = conn, crs = crs, geom_col = geom_col, ...))
     } else {
        # Pick the first sfc column as geometry
        first_sfc <- names(x)[which(is_sfc)[1]]
        cli::cli_inform("Detected {.cls sfc} column {.val {first_sfc}}, converting to {.cls sf} first.")
        return(as_duckspatial_df(sf::st_as_sf(x, sf_column_name = first_sfc), conn = conn, crs = crs, ...))
     }
   }

   # Support coords/wkt ingestion for data.frames
   dots <- list(...)
   if (!is.null(dots$coords) || !is.null(dots$wkt)) {
     return(handle_heterogeneous_ingestion(x, conn, crs, geom_col, ...))
   }
   
   # Non-spatial data frame path (only if we can't find sfc)
   # NOTE: Usually we want to error here if it's not spatial, 
   # but maybe someone wants to upload a table and then set_geometry later?
   # For now, let's enforce that it must have at least one sfc or we error.
   if (!any(is_sfc)) {
     # Auto-detect geometry column for raw upload
     if (is.null(geom_col)) {
       if ("geometry" %in% names(x)) {
         geom_col <- "geometry"
       } else if ("geom" %in% names(x)) {
         geom_col <- "geom"
       }
     }
     
     if (!is.null(geom_col) && geom_col %in% names(x)) {
        cli::cli_warn("Column {.val {geom_col}} exists but is not {.cls sfc}. Attempting raw upload.")
     } else {
        cli::cli_abort(c(
          "No {.cls sfc} geometry column found in data frame.",
          "i" = "Use {.fn st_as_sf} first, or provide {.arg coords} or {.arg wkt} to ingest non-spatial data."
        ))
     }
   }

   # Upload to DuckDB
   if (is.null(conn)) conn <- ddbs_default_conn()
   
   view_name <- ddbs_temp_table_name()
   DBI::dbWriteTable(conn, view_name, x)
   
   lazy_tbl <- dplyr::tbl(conn, view_name)
   new_duckspatial_df(
    lazy_tbl, 
    crs = crs, 
    geom_col = geom_col, 
    source_table = view_name,
    source_conn = conn
  )
}

#' Handle ingestion from coords or WKT
#' @keywords internal
handle_heterogeneous_ingestion <- function(x, conn, crs, geom_col, ...) {
  dots <- list(...)
  coords <- dots$coords
  wkt <- dots$wkt
  remove <- dots$remove %||% TRUE
  na.fail <- dots$na.fail %||% TRUE
  
  # 1. Normalize/Resolve
  # We avoid normalize_spatial_input here because it calls as_duckspatial_df,
  # which might call us back. We just need a connection and a query name.
  target_conn <- conn %||% get_conn_from_input(x) %||% ddbs_default_conn()
  
  if (inherits(x, "sf")) {
     # Already handled by as_duckspatial_df.sf before calling us
  } else if (is.data.frame(x)) {
     view_name <- ddbs_temp_table_name()
     DBI::dbWriteTable(target_conn, view_name, x)
     x <- view_name
  }
  
  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = target_conn, quiet = TRUE)
  target_conn <- resolve_conn$conn
  on.exit(resolve_conn$cleanup(), add = TRUE)
  
  x_list <- get_query_list(resolve_conn$x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)
  
  # We use geom_col as intermediate name, ddbs_handle_query will use it
  target_geom <- geom_col %||% "geom"
  
  if (!is.null(coords)) {
    if (!is.character(coords) || length(coords) != 2) {
      cli::cli_abort("{.arg coords} must be a character vector of length 2.")
    }
    crs <- crs %||% "EPSG:4326"
    coords_quoted <- paste0('"', coords, '"')
    
    tryCatch({
      DBI::dbExecute(target_conn, glue::glue("SELECT {paste0(coords_quoted, collapse = ', ')} FROM {x_list$query_name} WHERE 1=0"))
    }, error = function(e) {
      cli::cli_abort("Coordinate columns {.val {coords}} not found in input.")
    })
    
    if (na.fail) {
      null_count <- DBI::dbGetQuery(
        target_conn, 
        glue::glue("SELECT COUNT(*) as n FROM {x_list$query_name} WHERE {paste0(coords_quoted, ' IS NULL', collapse = ' OR ')}")
      )$n
      if (null_count > 0) {
        cli::cli_abort("Missing values found in coordinate columns {.val {coords}}. Set {.code na.fail = FALSE} to ignore.")
      }
    }
    
    coords_str <- paste0(coords_quoted, collapse = ", ")
    st_function <- glue::glue("ST_Point({coords_str})")
    select_clause <- if (remove) glue::glue("SELECT * EXCLUDE ({coords_str}),") else "SELECT *,"
    
    query <- glue::glue("
      {select_clause}
      {build_geom_query(st_function, name = NULL, crs = crs, mode = 'duckspatial')} as {target_geom}
      FROM {x_list$query_name}
    ")
    
    return(ddbs_handle_query(
      query = query,
      conn = target_conn,
      mode = "duckspatial",
      crs = crs,
      x_geom = target_geom
    ))
  }
  
  if (!is.null(wkt)) {
    # WKT internal logic
    assert_character_scalar(wkt, "wkt")
    
    # 2. Validation
    wkt_quoted <- paste0('"', wkt, '"')
    tryCatch({
      DBI::dbExecute(target_conn, glue::glue("SELECT {wkt_quoted} FROM {x_list$query_name} WHERE 1=0"))
    }, error = function(e) {
      cli::cli_abort("WKT column {.val {wkt}} not found in input.")
    })
    
    if (na.fail) {
      null_count <- DBI::dbGetQuery(
        target_conn, 
        glue::glue("SELECT COUNT(*) as n FROM {x_list$query_name} WHERE {wkt_quoted} IS NULL")
      )$n
      if (null_count > 0) {
        cli::cli_abort("Missing values found in WKT column {.val {wkt}}. Set {.code na.fail = FALSE} to ignore.")
      }
    }
    
    # 3. Build Query
    st_function <- glue::glue("ST_GeomFromText({wkt_quoted})")
    select_clause <- if (remove) {
      glue::glue("SELECT * EXCLUDE ({wkt_quoted}),")
    } else {
      "SELECT *,"
    }
    
    query <- glue::glue("
      {select_clause}
      {build_geom_query(st_function, name = NULL, crs = crs, mode = 'duckspatial')} as {target_geom}
      FROM {x_list$query_name}
    ")
    
    return(ddbs_handle_query(
      query = query,
      conn = target_conn,
      mode = "duckspatial",
      crs = crs,
      x_geom = target_geom
    ))
  }
}

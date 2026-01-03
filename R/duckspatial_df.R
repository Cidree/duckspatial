#' Create a duckspatial lazy spatial data frame
#'
#' Extends tbl_duckdb_connection with spatial metadata (CRS, geometry column).
#'
#' @param x Input: tbl_duckdb_connection, tbl_lazy, or similar dbplyr object
#' @param crs CRS object or string
#' @param geom_col Name of geometry column (default: "geom")
#' @param source_table Name of the source table if applicable
#' @return A duckspatial_df object
new_duckspatial_df <- function(x, crs = NULL, geom_col = "geom", source_table = NULL) {
  # Avoid double wrapping
  if (is_duckspatial_df(x)) return(x)
  
  if (!inherits(x, "tbl_sql")) {
    cli::cli_abort("{.arg x} must be a {.cls tbl_sql} (lazy DuckDB table). Use {.fn as_duckspatial_df} for other objects.")
  }
  
  # Prepend our class
  structure(
    x,
    class = c("duckspatial_df", class(x)),
    sf_column = geom_col, # Keeping attribute name as sf_column for compatibility
    crs = if (inherits(crs, "crs")) crs else sf::st_crs(crs),
    source_table = source_table
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
#' @param x Object to convert (sf, tbl_lazy, data.frame, or table name)
#' @param conn DuckDB connection (required for character table names)
#' @param crs CRS object or string (auto-detected from sf objects)
#' @param geom_col Geometry column name (default: "geom")
#' @param ... Additional arguments passed to methods
#' @return A duckspatial_df object
#' @export
as_duckspatial_df <- function(x, conn = NULL, crs = NULL, geom_col = "geom", ...) {
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
  ddbs_write_vector(
    conn = conn,
    data = x,
    name = view_name,
    quiet = TRUE,
    temp_view = TRUE
  )
  
  # Create lazy table
  lazy_tbl <- dplyr::tbl(conn, view_name)
  new_duckspatial_df(lazy_tbl, crs = crs, geom_col = geom_col, source_table = view_name)
}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.tbl_duckdb_connection <- function(x, conn = NULL, crs = NULL, 
                                                     geom_col = "geom", ...) {
  # Auto-detect CRS if not provided (DuckDB-specific)
  if (is.null(crs)) {
    crs <- ddbs_crs(x)
  }
  
  # Extract source table for efficient get_query_list path
  source_table <- tryCatch(
    as.character(dbplyr::remote_name(x)),
    error = function(e) NULL
  )
  
  # Auto-detect geometry type for collect optimization
  geom_type <- tryCatch({
    desc <- DBI::dbGetQuery(dbplyr::remote_con(x), glue::glue("DESCRIBE {dbplyr::sql_render(x)}"))
    desc$column_type[desc$column_name == geom_col]
  }, error = function(e) NULL)
  
  res <- new_duckspatial_df(x, crs = crs, geom_col = geom_col, source_table = source_table)
  if (!is.null(geom_type)) attr(res, "geom_type") <- geom_type
  res
}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.tbl_lazy <- function(x, conn = NULL, crs = NULL, 
                                        geom_col = "geom", ...) {
  # Auto-detect CRS if not provided
  if (is.null(crs)) {
    crs <- ddbs_crs(x)
  }
  
  # Extract source table for efficient get_query_list path
  source_table <- tryCatch(
    as.character(dbplyr::remote_name(x)),
    error = function(e) NULL
  )
  
  new_duckspatial_df(x, crs = crs, geom_col = geom_col, source_table = source_table)
}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.character <- function(x, conn = NULL, crs = NULL, 
                                         geom_col = "geom", ...) {
  if (is.null(conn)) {
    conn <- ddbs_default_conn(create = FALSE)
    if (is.null(conn)) {
      cli::cli_abort("{.arg conn} must be provided when using table names as input.")
    }
  }
  
  lazy_tbl <- dplyr::tbl(conn, x)
  new_duckspatial_df(lazy_tbl, crs = crs, geom_col = geom_col, source_table = x)
}

#' @rdname as_duckspatial_df
#' @export
as_duckspatial_df.data.frame <- function(x, conn = NULL, crs = NULL, 
                                          geom_col = "geom", ...) {
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
   
   # Non-spatial data frame path (only if we can't find sfc)
   # NOTE: Usually we want to error here if it's not spatial, 
   # but maybe someone wants to upload a table and then set_geometry later?
   # For now, let's enforce that it must have at least one sfc or we error.
   if (!any(is_sfc)) {
     if (!is.null(geom_col) && geom_col %in% names(x)) {
        cli::cli_warn("Column {.val {geom_col}} exists but is not {.cls sfc}. Attempting raw upload.")
     } else {
        cli::cli_abort("No {.cls sfc} geometry column found in data frame. Use {.fn st_as_sf} first or provide valid spatial data.")
     }
   }

   # Upload to DuckDB
   if (is.null(conn)) conn <- ddbs_default_conn()
   
   view_name <- ddbs_temp_view_name()
   DBI::dbWriteTable(conn, view_name, x)
   
   lazy_tbl <- dplyr::tbl(conn, view_name)
   new_duckspatial_df(lazy_tbl, crs = crs, geom_col = geom_col, source_table = view_name)
}

#' dplyr methods for duckspatial_df
#'
#' These methods use dplyr's extension mechanism (dplyr_reconstruct) to
#' properly preserve spatial metadata through operations.
#'
#' @name duckspatial_df_dplyr
#' @keywords internal
NULL

# =============================================================================
# Core Extension: dplyr_reconstruct
# =============================================================================

#' @export
#' @importFrom dplyr dplyr_reconstruct
dplyr_reconstruct.duckspatial_df <- function(data, template) {
  # Get the base classes from data
  base_classes <- class(data)
  
  # Reconstruct proper class hierarchy based on template
  # Template has: duckspatial_df, tbl_duckdb_connection, tbl_dbi, tbl_sql, tbl_lazy, tbl
  template_classes <- class(template)
  
  # Use template classes but only if data is likely a lazy table
  # (dplyr verbs on tbl_lazy usually return tbl_lazy)
  if (inherits(data, "tbl_sql")) {
    # Set class to match template structure
    # Ensure duckspatial_df is at the top
    if (!"duckspatial_df" %in% template_classes) {
        # Should not happen if template is duckspatial_df, but safe guard
        template_classes <- c("duckspatial_df", template_classes)
    }
    class(data) <- template_classes
  }
  
  # Restore spatial attributes from template
  attr(data, "sf_column") <- attr(template, "sf_column")
  attr(data, "crs") <- attr(template, "crs")
  attr(data, "source_table") <- attr(template, "source_table")
  
  data
}

# =============================================================================
# collect - Special handling for geometry conversion
# =============================================================================

#' Collect a duckspatial_df with flexible output formats
#'
#' @param x A duckspatial_df object
#' @param ... Additional arguments passed to dplyr::collect
#' @param as Output format: "sf" (default), "tibble" (no geometry), 
#'   "raw" (WKB bytes), or "geoarrow" (geoarrow_vctr)
#' @return Collected data in the specified format
#' @export
#' @importFrom dplyr collect
#' @importFrom rlang :=
collect.duckspatial_df <- function(x, ..., as = c("sf", "tibble", "raw", "geoarrow")) {
  as <- match.arg(as)
  geom_col <- attr(x, "sf_column") %||% "geom"
  crs_obj <- attr(x, "crs")
  
  # Strip class to treat as standard lazy table for further manipulation
  x_lazy <- x
  class(x_lazy) <- setdiff(class(x_lazy), "duckspatial_df")
  
  # --- Handle tibble case: just drop geometry and collect ---
  if (as == "tibble") {
    # If the user requested tibble, they likely don't want the geometry column
    # or don't care about its format. 
    # But if we just collect, it might fail if geom is effectively 'GEOMETRY' type?
    # DuckDB returns native blobs for GEOMETRY type. R handles them as list(raw).
    # So strictly speaking, simple collect works.
    
    # We drop geometry to be safe/consistent with expectation of "non-spatial tibble"
    if (geom_col %in% colnames(x_lazy)) {
       x_lazy <- dplyr::select(x_lazy, -dplyr::all_of(geom_col))
    }
    return(dplyr::collect(x_lazy, ...))
  }
  
  # --- For sf/raw/geoarrow: we need Valid WKB data ---
  # Native DuckDB geometry blobs are NOT standard WKB.
  # We MUST inject ST_AsWKB() execution into the query.
  
  # Check if geometry column exists in the output
  if (geom_col %in% colnames(x_lazy)) {
      # Inject ST_AsWKB(geom) conversion
      # We use dbplyr::sql to pass the raw SQL function
      # We assume the column name is safe or quoted by dbplyr if we used sym?
      # But inside sql() we must quote manually if needed. 
      # dbplyr::ident handles quoting.
      
      # Safer: use dplyr::mutate with sql snippet
      # We need to construct the SQL "ST_AsWKB("colname")" safely.
      # rlang::sym(geom_col) allows dbplyr to quote the column name in the generated SQL.
      # But we need to wrap it in function.
      
      # Method: use explicit SQL string construction
      # x_lazy |> mutate(geom = sql("ST_AsWKB(geom)"))
      # But quoting: ST_AsWKB("geom")
      
      # Let's use dbplyr's translation if available, or raw sql.
      # ST_AsWKB is standard.
      
      # NOTE: We use dplyr::mutate to overwrite the geometry column with its WKB representation
      conn <- dbplyr::remote_con(x_lazy)
      x_lazy <- dplyr::mutate(
          x_lazy, 
           !!rlang::sym(geom_col) := dbplyr::sql(glue::glue("ST_AsWKB(CAST({DBI::dbQuoteIdentifier(conn, geom_col)} AS GEOMETRY))"))
      )
  }
  
  # Collect with dbplyr
  collected <- dplyr::collect(x_lazy, ...)
  
  # --- Convert based on output format ---
  if (!geom_col %in% names(collected)) {
    # No geometry column found (maybe user selected it out), return as tibble
    return(tibble::as_tibble(collected))
  }
  
  if (as == "raw") {
    # Return tibble with raw WKB bytes (no conversion)
    return(tibble::as_tibble(collected))
  }
  
  if (as == "geoarrow") {
    # Convert WKB to geoarrow_vctr
    geom_data <- collected[[geom_col]]
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
      collected[[geom_col]] <- col_converted
    }
    return(tibble::as_tibble(collected))
  }
  
  # as == "sf" (default)
  convert_to_sf_wkb(
    data = collected,
    crs = crs_obj,
    crs_column = NULL,
    x_geom = geom_col
  )
}

# =============================================================================
# Join methods - preserve spatial class from left side
# =============================================================================
# Note: dbplyr join methods typically return tbl_lazy.
# Our dplyr_reconstruct should handle class restoration if dbplyr calls it.
# But often generic joins dispatch to dbplyr methods directly.
# Let's enable generic join methods just in case to verify attributes.

#' @export
#' @importFrom dplyr left_join
left_join.duckspatial_df <- function(x, y, by = NULL, copy = FALSE, 
                                      suffix = c(".x", ".y"), ...,
                                      keep = NULL, na_matches = c("na", "never"),
                                      relationship = NULL) {
  # Strip class to avoid infinite recursion if NextMethod doesn't strip it?
  # Actually NextMethod works fine.
  out <- NextMethod()
  
  # If out lost the class (common with dbplyr), restore it
  if (!inherits(out, "duckspatial_df")) {
      # Re-wrap
      class(out) <- c("duckspatial_df", class(out))
      attr(out, "sf_column") <- attr(x, "sf_column")
      attr(out, "crs") <- attr(x, "crs")
  }
  
  out
}

#' @export
#' @importFrom dplyr inner_join
inner_join.duckspatial_df <- function(x, y, by = NULL, copy = FALSE, 
                                       suffix = c(".x", ".y"), ...,
                                       keep = NULL, na_matches = c("na", "never"),
                                       relationship = NULL) {
  out <- NextMethod()
  if (!inherits(out, "duckspatial_df")) {
      class(out) <- c("duckspatial_df", class(out))
      attr(out, "sf_column") <- attr(x, "sf_column")
      attr(out, "crs") <- attr(x, "crs")
  }
  out
}

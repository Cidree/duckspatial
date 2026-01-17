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

#' @rdname duckspatial_df_dplyr
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
  # NOTE: We deliberately do NOT copy source_table here.

  # The source_table attribute is an optimization hint for get_query_list()
  # that allows direct table reference when the lazy query is unmodified.
  # However, dplyr_reconstruct is called AFTER dplyr verbs modify the query,
  # so copying source_table would cause get_query_list() to use the original
  # table instead of the modified lazy query (losing filters, selects, etc).
  #
  # By not setting source_table, get_query_list() will use sql_render() to
  # create a temporary view from the full lazy query, preserving all operations.
  attr(data, "sf_column") <- attr(template, "sf_column")
  attr(data, "crs") <- attr(template, "crs")
  
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
#' @rdname ddbs_collect
#' @export
#' @importFrom dplyr collect
#' @importFrom rlang :=
collect.duckspatial_df <- function(x, ..., as = NULL) {
  # Resolve output type: parameter > global option > default "sf"
  # Note: if global option is "duckspatial_df" (lazy), collect defaults to "sf" (eager)
  if (is.null(as)) {
    as <- getOption("duckspatial.output_type", "sf")
    if (as == "duckspatial_df") as <- "sf"
  }
  
  as <- match.arg(as, c("sf", "tibble", "raw", "geoarrow"))
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
      conn <- dbplyr::remote_con(x_lazy)
      query_sql <- dbplyr::sql_render(x_lazy)
      
      # Check column type in the lazy table
      # Use cached type from attributes if available to avoid extra DESCRIBE round-trip
      cached_type <- attr(x, "geom_type")
      
      # Inject ST_AsWKB() conversion
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
      
      # Check column type in the lazy table
      # Use cached type from attributes if available to avoid extra DESCRIBE round-trip
      # Use cached type from attributes if available to avoid extra DESCRIBE round-trip
      cached_type <- attr(x, "geom_type")
      
      # Variables to hold resolved state
      target_col_sql <- geom_col # Default to attribute name
      should_convert <- FALSE
      
      if (!is.null(cached_type)) {
          should_convert <- grepl("GEOMETRY|BLOB", cached_type, ignore.case = TRUE)
      } else {
          tryCatch({
              # Check type of geom_col
              # DESCRIBE (query) is standard DuckDB
              desc <- DBI::dbGetQuery(conn, glue::glue("DESCRIBE {query_sql}"))
              
              # Match geom_col (case insensitive search)
              # use numeric index to handle NAs safely
              match_idx <- which(tolower(desc$column_name) == tolower(geom_col))
              
              if (length(match_idx) > 0) {
                  # Found column in DB stats
                  idx <- match_idx[1]
                  col_info <- desc[idx, ]
                  
                  # resolving correct casing from DB for quoting
                  target_col_sql <- col_info$column_name
                  
                  ctype <- if ("column_type" %in% names(col_info)) col_info$column_type else col_info$data_type
                  # We only wrap ST_AsWKB if it is GEOMETRY or BLOB/WKB_BLOB
                  should_convert <- grepl("GEOMETRY|BLOB", ctype, ignore.case = TRUE)
              } else {
                  # Column not found in DESCRIBE? 
                  # It might be present but DESCRIBE logic missed it or view shenanigans.
                  # Fallback: Assume it exists and needs conversion (Safe path)
                  should_convert <- TRUE
              }
          }, error = function(e) {
              # Fallback: safer to wrap than to get raw DuckDB internal blobs
              should_convert <<- TRUE 
          })
      }

      if (should_convert) {
           x_lazy <- dplyr::mutate(
               x_lazy, 
                !!rlang::sym(geom_col) := dbplyr::sql(glue::glue("ST_AsWKB({DBI::dbQuoteIdentifier(conn, target_col_sql)})"))
           )
      }

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

#' @rdname duckspatial_df_dplyr
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

#' @rdname duckspatial_df_dplyr
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

#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom utils head
head.duckspatial_df <- function(x, n = 6L, ...) {
  # Preserve spatial metadata through head()
  crs <- attr(x, "crs")
  geom_col <- attr(x, "sf_column")
  # NOTE: We do NOT preserve source_table here because head() modifies the

  # query (adds LIMIT). The lazy query will be used by get_query_list().
  
  # Strip class to delegate to dbplyr's head.tbl_lazy
  class(x) <- setdiff(class(x), "duckspatial_df")
  
  # Execute via base/dbplyr
  result <- head(x, n = n, ...)
  
  # Re-wrap as duckspatial_df
  class(result) <- c("duckspatial_df", class(result))
  attr(result, "sf_column") <- geom_col
  attr(result, "crs") <- crs
  # source_table intentionally not set
  
  result
}

# =============================================================================
# compute - Force execution while staying lazy
# =============================================================================

#' Force computation of a duckspatial_df
#'
#' Executes the accumulated query and stores the result in a DuckDB temporary
#' table. The result remains lazy (a `duckspatial_df`) but points to the
#' materialized data, avoiding repeated computation of complex query plans.
#'
#' This is useful when you want to:
#' - Cache intermediate results for reuse across multiple subsequent operations
#' - Simplify complex query plans before heavy operations like spatial joins
#' - Force execution at a specific point without pulling data into R memory
#'
#' @param x A `duckspatial_df` object
#' @param name Optional name for the result table. If NULL, a unique temporary
#'   name is generated.
#' @param temporary If TRUE (default), creates a temporary table that is
#'   automatically cleaned up when the connection closes.
#' @param ... Additional arguments passed to [dplyr::compute()]
#' @return A new `duckspatial_df` pointing to the materialized table, with
#'   spatial metadata (CRS, geometry column) preserved.
#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr compute
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Complex pipeline - compute() caches intermediate result
#' result <- countries |>
#'   filter(POP_EST > 50000000) |>
#'   ddbs_filter(argentina, predicate = "touches") |>
#'   compute() |>  # Execute and cache here
#'   select(NAME_ENGL, POP_EST) |>
#'   ddbs_join(rivers, join = "intersects")
#'
#' # Check query plan - should reference the cached table
#' show_query(result)
#' }
compute.duckspatial_df <- function(x, name = NULL, temporary = TRUE, ...) {
  # Extract spatial metadata before compute
  crs <- attr(x, "crs")
  geom_col <- attr(x, "sf_column")
  
  # Strip our class to delegate to dbplyr's compute.tbl_sql
  class(x) <- setdiff(class(x), "duckspatial_df")
  
  # Execute via dbplyr
  result <- dplyr::compute(x, name = name, temporary = temporary, ...)
  
  # Get the new table name
  new_source <- tryCatch(
    as.character(dbplyr::remote_name(result)),
    error = function(e) NULL
  )
  
  # Re-wrap as duckspatial_df with preserved metadata
  new_duckspatial_df(result, crs = crs, geom_col = geom_col, source_table = new_source)
}

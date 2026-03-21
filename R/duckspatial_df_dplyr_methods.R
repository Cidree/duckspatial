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

# #' @rdname duckspatial_df_dplyr
# #' @export
# #' @importFrom dplyr dplyr_reconstruct
# dplyr_reconstruct.duckspatial_df <- function(data, template) {
#   # Get the base classes from data
#   base_classes <- class(data)
  
#   # Reconstruct proper class hierarchy based on template
#   # Template has: duckspatial_df, tbl_duckdb_connection, tbl_dbi, tbl_sql, tbl_lazy, tbl
#   template_classes <- class(template)
  
#   # Use template classes but only if data is likely a lazy table
#   # (dplyr verbs on tbl_lazy usually return tbl_lazy)
#   if (inherits(data, "tbl_sql")) {
#     # Set class to match template structure
#     # Ensure duckspatial_df is at the top
#     if (!"duckspatial_df" %in% template_classes) {
#         # Should not happen if template is duckspatial_df, but safe guard
#         template_classes <- c("duckspatial_df", template_classes)
#     }
#     class(data) <- template_classes
#   }
  
#   # Restore spatial attributes from template
#   # NOTE: We deliberately do NOT copy source_table here.

#   # The source_table attribute is an optimization hint for get_query_list()
#   # that allows direct table reference when the lazy query is unmodified.
#   # However, dplyr_reconstruct is called AFTER dplyr verbs modify the query,
#   # so copying source_table would cause get_query_list() to use the original
#   # table instead of the modified lazy query (losing filters, selects, etc).
#   #
#   # By not setting source_table, get_query_list() will use sql_render() to
#   # create a temporary view from the full lazy query, preserving all operations.
#   attr(data, "sf_column") <- attr(template, "sf_column")
#   attr(data, "crs") <- attr(template, "crs")
  
#   data
# }

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
  # class(x_lazy) <- setdiff(class(x_lazy), "duckspatial_df")
  
  # --- Handle tibble case: just drop geometry and collect ---
  if (as == "tibble") {
    # If the user requested tibble, they likely don't want the geometry column
    # or don't care about its format. 
    # But if we just collect, it might fail if geom is effectively 'GEOMETRY' type?
    # DuckDB returns native blobs for GEOMETRY type. R handles them as list(raw).
    # So strictly speaking, simple collect works.

    # Strip class
    class(x_lazy) <- setdiff(class(x_lazy), "duckspatial_df")
    
    # We drop geometry to be safe/consistent with expectation of "non-spatial tibble"
    if (geom_col %in% colnames(x_lazy)) {
       x_lazy <- dplyr::select(x_lazy, -dplyr::all_of(geom_col))
    }
    return(dplyr::collect(x_lazy, ...))
  }

  # sf/raw/geoarrow in DuckDB 1.5
  if (geom_col %in% colnames(x_lazy)) {

    # Get the table connection and name
    conn <- attr(x, "source_conn") %||% dbplyr::remote_con(x)
    table_name <- attr(x, "source_table")

    # In order to collect, we need to cast the geometry type to an R-understandable
    # format. We will create a temporary view that we will collect
    temp_view <- ddbs_temp_view_name()
    DBI::dbExecute(
      conn,
      glue::glue("
        CREATE TEMP VIEW {temp_view} AS
        SELECT * REPLACE(ST_AsWKB({geom_col}) AS {geom_col})
        FROM {table_name};
      ")
    )
    on.exit(DBI::dbExecute(conn, glue::glue("DROP VIEW IF EXISTS {temp_view}")))

    # Get tbl lazily
    x_lazy <- dplyr::tbl(conn, temp_view)

  }



  
  # --- For sf/raw/geoarrow: we need Valid WKB data ---
  # Native DuckDB geometry blobs are NOT standard WKB.
  # We MUST inject ST_AsWKB() execution into the query.
  
  # Check if geometry column exists in the output
  # if (geom_col %in% colnames(x_lazy)) {
  #     conn <- dbplyr::remote_con(x_lazy)
  #     query_sql <- dbplyr::sql_render(x_lazy)
      
  #     # Inject ST_AsWKB() conversion
  #     # We use dbplyr::sql to pass the raw SQL function
  #     # We assume the column name is safe or quoted by dbplyr if we used sym?
  #     # But inside sql() we must quote manually if needed. 
  #     # dbplyr::ident handles quoting.
      
  #     # Safer: use dplyr::mutate with sql snippet
  #     # We need to construct the SQL "ST_AsWKB("colname")" safely.
  #     # rlang::sym(geom_col) allows dbplyr to quote the column name in the generated SQL.
  #     # But we need to wrap it in function.
      
  #     # Method: use explicit SQL string construction
  #     # x_lazy |> mutate(geom = sql("ST_AsWKB(geom)"))
  #     # But quoting: ST_AsWKB("geom")
      
  #     # Let's use dbplyr's translation if available, or raw sql.
  #     # ST_AsWKB is standard.
      
  #     # Check column type in the lazy table
  #     # Use cached type from attributes if available to avoid extra DESCRIBE round-trip
  #     # Use cached type from attributes if available to avoid extra DESCRIBE round-trip
  #     cached_type <- attr(x, "geom_type")
      
  #     # Variables to hold resolved state
  #     target_col_sql <- geom_col # Default to attribute name
  #     should_convert <- FALSE
      
  #     if (!is.null(cached_type)) {
  #         should_convert <- grepl("GEOMETRY|BLOB", cached_type, ignore.case = TRUE)
  #     } else {
  #         tryCatch({
  #             # Check type of geom_col
  #             # DESCRIBE (query) is standard DuckDB
  #             desc <- DBI::dbGetQuery(conn, glue::glue("DESCRIBE {query_sql}"))
              
  #             # Match geom_col (case insensitive search)
  #             # use numeric index to handle NAs safely
  #             match_idx <- which(tolower(desc$column_name) == tolower(geom_col))
              
  #             if (length(match_idx) > 0) {
  #                 # Found column in DB stats
  #                 idx <- match_idx[1]
  #                 col_info <- desc[idx, ]
                  
  #                 # resolving correct casing from DB for quoting
  #                 target_col_sql <- col_info$column_name
                  
  #                 ctype <- if ("column_type" %in% names(col_info)) col_info$column_type else col_info$data_type
  #                 # We only wrap ST_AsWKB if it is GEOMETRY or BLOB/WKB_BLOB
  #                 should_convert <- grepl("GEOMETRY|BLOB", ctype, ignore.case = TRUE)
  #             } else {
  #                 # Column not found in DESCRIBE? 
  #                 # It might be present but DESCRIBE logic missed it or view shenanigans.
  #                 # Fallback: Assume it exists and needs conversion (Safe path)
  #                 should_convert <- TRUE
  #             }
  #         }, error = function(e) {
  #             # Fallback: safer to wrap than to get raw DuckDB internal blobs
  #             should_convert <<- TRUE 
  #         })
  #     }

  #     if (should_convert) {
  #          x_lazy <- dplyr::mutate(
  #              x_lazy, 
  #               !!rlang::sym(geom_col) := dbplyr::sql(glue::glue("ST_AsWKB({DBI::dbQuoteIdentifier(conn, target_col_sql)})"))
  #          )
  #     }

  # }
  
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
    x_geom = geom_col
  )
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





# =============================================================================
# Passthrough verbs — geometry unchanged, use dplyr_reconstruct pattern
# =============================================================================


#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr select
select.duckspatial_df <- function(.data, ...) {
  atts <- attributes(.data)
  
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Strip geometry, select requested cols, re-add geometry as WKB
  res <- dplyr::select(.data, ..., dplyr::all_of(atts$sf_column))
  
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr filter
filter.duckspatial_df <- function(.data, ...) {
  atts <- attributes(.data)
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  res <- dplyr::filter(.data, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr arrange
arrange.duckspatial_df <- function(.data, ...) {
  atts <- attributes(.data)
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  res <- dplyr::arrange(.data, ...)
  structure(res,
    class        = c("duckspatial_df", class(res)),
    sf_column    = atts$sf_column,
    crs          = atts$crs,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr rename
rename.duckspatial_df <- function(.data, ...) {
  atts <- attributes(.data)
  
  # Check if geometry column is being renamed
  dots <- rlang::enquos(...)
  new_geom_col <- atts$sf_column
  for (nm in names(dots)) {
    if (rlang::as_name(dots[[nm]]) == atts$sf_column) {
      new_geom_col <- nm
      break
    }
  }
  
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  res <- dplyr::rename(.data, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr slice
slice.duckspatial_df <- function(.data, ...) {
  atts <- attributes(.data)
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  res <- dplyr::slice(.data, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom utils head
head.duckspatial_df <- function(x, n = 6L, ...) {
  atts <- attributes(x)

  class(x) <- setdiff(class(x), "duckspatial_df")
  res <- head(x, n = n, ...)

  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}

#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr glimpse
glimpse.duckspatial_df <- function(x, width = NULL, ...) {
  # Preserve spatial metadata
  crs <- attr(x, "crs")
  geom_col <- attr(x, "sf_column")
  bbox <- st_bbox(x)
  geomtype <- ddbs_geometry_type(x, by_feature = FALSE) |> 
    as.character()

  # Header
  cat(cli::col_white("# A duckspatial lazy spatial table\n"))
  cat(cli::col_white("#"), cli::col_blue("\u25cf CRS:"), cli::col_white(ddbs_format_crs(crs)), "\n")
  cat(cli::col_white("#"), cli::col_blue("\u25cf Geometry column:"), cli::col_white(geom_col), "\n")
  cat(cli::col_white("#"), cli::col_blue("\u25cf Geometry type:"), cli::col_white(geomtype), "\n")
  cat(cli::col_white("#"), cli::col_blue("\u25cf Bounding box:"), 
      cli::col_white(sprintf("xmin: %.5g ymin: %.5g xmax: %.5g ymax: %.5g", 
                              bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])), "\n")
  cat(cli::col_white("# Data backed by DuckDB (dbplyr lazy evaluation)\n"))
  cat(cli::col_white("# Use ddbs_collect() or st_as_sf() to materialize to sf\n"))
  cat(cli::col_white("#\n"))

  # Execute via dplyr
  source_table <- attr(x, "source_table")
  head_data <- dplyr::tbl(
    dbplyr::remote_con(x),
    dplyr::sql(glue::glue(
      "SELECT * REPLACE (ST_AsWKB({geom_col}) AS {geom_col})
       FROM {source_table} LIMIT 20"
    ))
  )
  dplyr::glimpse(head_data, width = width, ...)

  invisible(x)
}





# =============================================================================
# Transforming verbs — may change geometry, use new_duckspatial_df pattern
# =============================================================================

#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr mutate
mutate.duckspatial_df <- function(.data, ...) {
  atts <- attributes(.data)
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  res <- dplyr::mutate(.data, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr count
count.duckspatial_df <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  # count() aggregates — intentionally drops spatial class (no geometry)
  class(x) <- setdiff(class(x), "duckspatial_df")
  dplyr::count(x, ..., wt = {{ wt }}, sort = sort, name = name)
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr distinct
distinct.duckspatial_df <- function(.data, ..., .keep_all = FALSE) {
  crs <- attr(.data, "crs")
  geom_col <- attr(.data, "sf_column")
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  out <- dplyr::distinct(.data, ..., .keep_all = .keep_all)

  ## if geometry column is in the table, return duckspatial_df
  ## otherwise, return a lazy tbl
  if (geom_col %in% colnames(out)) {
    class(out) <- c("duckspatial_df", class(out))
    attr(out, "sf_column") <- geom_col
    attr(out, "crs") <- crs
    out
  } else {
    out
  }
}





# =============================================================================
# Join methods - preserve spatial class from left side
# =============================================================================
# Note: dbplyr join methods typically return tbl_lazy.
# Our dplyr_reconstruct should handle class restoration if dbplyr calls it.
# But often generic joins dispatch to dbplyr methods directly.
# Let's enable generic join methods just in case to verify attributes.

# #' @rdname duckspatial_df_dplyr
# #' @export
# #' @importFrom dplyr left_join
# left_join.duckspatial_df <- function(x, y, by = NULL, copy = FALSE, 
#                                       suffix = c(".x", ".y"), ...,
#                                       keep = NULL, na_matches = c("na", "never"),
#                                       relationship = NULL) {
#   # Strip class to avoid infinite recursion if NextMethod doesn't strip it?
#   # Actually NextMethod works fine.
#   out <- NextMethod()
  
#   # If out lost the class (common with dbplyr), restore it
#   if (!inherits(out, "duckspatial_df")) {
#       # Re-wrap
#       class(out) <- c("duckspatial_df", class(out))
#       attr(out, "sf_column") <- attr(x, "sf_column")
#       attr(out, "crs") <- attr(x, "crs")
#   }
  
#   out
# }

# #' @rdname duckspatial_df_dplyr
# #' @export
# #' @importFrom dplyr inner_join
# inner_join.duckspatial_df <- function(x, y, by = NULL, copy = FALSE, 
#                                        suffix = c(".x", ".y"), ...,
#                                        keep = NULL, na_matches = c("na", "never"),
#                                        relationship = NULL) {
#   out <- NextMethod()
#   if (!inherits(out, "duckspatial_df")) {
#       class(out) <- c("duckspatial_df", class(out))
#       attr(out, "sf_column") <- attr(x, "sf_column")
#       attr(out, "crs") <- attr(x, "crs")
#   }
#   out
# }

#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr left_join
left_join.duckspatial_df <- function(x, y, by = NULL, ...) {
  atts <- attributes(x)
  class(x) <- setdiff(class(x), "duckspatial_df")
  if (inherits(y, "duckspatial_df")) {
    class(y) <- setdiff(class(y), "duckspatial_df")
  }
  res <- dplyr::left_join(x, y, by = by, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr inner_join
inner_join.duckspatial_df <- function(x, y, by = NULL, ...) {
  atts <- attributes(x)
  class(x) <- setdiff(class(x), "duckspatial_df")
  if (inherits(y, "duckspatial_df")) {
    class(y) <- setdiff(class(y), "duckspatial_df")
  }
  res <- dplyr::inner_join(x, y, by = by, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr right_join
right_join.duckspatial_df <- function(x, y, by = NULL, ...) {
  atts <- attributes(x)
  class(x) <- setdiff(class(x), "duckspatial_df")
  if (inherits(y, "duckspatial_df")) {
    class(y) <- setdiff(class(y), "duckspatial_df")
  }
  res <- dplyr::right_join(x, y, by = by, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}



#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr full_join
full_join.duckspatial_df <- function(x, y, by = NULL, ...) {
  atts <- attributes(x)
  class(x) <- setdiff(class(x), "duckspatial_df")
  if (inherits(y, "duckspatial_df")) {
    class(y) <- setdiff(class(y), "duckspatial_df")
  }
  res <- dplyr::full_join(x, y, by = by, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}





# =============================================================================
# Grouping and aggregating methods
# =============================================================================


#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr group_by
group_by.duckspatial_df <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  atts <- attributes(.data)
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  res <- dplyr::group_by(.data, ..., .add = .add, .drop = .drop)
  return(res)
}

#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr ungroup
ungroup.duckspatial_df <- function(x, ...) {
  atts <- attributes(x)
  class(x) <- setdiff(class(x), "duckspatial_df")
  res <- dplyr::ungroup(x, ...)
  new_duckspatial_df(
    x            = res,
    crs          = atts$crs,
    geom_col     = atts$sf_column,
    source_table = NULL,
    source_conn  = atts$source_conn
  )
}

#' @rdname duckspatial_df_dplyr
#' @export
#' @importFrom dplyr summarise
summarise.duckspatial_df <- function(.data, ...) {
  atts <- attributes(.data)
  geom_col <- atts$sf_column

  # Check if geometry column is preserved in the summary by name only
  dots <- rlang::enquos(...)
  geom_preserved <- geom_col %in% names(dots)

  class(.data) <- setdiff(class(.data), "duckspatial_df")
  res <- dplyr::summarise(.data, ...)

  if (geom_preserved) {
    new_duckspatial_df(
      x            = res,
      crs          = atts$crs,
      geom_col     = geom_col,
      source_table = NULL,
      source_conn  = atts$source_conn
    )
  } else {
    cli::cli_warn(c(
      "Geometry column {.field {geom_col}} was dropped by {.fn summarise}.",
      "i" = "Use a spatial aggregate like {.fn ST_Union_Agg} to preserve geometry.",
      "i" = "Result is no longer a {.cls duckspatial_df}."
    ))
    res
  }
}
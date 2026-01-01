#' dplyr methods for duckspatial_df with geometry protection
#'
#' These methods intercept dplyr verbs to:
#' 1. Prevent accidental modification of geometry columns
#' 2. Preserve spatial metadata (CRS, geometry column name) through operations
#' 3. Warn when geometry is dropped
#'
#' @name duckspatial_df_dplyr
#' @keywords internal
NULL

#' Restore duckspatial_df class after dplyr operation
#' @keywords internal
restore_duckspatial_df <- function(out, template) {
  # Prepend our class
  class(out) <- c("duckspatial_df", class(out))
  
  # Restore spatial metadata
  attr(out, "sf_column") <- attr(template, "sf_column")
  attr(out, "crs") <- attr(template, "crs")
  
  out
}

#' @export
#' @importFrom dplyr mutate
mutate.duckspatial_df <- function(.data, ...) {
  geom_col <- attr(.data, "sf_column") %||% "geometry"
  dots <- rlang::enquos(...)
  
  # Check if any expression modifies geometry column
  modified_cols <- names(dots)
  if (geom_col %in% modified_cols) {
    cli::cli_abort(c(
      "Cannot modify geometry column {.field {geom_col}} with {.fn mutate}.",
      "i" = "Use spatial functions like {.fn ddbs_buffer} or {.fn ddbs_centroid} to transform geometries.",
      "i" = "To rename geometry column, use {.fn ddbs_rename_geometry}."
    ))
  }
  
  # Remove our class temporarily to dispatch to duckplyr
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Call duckplyr's mutate
  out <- dplyr::mutate(.data, ...)
  
  # Restore our class
  restore_duckspatial_df(out, .data)
}

#' @export
#' @importFrom dplyr select
select.duckspatial_df <- function(.data, ...) {
  geom_col <- attr(.data, "sf_column") %||% "geometry"
  crs <- attr(.data, "crs")
  
  # Remove our class temporarily to dispatch to duckplyr
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Call duckplyr's select
  out <- dplyr::select(.data, ...)
  
  # Check if geometry column was dropped
  if (!geom_col %in% names(out)) {
    cli::cli_warn(c(
      "Geometry column {.field {geom_col}} was dropped.",
      "i" = "Result is no longer a spatial object.",
      "i" = "Use {.code select(..., {geom_col})} to keep geometry."
    ))
    # Return as plain duckplyr_df without spatial class
    return(out)
  }
  
  # Restore our class
  restore_duckspatial_df(out, .data)
}

#' @export
#' @importFrom dplyr filter
filter.duckspatial_df <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  geom_col <- attr(.data, "sf_column")
  crs <- attr(.data, "crs")
  
  # Remove our class temporarily to dispatch to duckplyr
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Call duckplyr's filter
  out <- dplyr::filter(.data, ..., .by = .by, .preserve = .preserve)
  
  # Restore our class
  restore_duckspatial_df(out, .data)
}

#' @export
#' @importFrom dplyr arrange
arrange.duckspatial_df <- function(.data, ..., .by_group = FALSE) {
  geom_col <- attr(.data, "sf_column")
  crs <- attr(.data, "crs")
  
  # Remove our class temporarily to dispatch to duckplyr
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Call duckplyr's arrange
  out <- dplyr::arrange(.data, ..., .by_group = .by_group)
  
  # Restore our class
  restore_duckspatial_df(out, .data)
}

#' @export
#' @importFrom dplyr rename
rename.duckspatial_df <- function(.data, ...) {
  geom_col <- attr(.data, "sf_column") %||% "geometry"
  crs <- attr(.data, "crs")
  
  # Capture rename mappings
  dots <- rlang::enquos(...)
  
  # Remove our class temporarily
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Call duckplyr's rename
  out <- dplyr::rename(.data, ...)
  
  # Check if geometry column was renamed
  # dots format: new_name = old_name, so we need to find if geom_col was an old_name
  # This is tricky because quosure evaluation...
  # For now, just check if geom_col is still in names
  if (!geom_col %in% names(out)) {
    # Find the new name
    for (i in seq_along(dots)) {
      old_name <- rlang::as_name(rlang::quo_get_expr(dots[[i]]))
      if (old_name == geom_col) {
        geom_col <- names(dots)[i]
        break
      }
    }
  }
  
  # Restore our class with potentially updated geom_col
  class(out) <- c("duckspatial_df", class(out))
  attr(out, "sf_column") <- geom_col
  attr(out, "crs") <- crs
  
  out
}

#' @export
#' @importFrom dplyr summarise
summarise.duckspatial_df <- function(.data, ..., .by = NULL, .groups = NULL) {
  geom_col <- attr(.data, "sf_column") %||% "geometry"
  
  cli::cli_warn(c(
    "{.fn summarise} on spatial data drops geometry.",
    "i" = "Use {.fn ddbs_union} or {.fn ddbs_combine} for spatial aggregation."
  ))
  
  # Remove our class - result won't be spatial
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Call duckplyr's summarise
  dplyr::summarise(.data, ..., .by = {{ .by }}, .groups = .groups)
}

#' @export
#' @importFrom dplyr summarize
summarize.duckspatial_df <- summarise.duckspatial_df

#' @export
#' @importFrom dplyr group_by
group_by.duckspatial_df <- function(.data, ..., .add = FALSE, .drop = TRUE) {
  geom_col <- attr(.data, "sf_column")
  crs <- attr(.data, "crs")
  
  # Remove our class temporarily
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Call duckplyr's group_by
  out <- dplyr::group_by(.data, ..., .add = .add, .drop = .drop)
  
  # Restore our class
  restore_duckspatial_df(out, .data)
}

#' @export
#' @importFrom dplyr ungroup
ungroup.duckspatial_df <- function(x, ...) {
  geom_col <- attr(x, "sf_column")
  crs <- attr(x, "crs")
  
  # Remove our class temporarily
  class(x) <- setdiff(class(x), "duckspatial_df")
  
  # Call duckplyr's ungroup
  out <- dplyr::ungroup(x, ...)
  
  # Restore our class
  class(out) <- c("duckspatial_df", class(out))
  attr(out, "sf_column") <- geom_col
  attr(out, "crs") <- crs
  
  out
}

#' @export
#' @importFrom dplyr distinct
distinct.duckspatial_df <- function(.data, ..., .keep_all = FALSE) {
  geom_col <- attr(.data, "sf_column")
  crs <- attr(.data, "crs")
  
  # Remove our class temporarily
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  
  # Call duckplyr's distinct
  out <- dplyr::distinct(.data, ..., .keep_all = .keep_all)
  
  # Check if geometry is still there
  if (!geom_col %in% names(out)) {
    cli::cli_warn("Geometry column {.field {geom_col}} was dropped by {.fn distinct}.")
    return(out)
  }
  
  # Restore our class
  restore_duckspatial_df(out, .data)
}

#' @export
#' @importFrom dplyr slice_head
slice_head.duckspatial_df <- function(.data, ..., n, prop, by = NULL) {
  geom_col <- attr(.data, "sf_column")
  crs <- attr(.data, "crs")
  
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  out <- dplyr::slice_head(.data, n = n, prop = prop, by = {{ by }})
  restore_duckspatial_df(out, .data)
}

#' @export
#' @importFrom dplyr slice_tail
slice_tail.duckspatial_df <- function(.data, ..., n, prop, by = NULL) {
  geom_col <- attr(.data, "sf_column")
  crs <- attr(.data, "crs")
  
  class(.data) <- setdiff(class(.data), "duckspatial_df")
  out <- dplyr::slice_tail(.data, n = n, prop = prop, by = {{ by }})
  restore_duckspatial_df(out, .data)
}

#' @export
#' @importFrom dplyr collect
collect.duckspatial_df <- function(x, ...) {
  ddbs_collect(x, ...)
}

#' @export
#' @importFrom dplyr pull
pull.duckspatial_df <- function(.data, var = -1, name = NULL, ...) {
  # Remove our class - pull returns a vector, not spatial

  class(.data) <- setdiff(class(.data), "duckspatial_df")
  dplyr::pull(.data, var = {{ var }}, name = {{ name }}, ...)
}

#' @export
#' @importFrom dplyr left_join
left_join.duckspatial_df <- function(x, y, by = NULL, copy = FALSE, 
                                      suffix = c(".x", ".y"), ...,
                                      keep = NULL, na_matches = c("na", "never"),
                                      relationship = NULL) {
  geom_col <- attr(x, "sf_column")
  crs <- attr(x, "crs")
  
  class(x) <- setdiff(class(x), "duckspatial_df")
  out <- dplyr::left_join(x, y, by = by, copy = copy, suffix = suffix, ...,
                          keep = keep, na_matches = na_matches, 
                          relationship = relationship)
  
  if (geom_col %in% names(out)) {
    restore_duckspatial_df(out, x)
  } else {
    out
  }
}

#' @export
#' @importFrom dplyr inner_join
inner_join.duckspatial_df <- function(x, y, by = NULL, copy = FALSE, 
                                       suffix = c(".x", ".y"), ...,
                                       keep = NULL, na_matches = c("na", "never"),
                                       relationship = NULL) {
  geom_col <- attr(x, "sf_column")
  crs <- attr(x, "crs")
  
  class(x) <- setdiff(class(x), "duckspatial_df")
  out <- dplyr::inner_join(x, y, by = by, copy = copy, suffix = suffix, ...,
                           keep = keep, na_matches = na_matches, 
                           relationship = relationship)
  
  if (geom_col %in% names(out)) {
    restore_duckspatial_df(out, x)
  } else {
    out
  }
}

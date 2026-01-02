#' sf methods for duckspatial_df
#'
#' These methods provide sf compatibility for duckspatial_df objects,
#' allowing them to work with sf functions like st_crs(), st_geometry(), etc.
#'
#' @name duckspatial_df_sf
#' @keywords internal
NULL

#' @export
#' @importFrom sf st_crs
st_crs.duckspatial_df <- function(x, ...) {
  attr(x, "crs") %||% sf::st_crs(NA)
}

#' @export
#' @importFrom sf st_geometry
st_geometry.duckspatial_df <- function(obj, ...) {
  geom_col <- attr(obj, "sf_column") %||% "geometry"
  
  # Access the geometry column by materializing it from DuckDB
  geom_data <- dplyr::pull(obj, geom_col)
  
  if (is.null(geom_data)) {
    cli::cli_abort("Geometry column {.field {geom_col}} not found in data.")
  }
  
  # Convert WKB to sfc using the helper from db_utils_not_exported.R
  convert_wkb_to_sfc(geom_data, st_crs(obj))
}

#' Convert WKB data to sfc
#' @keywords internal
convert_wkb_to_sfc <- function(geom_data, crs) {
  if (inherits(geom_data, "sfc")) return(geom_data)
  
  # Use wk for fast conversion if possible
  tryCatch({
    attributes(geom_data) <- NULL
    wkb_obj <- wk::new_wk_wkb(geom_data)
    sf::st_as_sfc(wkb_obj, crs = crs)
  }, error = function(e) {
    # Fallback to slow path
    sf::st_as_sfc(structure(geom_data, class = "WKB"), crs = crs)
  })
}

#' @export
#' @importFrom sf st_bbox
st_bbox.duckspatial_df <- function(obj, ...) {
  geom_col <- attr(obj, "sf_column") %||% "geometry"
  crs <- st_crs(obj)
  

  # Try to use DuckDB's ST_Extent for efficiency
  tryCatch({
    # Get the DuckDB relation and execute extent query
    # For now, fall back to materializing geometry
    geom <- sf::st_geometry(obj)
    sf::st_bbox(geom)
  }, error = function(e) {
    # If anything fails, return NA bbox
    sf::st_bbox(c(xmin = NA_real_, ymin = NA_real_, 
                  xmax = NA_real_, ymax = NA_real_), 
                crs = crs)
  })
}

#' Collect and materialize a duckspatial_df
#'
#' Materializes a lazy \code{duckspatial_df} object by executing the underlying
#' DuckDB query. Supports multiple output formats.
#'
#' @param x A \code{duckspatial_df} object
#' @param ... Additional arguments passed to \code{collect}
#' @param as Output format. One of:
#'   \describe{
#'     \item{\code{"sf"}}{(Default) Returns an \code{sf} object with \code{sfc} geometry}
#'     \item{\code{"tibble"}}{Returns a tibble with geometry column dropped (fastest)}
#'     \item{\code{"raw"}}{Returns a tibble with geometry as raw WKB bytes}
#'     \item{\code{"geoarrow"}}{Returns a tibble with geometry as \code{geoarrow_vctr}}
#'   }
#'
#' @returns Data in the specified format
#' @export
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#'
#' # Load lazy spatial data
#' nc <- ddbs_open_dataset(system.file("shape/nc.shp", package = "sf"))
#'
#' # Perform lazy operations
#' result <- nc |> dplyr::filter(AREA > 0.1)
#'
#' # Collect to sf (default)
#' result_sf <- ddbs_collect(result)
#' plot(result_sf["AREA"])
#'
#' # Collect as tibble without geometry (fast)
#' result_tbl <- ddbs_collect(result, as = "tibble")
#'
#' # Collect with raw WKB bytes
#' result_raw <- ddbs_collect(result, as = "raw")
#'
#' # Collect as geoarrow for Arrow workflows
#' result_ga <- ddbs_collect(result, as = "geoarrow")
#' }
ddbs_collect <- function(x, ..., as = c("sf", "tibble", "raw", "geoarrow")) {
  if (!inherits(x, "duckspatial_df")) {
    cli::cli_abort("{.arg x} must be a {.cls duckspatial_df} object.")
  }
  as <- match.arg(as)
  dplyr::collect(x, ..., as = as)
}

#' @export
#' @importFrom sf st_as_sf
st_as_sf.duckspatial_df <- function(x, ...) {
  # st_as_sf always returns sf, ignore any as= argument
  dplyr::collect(x, ..., as = "sf")
}

#' @export
print.duckspatial_df <- function(x, ..., n = 10) {
  geom_col <- attr(x, "sf_column") %||% "geometry"
  crs <- st_crs(x)
  
  cat("# A duckspatial lazy spatial table\n")
  cat("# CRS:", format(crs$input), "\n")
  cat("# Geometry column:", geom_col, "\n")
  cat("#\n")
  cat("# Data backed by DuckDB (dbplyr lazy evaluation)\n")
  cat("# Use ddbs_collect() or st_as_sf() to materialize to sf.\n")
  cat("#\n")
  
  # Print a preview using standard print
  # We need to call the parent class print
  tryCatch({
    # Get preview without triggering full materialization
    # dbplyr handles this well
    class(x) <- setdiff(class(x), "duckspatial_df")
    print(x, n = n)
  }, error = function(e) {
    cat("# (Preview unavailable)\n")
  })
  
  invisible(x)
}

#' Get the geometry column name
#' @param x A duckspatial_df object
#' @return Character string with geometry column name
#' @export
ddbs_geom_col <- function(x) {
  if (!is_duckspatial_df(x)) {
    if (inherits(x, "sf")) {
      return(attr(x, "sf_column"))
    }
    cli::cli_abort("{.arg x} must be a duckspatial_df or sf object.")
  }
  attr(x, "sf_column") %||% "geometry"
}

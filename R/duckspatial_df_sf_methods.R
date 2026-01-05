#' sf methods for duckspatial_df
#'
#' These methods provide sf compatibility for duckspatial_df objects,
#' allowing them to work with sf functions like st_crs(), st_geometry(), etc.
#'
#' @name duckspatial_df_sf
#' @keywords internal
NULL

#' @rdname duckspatial_df_sf
#' @export
#' @importFrom sf st_crs
st_crs.duckspatial_df <- function(x, ...) {
  attr(x, "crs") %||% sf::st_crs(NA)
}

#' @rdname duckspatial_df_sf
#' @export
#' @importFrom sf st_geometry
st_geometry.duckspatial_df <- function(obj, ...) {
  # For duckspatial_df, st_geometry is a materialization point.
  # We delegate to st_as_sf to ensure we use the robust collect() path
  # which handles ST_AsWKB conversion correctly.
  sf::st_geometry(sf::st_as_sf(obj, ...))
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

#' @rdname duckspatial_df_sf
#' @export
#' @importFrom sf st_bbox
st_bbox.duckspatial_df <- function(obj, ...) {
  geom_col <- attr(obj, "sf_column") %||% "geom"
  crs_obj <- st_crs(obj)
  
  # Try to use DuckDB's ST_Extent for efficiency
  # We use the lazy table directly
  tryCatch({
    conn <- dbplyr::remote_con(obj)
    query_sql <- dbplyr::sql_render(obj)
    
    # We construct the extent query
    # ST_Extent returns the MBR of all geometries in the table/query
    extent_query <- glue::glue(
      "SELECT 
        ST_XMin(ext) as xmin, 
        ST_YMin(ext) as ymin, 
        ST_XMax(ext) as xmax, 
        ST_YMax(ext) as ymax 
       FROM (SELECT ST_Extent(ST_Collect(LIST({DBI::dbQuoteIdentifier(conn, geom_col)}))) as ext FROM ({query_sql}))"
    )
    
    res <- DBI::dbGetQuery(conn, extent_query)
    
    if (nrow(res) > 0 && !is.na(res$xmin[1])) {
      return(sf::st_bbox(c(
        xmin = res$xmin[1], 
        ymin = res$ymin[1], 
        xmax = res$xmax[1], 
        ymax = res$ymax[1]
      ), crs = crs_obj))
    }
    
    # Empty result or all NAs
    sf::st_bbox(c(xmin = NA_real_, ymin = NA_real_, 
                  xmax = NA_real_, ymax = NA_real_), 
                crs = crs_obj)
  }, error = function(e) {
    # If anything fails, return NA bbox
    sf::st_bbox(c(xmin = NA_real_, ymin = NA_real_, 
                  xmax = NA_real_, ymax = NA_real_), 
                crs = crs_obj)
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

#' Force computation of a lazy duckspatial_df
#'
#' Executes the accumulated query and stores the result in a DuckDB temporary
#' table. The result remains lazy (a \code{duckspatial_df}) but points to the
#' materialized data, avoiding repeated computation of complex query plans.
#'
#' This is useful when you want to:
#' \itemize{
#'   \item Cache intermediate results for reuse across multiple subsequent operations
#'   \item Simplify complex query plans before heavy operations like spatial joins
#'   \item Force execution at a specific point without pulling data into R memory
#' }
#'
#' @param x A \code{duckspatial_df} object
#' @param ... Additional arguments passed to \code{dplyr::compute}
#' @param name Optional name for the result table. If NULL, a unique temporary
#'   name is generated.
#' @param temporary If TRUE (default), creates a temporary table that is
#'   automatically cleaned up when the connection closes.
#'
#' @returns A new \code{duckspatial_df} pointing to the materialized table
#' @export
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#' library(dplyr)
#'
#' # Load lazy spatial data
#' countries <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", package = "duckspatial")
#' )
#'
#' # Complex pipeline - ddbs_compute() caches intermediate result
#' cached <- countries |>
#'   filter(CNTR_ID %in% c("DE", "FR", "IT")) |>
#'   ddbs_compute()  # Execute and store in temp table
#'
#' # Check query plan - should reference temp table
#' show_query(cached)
#'
#' # Further operations continue from cached result
#' result <- cached |>
#'   ddbs_filter(other_layer, predicate = "intersects") |>
#'   st_as_sf()
#' }
ddbs_compute <- function(x, ..., name = NULL, temporary = TRUE) {
  if (!inherits(x, "duckspatial_df")) {
    cli::cli_abort("{.arg x} must be a {.cls duckspatial_df} object.")
  }
  dplyr::compute(x, name = name, temporary = temporary, ...)
}

#' @rdname duckspatial_df_sf
#' @export
#' @importFrom sf st_as_sf
st_as_sf.duckspatial_df <- function(x, ...) {
  # st_as_sf always returns sf, ignore any as= argument
  dplyr::collect(x, ..., as = "sf")
}

#' @rdname duckspatial_df_sf
#' @export
print.duckspatial_df <- function(x, ..., n = 10) {
  geom_col <- attr(x, "sf_column") %||% "geom"
  crs <- st_crs(x)
  
  cat("# A duckspatial lazy spatial table\n")
  cat("# CRS:", ddbs_format_crs(crs), "\n")
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

#' Format a CRS object compactly for printing
#' @param crs An sf crs object
#' @return A character string
#' @keywords internal
ddbs_format_crs <- function(crs) {
  if (is.na(crs)) return("NA")
  
  # 1. If EPSG code is available, use it (shortest, most recognizable)
  if (!is.null(crs$epsg) && !is.na(crs$epsg)) {
    return(paste0("EPSG:", crs$epsg))
  }
  
  # 2. Try to extract ID["Authority", "Code"] from WKT if EPSG is missing
  # This often covers well-known CRSs that aren't fully resolved in the object
  wkt <- crs$wkt
  if (!is.null(wkt)) {
    # Match ID at the end of the WKT block, e.g. ID["OGC","CRS84"]]
    id_match <- regmatches(wkt, regexec('ID\\["([A-Za-z0-9_]+)",\\s*"?([A-Za-z0-9_]+)"?\\]\\]$', wkt))
    if (length(id_match[[1]]) == 3) {
      return(paste0(id_match[[1]][2], ":", id_match[[1]][3]))
    }
  }

  # 3. Check for OGC:CRS84 specifically if it wasn't caught
  if (!is.null(crs$input) && crs$input == "OGC:CRS84") return("OGC:CRS84")

  # 4. If input is short and not JSON, use it
  input <- crs$input
  if (!is.null(input) && nchar(input) > 0 && nchar(input) < 50 && !grepl("^\\{", input)) {
    return(input)
  }
  
  # 5. Fallback to Name if available
  if (!is.null(crs$Name) && nchar(crs$Name) > 0) return(crs$Name)
  
  # 6. Absolute fallback: truncated input or just 'custom'
  if (is.null(input)) return("unknown")
  if (nchar(input) > 50) return(paste0(substr(input, 1, 47), "..."))
  input
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
  attr(x, "sf_column") %||% "geom"
}

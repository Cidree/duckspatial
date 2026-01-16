#' @param output Character. Controls the return type. Options:
#'   \itemize{
#'     \item \code{"duckspatial_df"} (default): Lazy spatial data frame backed by dbplyr/DuckDB
#'     \item \code{"sf"}: Eagerly collected sf object (uses memory)
#'     \item \code{"tibble"}: Eagerly collected tibble without geometry
#'     \item \code{"raw"}: Eagerly collected tibble with WKB geometry (list of raw vectors)
#'     \item \code{"geoarrow"}: Eagerly collected tibble with geoarrow geometry (geoarrow_vctr)
#'   }
#'   Can be set globally via \code{\link{ddbs_options}(output_type = "...")} or
#'   per-function via this argument. Per-function overrides global setting.

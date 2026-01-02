#' @param output Character. Controls the return type. Options:
#'   \itemize{
#'     \item \code{"duckspatial_df"} (default): Lazy spatial data frame backed by dbplyr/DuckDB
#'     \item \code{"sf"}: Eagerly collected sf object (uses memory)
#'     \item \code{"tibble"}: Eagerly collected tibble without geometry
#'   }
#'   Can be set globally via \code{options(duckspatial.output_type = "...")} or
#'   per-function via this argument. Per-function overrides global setting.
#'   See \code{\link{ddbs_options}} for details.

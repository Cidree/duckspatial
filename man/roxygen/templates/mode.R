#' @param mode Character. Controls the return type. Options:
#'   \itemize{
#'     \item \code{"duckspatial"} (default): Lazy spatial data frame backed by dbplyr/DuckDB
#'     \item \code{"sf"}: Eagerly collected sf object (uses memory)
#'   }
#'   Can be set globally via \code{\link{ddbs_options}(mode = "...")} or
#'   per-function via this argument. Per-function overrides global setting.

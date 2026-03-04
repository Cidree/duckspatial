#' @returns Depends on the \code{mode} argument (or global preference set by \code{\link{ddbs_options}}):
#' \itemize{
#'   \item \code{duckspatial} (default): A \code{duckspatial_df} (lazy spatial data frame) backed by dbplyr/DuckDB.
#'   \item \code{sf}: An eagerly collected object in R memory, that will return the same data type as the
#' \code{sf} equivalent (e.g. `sf` or `units` vector).
#' }
#' When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).

#' @param duckdb_storage_version Storage compatibility for newly created persistent
#'   native DuckDB files (\code{.duckdb}, \code{.db}, \code{.ddb}). See
#'   <https://duckdb.org/docs/2.0/internals/storage> for more information on
#'   DuckDB storage versions and compatibility.
#'   \itemize{
#'     \item \code{"v2.0.0"} (\strong{Default, Modern Storage}): Introduces lazy
#'           column metadata loading, enables \code{DICT_FSST} string compression
#'           by default, stores row deletions more compactly, and performs
#'           stronger corruption validation. Requires DuckDB >= 2.0.0 to open
#'           the file.
#'     \item \code{"v1.5.0"} (\strong{Native Spatial Storage}): Preserves
#'           CRS metadata in native DuckDB \code{GEOMETRY} columns. Requires
#'           DuckDB >= 1.5.0 to open the file.
#'     \item \code{"v1.0.0"} (\strong{Legacy Compatibility}): Creates
#'           files readable by older DuckDB versions (>= 1.0.0). Persists CRS
#'           metadata in duckspatial-managed column comments (a convention not
#'           recognized by other spatial software).
#'     \item \code{"latest"}: Use the highest storage version supported by your
#'           installed DuckDB engine.
#'   }
#'   Other major version strings like \code{"v1.4.0"}, \code{"v1.3.0"}, etc., are also supported.

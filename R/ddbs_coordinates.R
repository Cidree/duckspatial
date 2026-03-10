#' Extract X and Y coordinates from geometries
#'
#' `ddbs_x()` extracts the X coordinate (longitude) and `ddbs_y()` extracts 
#' the Y coordinate (latitude) from point geometries, adding them as a new 
#' column to the dataset.
#'
#' @template x
#' @template conn_null
#' @template name
#' @param new_column Name of the new column to store the extracted coordinate.
#'   Defaults to `"X"` for `ddbs_x()` and `"Y"` for `ddbs_y()`.
#' @template crs
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' 
#' @name ddbs_xy
#' @rdname ddbs_xy
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## extract coordinates without using a connection
#' ddbs_x(argentina_ddbs)
#' ddbs_y(argentina_ddbs)
#' }
NULL




#' @rdname ddbs_xy
#' @export
ddbs_x <- function(
  x,
  conn = NULL,
  name = NULL,
  new_column = "X",
  crs = NULL,
  crs_column = "crs_duckspatial",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    conn = conn,
    name = name,
    new_column = new_column,
    crs = crs,
    crs_column = crs_column,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_X"
  )
  
}



#' @rdname ddbs_xy
#' @export
ddbs_y <- function(
  x,
  conn = NULL,
  name = NULL,
  new_column = "X",
  crs = NULL,
  crs_column = "crs_duckspatial",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    conn = conn,
    name = name,
    new_column = new_column,
    crs = crs,
    crs_column = crs_column,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_Y"
  )
  
}
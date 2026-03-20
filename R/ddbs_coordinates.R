#' Extract X and Y coordinates from geometries
#'
#' `ddbs_x()` extracts the X coordinate (longitude) and `ddbs_y()` extracts 
#' the Y coordinate (latitude) from point geometries, adding them as a new 
#' column to the dataset.
#'
#' @template x
#' @param new_column Name of the new column to store the extracted coordinate.
#'   Defaults to `"X"` for `ddbs_x()` and `"Y"` for `ddbs_y()`.
#' @template conn_null
#' @template name
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
  new_column = "X",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    new_column = new_column,
    conn = conn,
    name = name,
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
  new_column = "Y",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    new_column = new_column,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_Y"
  )
  
}
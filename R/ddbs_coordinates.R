#' Extract coordinates from geometries
#'
#' Extracts the X, Y, M, or Z coordinates from `POINT` geometries
#'
#' @template x
#' @param new_column Name of the new column to store the extracted coordinate.
#'   Defaults to `"X"`, `"Y"`, `"M"`, or `"Z"`.
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' 
#' @details
#' - `ddbs_x()`: Extracts the X coordinate (longitude).
#' - `ddbs_y()`: Extracts the Y coordinate (latitude).
#' - `ddbs_m()`: Extracts the M coordinate (measure).
#' - `ddbs_z()`: Extracts the Z coordinate (elevation). 
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
#' ## extract coordinates
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



#' @rdname ddbs_xy
#' @export
ddbs_m <- function(
  x,
  new_column = "M",
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
    fun = "ST_M"
  )
  
}


#' @rdname ddbs_xy
#' @export
ddbs_z <- function(
  x,
  new_column = "Z",
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
    fun = "ST_Z"
  )
  
}

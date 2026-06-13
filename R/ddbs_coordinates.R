## Coordinates:
## - ddbs_x, ddbs_y, ddbs_z, ddbs_m (point coordinates)
## - ddbs_xmax, ddbs_xmin, ddbs_ymax, ddbs_ymin,
##   ddbs_zmax, ddbs_zmin, ddbs_mmax, ddbs_mmin (coordinate bounds)


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




#' Coordinate bounds of geometries
#'
#' Returns the minimum or maximum value of a specific coordinate axis across
#' all points of a geometry. When \code{by_feature = TRUE} (default), a value
#' is computed per row. When \code{by_feature = FALSE}, a single global
#' value is returned for the entire dataset.
#'
#' @template x
#' @param new_column Name of the new column. Defaults to the lowercase function
#'   name (e.g. \code{"xmax"}, \code{"xmin"}, \code{"ymax"}, \ldots).
#' @template by_feature
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @details
#' - `ddbs_xmax()` / `ddbs_xmin()`: maximum / minimum X coordinate.
#' - `ddbs_ymax()` / `ddbs_ymin()`: maximum / minimum Y coordinate.
#' - `ddbs_zmax()` / `ddbs_zmin()`: maximum / minimum Z coordinate.
#' - `ddbs_mmax()` / `ddbs_mmin()`: maximum / minimum M (measure) coordinate.
#'
#' When \code{by_feature = FALSE}, the result is always a single \code{numeric}
#' scalar representing the global extreme across the entire dataset.
#'
#' @returns
#' \itemize{
#'   \item \code{by_feature = TRUE} and \code{mode = "duckspatial"} (default):
#'     A \code{duckspatial_df}.
#'   \item \code{by_feature = TRUE} and \code{mode = "sf"}: A numeric vector.
#'   \item \code{by_feature = FALSE}: A single \code{numeric} scalar.
#'   \item When \code{name} is provided: writes the table in DuckDB and
#'     returns \code{TRUE} (invisibly).
#' }
#'
#' @name ddbs_coord_bounds
#' @rdname ddbs_coord_bounds
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#'
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", package = "duckspatial")
#' )
#'
#' ## per-feature X extent (default)
#' ddbs_xmax(argentina_ddbs)
#' ddbs_xmin(argentina_ddbs)
#'
#' ## global bounding values
#' ddbs_xmax(argentina_ddbs, by_feature = FALSE)
#' ddbs_ymin(argentina_ddbs, by_feature = FALSE)
#' }
NULL


#' @rdname ddbs_coord_bounds
#' @export
ddbs_xmax <- function(
    x,
    new_column = "xmax",
    by_feature = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  template_new_column(
    x = x,
    new_column = new_column,
    by_feature = by_feature,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_XMax"
  )
}


#' @rdname ddbs_coord_bounds
#' @export
ddbs_xmin <- function(
    x,
    new_column = "xmin",
    by_feature = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  template_new_column(
    x = x,
    new_column = new_column,
    by_feature = by_feature,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_XMin"
  )
}


#' @rdname ddbs_coord_bounds
#' @export
ddbs_ymax <- function(
    x,
    new_column = "ymax",
    by_feature = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  template_new_column(
    x = x,
    new_column = new_column,
    by_feature = by_feature,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_YMax"
  )
}


#' @rdname ddbs_coord_bounds
#' @export
ddbs_ymin <- function(
    x,
    new_column = "ymin",
    by_feature = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  template_new_column(
    x = x,
    new_column = new_column,
    by_feature = by_feature,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_YMin"
  )
}


#' @rdname ddbs_coord_bounds
#' @export
ddbs_zmax <- function(
    x,
    new_column = "zmax",
    by_feature = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  template_new_column(
    x = x,
    new_column = new_column,
    by_feature = by_feature,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_ZMax"
  )
}


#' @rdname ddbs_coord_bounds
#' @export
ddbs_zmin <- function(
    x,
    new_column = "zmin",
    by_feature = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  template_new_column(
    x = x,
    new_column = new_column,
    by_feature = by_feature,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_ZMin"
  )
}


#' @rdname ddbs_coord_bounds
#' @export
ddbs_mmax <- function(
    x,
    new_column = "mmax",
    by_feature = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  template_new_column(
    x = x,
    new_column = new_column,
    by_feature = by_feature,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_MMax"
  )
}


#' @rdname ddbs_coord_bounds
#' @export
ddbs_mmin <- function(
    x,
    new_column = "mmin",
    by_feature = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  template_new_column(
    x = x,
    new_column = new_column,
    by_feature = by_feature,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_MMin"
  )
}

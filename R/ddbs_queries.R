#' Count geometry components
#'
#' Functions to count the number of points or sub-geometries in a geometry
#'
#' @template x
#' @template new_column
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @details
#' These functions query structural properties of geometries:
#'
#' - `ddbs_get_npoints()` returns the number of points (vertices) in a geometry.
#'   For LINESTRING geometries this is the vertex count; for POLYGON types it
#'   includes all vertices of the exterior ring and any interior rings.
#'
#' - `ddbs_get_ngeometries()` returns the number of sub-geometries in a
#'   GEOMETRYCOLLECTION or MULTI* geometry (e.g. MULTIPOLYGON, MULTILINESTRING).
#'   Returns 1 for simple (non-collection) geometry types.
#'
#' @template returns_mode
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## count points and sub-geometries
#' ddbs_get_npoints(countries_ddbs)
#' ddbs_get_ngeometries(countries_ddbs)
#' }
#'
#' @name ddbs_get_npoints
#' @rdname ddbs_get_npoints
NULL



#' @rdname ddbs_get_npoints
#' @export
ddbs_get_npoints <- function(
  x,
  new_column = "npoints",
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
    fun = "ST_NumPoints"
  )

}



#' @rdname ddbs_get_npoints
#' @export
ddbs_get_ngeometries <- function(
  x,
  new_column = "ngeometries",
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
    fun = "ST_NumGeometries"
  )

}




#' Get the topological dimension of a geometry
#'
#' Returns the topological dimension of each geometry: 0 for points, 1 for
#' lines, 2 for polygons, and -1 for empty geometries.
#'
#' @template x
#' @template new_column
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## get topological dimension (2 for polygons)
#' ddbs_dimension(countries_ddbs)
#' }
#'
#' @export
ddbs_dimension <- function(
  x,
  new_column = "dimension",
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
    fun = "ST_Dimension"
  )

}

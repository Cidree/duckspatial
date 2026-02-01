
#' Geometry validation functions
#'
#' Functions to check various geometric properties and validity conditions of spatial
#' geometries using DuckDB's spatial extension.
#' 
#' @template x
#' @template conn_null
#' @template crs
#' @template quiet
#'
#' @details
#' These functions provide different types of geometric validation:
#'
#' - `ddbs_is_valid()` checks if a geometry is valid according to the OGC Simple Features
#'   specification. Invalid geometries may have issues like self-intersections in polygons,
#'   duplicate points, or incorrect ring orientations.
#'
#' - `ddbs_is_simple()` determines whether geometries are simple, meaning they are free of
#'   self-intersections. For example, a linestring that crosses itself is not simple.
#'
#' - `ddbs_is_ring()` checks if a linestring geometry is closed (first and last points are
#'   identical) and simple (no self-intersections), forming a valid ring.
#'
#' - `ddbs_is_empty()` tests whether a geometry is empty, containing no points. Empty
#'   geometries are valid but represent the absence of spatial information.
#'
#' - `ddbs_is_closed()` determines if a linestring geometry is closed, meaning the first
#'   and last coordinates are identical. Unlike `ddbs_is_ring()`, this does not check for
#'   simplicity.
#' 
#' @returns A logical vector
#' 
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## geometry validation
#' ddbs_is_valid(countries_ddbs)
#' ddbs_is_simple(countries_ddbs)
#' ddbs_is_ring(rivers_ddbs)
#' ddbs_is_empty(countries_ddbs)
#' ddbs_is_closed(countries_ddbs)
#' 
#' }
#'
#' @name ddbs_geom_validation_funs
#' @rdname ddbs_geom_validation_funs
NULL




#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_simple <- function(
  x,
  conn = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE) {
  
  template_geometry_validation(
    x = x,
    conn = conn,
    crs = crs,
    crs_column = crs_column,
    quiet = quiet,
    fun = "ST_IsSimple"
  )
  
}





#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_valid <- function(
  x,
  conn = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE) {
  
  template_geometry_validation(
    x = x,
    conn = conn,
    crs = crs,
    crs_column = crs_column,
    quiet = quiet,
    fun = "ST_IsValid"
  )
  
}




#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_closed <- function(
  x,
  conn = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE) {
  
  template_geometry_validation(
    x = x,
    conn = conn,
    crs = crs,
    crs_column = crs_column,
    quiet = quiet,
    fun = "ST_IsClosed"
  )
  
}




#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_empty <- function(
  x,
  conn = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE) {
  
  template_geometry_validation(
    x = x,
    conn = conn,
    crs = crs,
    crs_column = crs_column,
    quiet = quiet,
    fun = "ST_IsEmpty"
  )
  
}





#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_ring <- function(
  x,
  conn = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE) {
  
  template_geometry_validation(
    x = x,
    conn = conn,
    crs = crs,
    crs_column = crs_column,
    quiet = quiet,
    fun = "ST_IsRing"
  )
  
}
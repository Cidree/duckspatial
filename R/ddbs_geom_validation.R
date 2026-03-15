
#' Geometry validation functions
#'
#' Functions to check various geometric properties and validity conditions of spatial
#' geometries using DuckDB's spatial extension.
#' 
#' @template x
#' @template conn_null
#' @template name
#' @template new_column
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @details
#' These functions provide different types of geometric validation. Note that by default,
#' the functions add a new column as a logical vector. This behaviour allows to filter the
#' data within DuckDB without the need or materializating a vector in R (see details).
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
#' @returns
#' \itemize{
#'   \item \code{mode = "duckspatial"} (default): A \code{duckspatial_df} (lazy spatial data frame) backed by dbplyr/DuckDB.
#'   \item \code{mode = "sf"}: An eagerly collected vector in R memory.
#'   \item When \code{name} is provided: writes the table in the DuckDB connection and returns \code{TRUE} (invisibly).
#' }
#' 
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#' library(dplyr)
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
#' ## filter invalid countries
#' ddbs_is_valid(countries_ddbs) |> filter(!is_valid)
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
  name = NULL,
  new_column = "is_simple",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    conn = conn,
    name = name,
    new_column = new_column,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_IsSimple"
  )
  
}





#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_valid <- function(
  x,
  conn = NULL,
  name = NULL,
  new_column = "is_valid",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    conn = conn,
    name = name,
    new_column = new_column,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_IsValid"
  )
  
}




#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_closed <- function(
  x,
  conn = NULL,
  name = NULL,
  new_column = "is_closed",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    conn = conn,
    name = name,
    new_column = new_column,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_IsClosed"
  )
  
}




#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_empty <- function(
  x,
  conn = NULL,
  name = NULL,
  new_column = "is_empty",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    conn = conn,
    name = name,
    new_column = new_column,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_IsEmpty"
  )
  
}





#' @rdname ddbs_geom_validation_funs
#' @export
ddbs_is_ring <- function(
  x,
  conn = NULL,
  name = NULL,
  new_column = "is_ring",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_new_column(
    x = x,
    conn = conn,
    name = name,
    new_column = new_column,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_IsRing"
  )
  
}
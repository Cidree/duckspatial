#' Check geometry dimensions
#'
#' Functions to check whether geometries have Z (elevation) or M (measure) dimensions
#' 
#' @template x
#' @template by_feature
#' @template conn_null
#' @template quiet
#'
#' @details
#' These functions check for additional coordinate dimensions beyond X and Y:
#'
#' - `ddbs_has_z()` checks if a geometry has Z coordinates (elevation/altitude values).
#'   Geometries with Z dimension are often referred to as 3D geometries and have
#'   coordinates in the form (X, Y, Z).
#'
#' - `ddbs_has_m()` checks if a geometry has M coordinates (measure values). The M
#'   dimension typically represents a measurement along the geometry, such as distance
#'   or time, and results in coordinates of the form (X, Y, M) or (X, Y, Z, M).
#'
#' @returns A logical vector
#' @examples
#' \dontrun{
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' 
#' ## create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' ) |> 
#'   filter(ISO3_CODE != "ATA")
#' 
#' ## check if it has Z or M
#' ddbs_has_m(countries_ddbs)
#' ddbs_has_z(countries_ddbs)
#' }
#'
#' @name ddbs_has_dim
#' @rdname ddbs_has_dim
NULL






#' @rdname ddbs_has_dim
#' @export
ddbs_has_z <- function(
  x,
  by_feature = FALSE,
  conn = NULL,
  quiet = FALSE) {
  
  template_has(    
    x = x,
    by_feature = by_feature,
    conn = conn,
    quiet = quiet,
    fun = "ST_HasZ"
  )

}




#' @rdname ddbs_has_dim
#' @export
ddbs_has_m <- function(
  x,
  by_feature = FALSE,
  conn = NULL,
  quiet = FALSE) {
  
  template_has(    
    x = x,
    by_feature = by_feature,
    conn = conn,
    quiet = quiet,
    fun = "ST_HasM"
  )

}







#' Force geometry dimensions
#'
#' Functions to force geometries to have specific coordinate dimensions (X, Y, Z, M)
#' 
#' @template x
#' @param var A numeric variable in `x` to be converted in the dimension specified
#' in the argument `dim`
#' @param var_z A numeric variable in `x` to be convered in `Z` dimension
#' @param var_m A numeric variable in `x` to be convered in `M` dimension
#' @param dim The dimension to add: either `"z"` (default) for
#' elevation or `"m"` for measure values.
#' @template conn_null
#' @template name
#' @template crs
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @details
#' These functions modify the dimensionality of geometries:
#'
#' - `ddbs_force_2d()` removes Z and M coordinates from geometries, returning only
#'   X and Y coordinates. This is useful for simplifying 3D or measured geometries
#'   to 2D.
#'
#' - `ddbs_force_3d()` forces geometries to have three dimensions. When `dim = "z"`
#'   (default), adds or retains Z coordinates (X, Y, Z). When `dim = "m"`, adds or
#'   retains M coordinates (X, Y, M). Missing values are typically set to 0. If the
#'   input geometry has a third dimension already, it will be replaced by the new one.
#'   If the input geometry has 4 dimensions, it will drop the dimension that wasn't 
#'   specified.
#'
#' - `ddbs_force_4d()` forces geometries to have all four dimensions (X, Y, Z, M).
#'   Missing Z or M values are typically set to 0.
#'
#' @template returns_mode
#' 
#' @examples
#' \dontrun{
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' 
#' ## load data and add 2 numeric vars
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' ) |> 
#'   dplyr::filter(ISO3_CODE != "ATA") |> 
#'   ddbs_area(new_column = "area") |> 
#'   ddbs_perimeter(new_column = "perim") 
#' 
#' ## add a Z dimension
#' countries_z_ddbs <- ddbs_force_3d(countries_ddbs, "area")
#' ddbs_has_z(countries_z_ddbs)
#' 
#' ## add a M dimension as 3D (removes current Z)
#' countries_m_ddbs <- ddbs_force_3d(countries_z_ddbs, "area", "m")
#' ddbs_has_z(countries_m_ddbs)
#' ddbs_has_m(countries_m_ddbs)
#' 
#' ## add both Z and M
#' countries_zm_ddbs <- ddbs_force_4d(countries_ddbs, "area", "perim")
#' ddbs_has_z(countries_zm_ddbs)
#' ddbs_has_m(countries_zm_ddbs)
#' 
#' ## drop both ZM
#' countries_drop_ddbs <- ddbs_force_2d(countries_zm_ddbs)
#' ddbs_has_z(countries_drop_ddbs)
#' ddbs_has_m(countries_drop_ddbs)
#' }
#'
#' @name ddbs_force_dim
#' @rdname ddbs_force_dim
NULL





#' @rdname ddbs_force_dim
#' @export
ddbs_force_2d <- function(
  x,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  
  template_unary_ops(
      x = x,
      conn = conn,
      name = name,
      crs = crs,
      crs_column = crs_column,
      mode = mode,
      overwrite = overwrite,
      quiet = quiet,
      fun = "ST_Force2D",
      other_args = NULL
  )
  
}





#' @rdname ddbs_force_dim
#' @export
ddbs_force_3d <- function(
  x,
  var,
  dim = "z",
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  # 0. Handle function-specific errors
  if (!tolower(dim) %in% c("m", "z")) cli::cli_abort("{.arg dim} must be {.val Z} or {.val M}.")
  
  # 1. Function to use
  st_fun <- glue::glue("ST_Force3D{dim}")

  
  template_unary_ops(
      x = x,
      conn = conn,
      name = name,
      crs = crs,
      crs_column = crs_column,
      mode = mode,
      overwrite = overwrite,
      quiet = quiet,
      fun = st_fun,
      other_args = var
  )
  
}





#' @rdname ddbs_force_dim
#' @export
ddbs_force_4d <- function(
  x,
  var_z,
  var_m,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  

  # 1. Build ST_Force4D parameters string
  force_args <- glue::glue("{var_z}, {var_m}")

  
  # 2. Pass to template
  template_unary_ops(
      x = x,
      conn = conn,
      name = name,
      crs = crs,
      crs_column = crs_column,
      mode = mode,
      overwrite = overwrite,
      quiet = quiet,
      fun = "ST_Force4D",
      other_args = force_args
  )
  
}

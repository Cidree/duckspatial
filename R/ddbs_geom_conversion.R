#' Convert geometries to standard interchange formats
#'
#' @description
#' Convert spatial geometries to common interchange formats using DuckDB spatial
#' serialization functions.
#'
#' * `ddbs_as_text()` – Convert geometries to Well-Known Text (WKT)
#' * `ddbs_as_wkb()` – Convert geometries to Well-Known Binary (WKB)
#' * `ddbs_as_hexwkb()` – Convert geometries to hexadecimal Well-Known Binary (HEXWKB)
#' * `ddbs_as_geojson()` – Convert geometries to GeoJSON
#'
#' @template x
#' @template conn_null
#'
#' @details
#' These functions are thin wrappers around DuckDB spatial serialization
#' functions (`ST_AsText`, `ST_AsWKB`, `ST_AsHEXWKB`, and `ST_AsGeoJSON`).
#'
#' They are useful for exporting geometries into widely supported formats for
#' interoperability with external spatial tools, databases, and web services.
#'
#' @return
#' Depending on the function:
#' \itemize{
#'   \item \code{ddbs_as_text()} returns a character vector of WKT geometries
#'   \item \code{ddbs_as_wkb()} returns a list of raw vectors (binary WKB)
#'   \item \code{ddbs_as_hexwkb()} returns a character vector of HEXWKB strings
#'   \item \code{ddbs_as_geojson()} returns a character vector of GeoJSON strings
#' }
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#'
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", package = "duckspatial")
#' )
#'
#' ddbs_as_text(argentina_ddbs)
#' ddbs_as_wkb(argentina_ddbs)
#' ddbs_as_hexwkb(argentina_ddbs)
#' ddbs_as_geojson(argentina_ddbs)
#' }
#'
#' @name ddbs_as_format
#' @rdname ddbs_as_format
NULL





#' @rdname ddbs_as_format
#' @export
ddbs_as_text <- function(
  x,
  conn = NULL) {

  template_geometry_conversion(
    x = x,
    conn = conn,
    fun = "ST_AsText"
  )

}





#' @rdname ddbs_as_format
#' @export
ddbs_as_wkb <- function(
  x,
  conn = NULL) {

  template_geometry_conversion(
    x = x,
    conn = conn,
    fun = "ST_AsWKB"
  )

}





#' @rdname ddbs_as_format
#' @export
ddbs_as_hexwkb <- function(
  x,
  conn = NULL) {

  template_geometry_conversion(
    x = x,
    conn = conn,
    fun = "ST_AsHEXWKB"
  )

}





#' @rdname ddbs_as_format
#' @export
ddbs_as_geojson <- function(
  x,
  conn = NULL) {

  template_geometry_conversion(
    x = x,
    conn = conn,
    fun = "ST_AsGeoJSON"
  )

}




#' Create geometries from standard interchange formats
#'
#' @description
#' Parse serialized geometries from common interchange formats into a spatial
#' object, using DuckDB spatial deserialization functions. These are the
#' inverses of the [ddbs_as_text()] / [ddbs_as_wkb()] / [ddbs_as_hexwkb()] /
#' [ddbs_as_geojson()] serializers.
#'
#' * `ddbs_geom_from_text()` – Parse Well-Known Text (WKT)
#' * `ddbs_geom_from_wkb()` – Parse Well-Known Binary (WKB)
#' * `ddbs_geom_from_hexwkb()` – Parse hexadecimal Well-Known Binary (HEXWKB)
#' * `ddbs_geom_from_hexewkb()` – Parse hexadecimal Extended Well-Known Binary (HEXEWKB)
#' * `ddbs_geom_from_geojson()` – Parse GeoJSON
#'
#' @param x For `ddbs_geom_from_wkb()`, a list of raw vectors (binary WKB). For
#'   all other functions, a character vector of serialized geometries.
#' @param crs Character or numeric CRS specification (e.g. `"EPSG:4326"` or
#'   `4326`) to assign to the resulting geometries. Defaults to `NULL` (no CRS
#'   assigned).
#' @param ... Named vectors of additional attribute columns to include in the
#'   output. Each must have the same length as `x`.
#' @param geom_col Name of the geometry column in the output. Defaults to
#'   `"geometry"`.
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @details
#' These functions are thin wrappers around the DuckDB spatial deserialization
#' functions (`ST_GeomFromText`, `ST_GeomFromWKB`, `ST_GeomFromHEXWKB`,
#' `ST_GeomFromHEXEWKB`, and `ST_GeomFromGeoJSON`).
#'
#' They are useful for importing geometries produced by external spatial tools,
#' databases, and web services back into a `duckspatial` workflow.
#'
#' @template returns_mode
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#'
#' ## from WKT
#' ddbs_geom_from_text(
#'   c("POINT (-58.38 -34.60)", "POINT (-64.18 -31.42)"),
#'   crs = 4326
#' )
#'
#' ## from GeoJSON, with an extra attribute column
#' ddbs_geom_from_geojson(
#'   '{"type":"Point","coordinates":[-58.38,-34.60]}',
#'   city = "Buenos Aires",
#'   crs  = 4326
#' )
#' }
#'
#' @name ddbs_geom_from
#' @rdname ddbs_geom_from
NULL




#' @rdname ddbs_geom_from
#' @export
ddbs_geom_from_text <- function(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  template_geom_from(
    x = x,
    crs = crs,
    ...,
    geom_col = geom_col,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_GeomFromText",
    input = "character"
  )

}




#' @rdname ddbs_geom_from
#' @export
ddbs_geom_from_wkb <- function(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  template_geom_from(
    x = x,
    crs = crs,
    ...,
    geom_col = geom_col,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_GeomFromWKB",
    input = "blob"
  )

}




#' @rdname ddbs_geom_from
#' @export
ddbs_geom_from_hexwkb <- function(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  template_geom_from(
    x = x,
    crs = crs,
    ...,
    geom_col = geom_col,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_GeomFromHEXWKB",
    input = "character"
  )

}




#' @rdname ddbs_geom_from
#' @export
ddbs_geom_from_hexewkb <- function(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  template_geom_from(
    x = x,
    crs = crs,
    ...,
    geom_col = geom_col,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_GeomFromHEXEWKB",
    input = "character"
  )

}




#' @rdname ddbs_geom_from
#' @export
ddbs_geom_from_geojson <- function(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  template_geom_from(
    x = x,
    crs = crs,
    ...,
    geom_col = geom_col,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_GeomFromGeoJSON",
    input = "character"
  )

}

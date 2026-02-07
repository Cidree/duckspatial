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
#' @template quiet
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
  conn = NULL,
  quiet = FALSE) {

  template_geometry_conversion(
    x = x,
    conn = conn,
    quiet = quiet,
    fun = "ST_AsText"
  )

}





#' @rdname ddbs_as_format
#' @export
ddbs_as_wkb <- function(
  x,
  conn = NULL,
  quiet = FALSE) {

  template_geometry_conversion(
    x = x,
    conn = conn,
    quiet = quiet,
    fun = "ST_AsWKB"
  )

}





#' @rdname ddbs_as_format
#' @export
ddbs_as_hexwkb <- function(
  x,
  conn = NULL,
  quiet = FALSE) {

  template_geometry_conversion(
    x = x,
    conn = conn,
    quiet = quiet,
    fun = "ST_AsHEXWKB"
  )

}





#' @rdname ddbs_as_format
#' @export
ddbs_as_geojson <- function(
  x,
  conn = NULL,
  quiet = FALSE) {

  template_geometry_conversion(
    x = x,
    conn = conn,
    quiet = quiet,
    fun = "ST_AsGeoJSON"
  )

}

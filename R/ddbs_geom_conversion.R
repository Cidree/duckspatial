#' Convert geometries to standard interchange formats
#'
#' @description
#' Convert spatial geometries to common interchange formats using DuckDB spatial
#' serialization functions.
#'
#' * `ddbs_as_text()` – Convert geometries to Well-Known Text (WKT)
#' * `ddbs_as_wkb()` – Convert geometries to Well-Known Binary (WKB)
#' * `ddbs_as_hexwkb()` – Convert geometries to hexadecimal Well-Known Binary (HEXWKB)
#' * `ddbs_as_geojson()` – Convert to GeoJSON (a `FeatureCollection` or one `Feature` per row)
#'
#' @template x
#' @template conn_null
#' @param feature_collection Logical, only used by `ddbs_as_geojson()`. If `TRUE`
#'   (default), all rows are returned as a single GeoJSON `FeatureCollection`
#'   string. If `FALSE`, a character vector with one `Feature` string per row is
#'   returned.
#'
#' @details
#' These functions are thin wrappers around DuckDB spatial serialization
#' functions (`ST_AsText`, `ST_AsWKB`, `ST_AsHEXWKB`, and `ST_AsGeoJSON`).
#'
#' They are useful for exporting geometries into widely supported formats for
#' interoperability with external spatial tools, databases, and web services.
#'
#' Unlike the other serializers, which only encode the geometry, `ddbs_as_geojson()`
#' produces complete GeoJSON `Feature`s: the geometry is placed in the `geometry`
#' member and all remaining (non-geometry) columns are included as feature
#' `properties`. By default the features are wrapped in a single `FeatureCollection`,
#' the canonical GeoJSON artifact for writing `.geojson` files and feeding web
#' services and mapping libraries.
#'
#' @return
#' Depending on the function:
#' \itemize{
#'   \item \code{ddbs_as_text()} returns a character vector of WKT geometries
#'   \item \code{ddbs_as_wkb()} returns a list of raw vectors (binary WKB)
#'   \item \code{ddbs_as_hexwkb()} returns a character vector of HEXWKB strings
#'   \item \code{ddbs_as_geojson()} returns a single GeoJSON \code{FeatureCollection}
#'     string (class \code{"geojson"}) when \code{feature_collection = TRUE}, or a
#'     character vector of \code{Feature} strings (one per row) when \code{FALSE};
#'     non-geometry columns are included as feature properties
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
#'
#' ## a single FeatureCollection (default)
#' ddbs_as_geojson(argentina_ddbs)
#'
#' ## one Feature string per row
#' ddbs_as_geojson(argentina_ddbs, feature_collection = FALSE)
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
  conn = NULL,
  feature_collection = TRUE) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_character(conn, x)
  assert_logic(feature_collection, "feature_collection")

  # 1. Prepare inputs
  x <- normalize_spatial_input(x, conn)
  sf_col_x <- attr(x, "sf_column")

  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = TRUE)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  on.exit(resolve_conn$cleanup(), add = TRUE)

  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)

  # 2. Resolve the geometry column and the remaining (property) columns
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  desc       <- DBI::dbGetQuery(target_conn, glue::glue("DESCRIBE SELECT * FROM {x_list$query_name};"))
  prop_cols  <- desc$column_name[desc$column_name != x_geom]

  ## Per-row properties as compact JSON text. to_json() handles type formatting
  ## and string escaping; '{}' when there are no non-geometry columns.
  ## to_json() comes from the DuckDB `json` extension, which is only needed when
  ## there are non-geometry columns to encode as feature properties.
  if (length(prop_cols)) {
    if (isFALSE(check_loaded_extension(target_conn, "json"))) {
      cli::cli_abort(c(
        "{.fn ddbs_as_geojson} needs the DuckDB {.pkg json} extension to encode feature properties.",
        "i" = "Install it with {.code ddbs_install(conn, extension = \"json\")}, or drop the non-geometry columns first."
      ))
    }
    prop_fields <- paste(sprintf('"%s" := "%s"', prop_cols, prop_cols), collapse = ", ")
    props_sql   <- sprintf("to_json(struct_pack(%s))::VARCHAR", prop_fields)
  } else {
    props_sql <- "'{}'"
  }

  ## Build each Feature by plain string concatenation. This avoids round-tripping
  ## the geometry through the JSON type (json_object / json_group_array), which is
  ## markedly slower and far more memory-hungry on large datasets. Properties
  ## precede the geometry to match geojsonsf::sf_geojson().
  feature_sql <- sprintf(
    "'{\"type\":\"Feature\",\"properties\":' || %s || ',\"geometry\":' || coalesce(ST_AsGeoJSON(\"%s\"), 'null') || '}'",
    props_sql, x_geom
  )

  # 3. Build and retrieve the result
  if (feature_collection) {
    ## Single FeatureCollection. coalesce() keeps `features` as [] (not null) when empty.
    tmp.query <- sprintf(
      "SELECT '{\"type\":\"FeatureCollection\",\"features\":[' || coalesce(string_agg(%s, ','), '') || ']}' AS geojson FROM %s;",
      feature_sql, x_list$query_name
    )
    out <- DBI::dbGetQuery(target_conn, tmp.query)$geojson
    ## tag like geojsonsf so downstream tools treat it as raw JSON, not a string
    class(out) <- c("geojson", "json")
    out
  } else {
    tmp.query <- sprintf("SELECT %s AS geojson FROM %s;", feature_sql, x_list$query_name)
    DBI::dbGetQuery(target_conn, tmp.query)$geojson
  }

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

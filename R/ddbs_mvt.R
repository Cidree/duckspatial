# Mapbox Vector Tiles (MVT) -----------------------------------------------------
#
# Everything related to MVT lives in this file:
#   - ddbs_as_mvt_geom()  : low-level ST_AsMVTGeom wrapper (geometry -> tile space)
#   - ddbs_write_mbtiles(): generate a vector tile pyramid into an MBTiles file
#   - internal helpers     : mvt_normalize_bounds(), mvt_tile_grid(),
#                            mvt_property_casts(), mvt_write_mbtiles_sqlite(),
#                            mvt_vector_layers_json()



#' Transform geometries into Mapbox Vector Tile coordinate space
#'
#' Prepares geometries for encoding as Mapbox Vector Tiles (MVT) by clipping
#' them to a tile's bounding box and transforming the coordinates into the
#' tile's integer pixel space. This is a low-level building block; to produce a
#' ready-to-serve tile pyramid use \code{\link{ddbs_write_mbtiles}}.
#'
#' @template x
#' @param bounds The tile bounding box, in the same CRS as \code{x}. Either a
#'   numeric vector \code{c(xmin, ymin, xmax, ymax)} or an object understood by
#'   \code{sf::st_bbox()} (e.g. an \code{sf}, \code{sfc}, or \code{bbox}
#'   object).
#' @param extent Integer. The width and height of the tile coordinate space
#'   (the geometry is mapped into the square \code{[0, extent]}). Defaults to
#'   \code{4096}.
#' @param buffer Integer. The number of pixels by which the tile bounds are
#'   expanded before clipping, to avoid rendering artefacts at tile edges.
#'   Defaults to \code{256}.
#' @param clip_geom Logical. If \code{TRUE} (default), geometries are clipped to
#'   the (buffered) tile bounds; geometries falling entirely outside become
#'   \code{NULL}. If \code{FALSE}, geometries are transformed but not clipped.
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @details
#' This is a wrapper around DuckDB's \code{ST_AsMVTGeom}. The output geometries
#' use the MVT tile coordinate system: the origin is the top-left corner and the
#' y-axis points downwards, so the result is no longer in the input CRS (the CRS
#' is dropped). Geometries that fall completely outside the tile bounds return
#' \code{NULL} when \code{clip_geom = TRUE}.
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#'
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", package = "duckspatial")
#' )
#'
#' ## use the layer's own bounding box as the tile bounds
#' ddbs_as_mvt_geom(argentina_ddbs, bounds = sf::st_bbox(sf::st_as_sf(argentina_ddbs)))
#'
#' ## explicit numeric bounds and a smaller tile extent
#' ddbs_as_mvt_geom(
#'   argentina_ddbs,
#'   bounds = c(-74, -56, -53, -21),
#'   extent = 256
#' )
#' }
ddbs_as_mvt_geom <- function(
  x,
  bounds,
  extent = 4096L,
  buffer = 256L,
  clip_geom = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  # 0. Handle function-specific errors
  assert_integer_scalar(extent, "extent")
  assert_integer_scalar(buffer, "buffer")
  assert_logic(clip_geom, "clip_geom")

  if (extent <= 0) cli::cli_abort("{.arg extent} must be a positive integer.")
  if (buffer < 0)  cli::cli_abort("{.arg buffer} must be a non-negative integer.")

  # 1. Normalize bounds to c(xmin, ymin, xmax, ymax)
  bbox <- mvt_normalize_bounds(bounds)

  # 2. Build the ST_AsMVTGeom argument string (bounds as a BOX_2D struct literal)
  bounds_sql <- glue::glue(
    "{{'min_x': {bbox[['xmin']]}, 'min_y': {bbox[['ymin']]}, ",
    "'max_x': {bbox[['xmax']]}, 'max_y': {bbox[['ymax']]}}}::BOX_2D"
  )
  mvt_args <- glue::glue(
    "{bounds_sql}, {as.integer(extent)}, {as.integer(buffer)}, {tolower(as.character(clip_geom))}"
  )

  # 3. Pass to template (drop CRS: output is in tile pixel coordinates)
  template_unary_ops(
    x = x,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_AsMVTGeom",
    other_args = mvt_args,
    drop_crs = TRUE
  )

}



#' Write a Mapbox Vector Tile pyramid to an MBTiles file
#'
#' Generates a pyramid of Mapbox Vector Tiles (MVT) from a spatial dataset and
#' stores them in an \href{https://github.com/mapbox/mbtiles-spec}{MBTiles} file
#' (a SQLite database). For each tile in the requested zoom range the geometries
#' are clipped and re-projected into tile space (\code{ST_AsMVTGeom}) and encoded
#' into a binary vector tile (\code{ST_AsMVT}).
#'
#' @template x
#' @param dsn Path to the output \code{.mbtiles} file.
#' @param layer_name Name of the vector tile layer. This is the value you pass
#'   as \code{source_layer} when adding the layer in a web map. Defaults to
#'   \code{"layer"}.
#' @param zoom_levels Integer vector of zoom levels to generate (e.g.
#'   \code{0:8}). Defaults to \code{0:6}.
#' @param extent Integer tile extent (resolution). Defaults to \code{4096}.
#' @param buffer Integer tile buffer in pixels. Defaults to \code{256}.
#' @template conn_null
#' @param overwrite Logical. Overwrite \code{dsn} if it already exists. Defaults
#'   to \code{FALSE}.
#' @template quiet
#'
#' @details
#' The input is re-projected to web mercator (\code{EPSG:3857}), so \code{x} must
#' have a CRS. All non-geometry columns are carried into the tiles as feature
#' properties; columns whose type is not supported by the vector tile format are
#' cast to \code{DOUBLE} (numeric types) or \code{VARCHAR} (everything else), and
#' \code{BLOB} columns are dropped.
#'
#' Writing the SQLite container relies on DuckDB's \code{sqlite} extension, which
#' is installed and loaded automatically (an internet connection may be required
#' the first time).
#'
#' MBTiles can be served with a vector tile server, or converted to
#' \href{https://github.com/protomaps/PMTiles}{PMTiles} (e.g. with the
#' \code{pmtiles convert} CLI) for static hosting. The resulting tiles can then
#' be displayed with the \pkg{mapgl} package, for example:
#'
#' \preformatted{
#' library(mapgl)
#' maplibre() |>
#'   add_pmtiles_source(id = "src", url = "https://example.com/argentina.pmtiles") |>
#'   add_fill_layer(id = "fill", source = "src", source_layer = "argentina")
#' }
#'
#' @return The path to \code{dsn}, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#'
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", package = "duckspatial")
#' )
#'
#' ddbs_write_mbtiles(
#'   argentina_ddbs,
#'   dsn         = tempfile(fileext = ".mbtiles"),
#'   layer_name  = "argentina",
#'   zoom_levels = 0:6
#' )
#' }
ddbs_write_mbtiles <- function(
  x,
  dsn,
  layer_name = "layer",
  zoom_levels = 0:6,
  extent = 4096L,
  buffer = 256L,
  conn = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_character(conn, x)
  assert_character_scalar(dsn, "dsn")
  assert_character_scalar(layer_name, "layer_name")
  assert_integer_scalar(extent, "extent")
  assert_integer_scalar(buffer, "buffer")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")

  if (!is.numeric(zoom_levels) || length(zoom_levels) < 1 ||
      any(zoom_levels != as.integer(zoom_levels)) ||
      any(zoom_levels < 0) || any(zoom_levels > 22)) {
    cli::cli_abort("{.arg zoom_levels} must be a vector of integers between 0 and 22.")
  }
  zoom_levels <- sort(unique(as.integer(zoom_levels)))

  if (extent <= 0) cli::cli_abort("{.arg extent} must be a positive integer.")
  if (buffer < 0)  cli::cli_abort("{.arg buffer} must be a non-negative integer.")

  if (file.exists(dsn)) {
    if (!overwrite) {
      cli::cli_abort("{.arg dsn} already exists. Use {.code overwrite = TRUE} to replace it.")
    }
    unlink(dsn)
  }

  # 1. Resolve input to a query in a DuckDB connection
  x <- normalize_spatial_input(x, conn)
  crs_x  <- sf::st_crs(ddbs_crs(x, conn))
  if (is.na(crs_x)) {
    cli::cli_abort("{.arg x} must have a CRS so it can be re-projected to web mercator (EPSG:3857).")
  }

  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  on.exit(resolve_conn$cleanup(), add = TRUE)

  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)

  x_geom <- attr(x, "sf_column") %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  # 2. Source CRS string for ST_Transform
  src_crs <- if (!is.na(crs_x$epsg)) paste0("EPSG:", crs_x$epsg) else crs_x$wkt
  src_q   <- ddbs_quote_sql_string(target_conn, src_crs)

  # 3. Introspect attribute columns and build supported-type property casts
  desc      <- DBI::dbGetQuery(target_conn, glue::glue("DESCRIBE SELECT * FROM {x_list$query_name};"))
  attr_cols <- desc[desc$column_name != x_geom, c("column_name", "column_type"), drop = FALSE]
  prop_casts    <- mvt_property_casts(attr_cols$column_name, attr_cols$column_type)
  struct_fields <- if (length(prop_casts)) {
    paste0(paste(prop_casts, collapse = ",\n               "), ",\n               ")
  } else {
    ""
  }

  # 4. Re-project to web mercator in a temp table
  tmp3857 <- ddbs_temp_table_name()
  DBI::dbExecute(target_conn, glue::glue(
    'CREATE TEMP TABLE {tmp3857} AS
     SELECT * EXCLUDE ("{x_geom}"),
            ST_Transform("{x_geom}", {src_q}, \'EPSG:3857\', always_xy := true) AS mvt_src_geom
     FROM {x_list$query_name};'
  ))
  on.exit(DBI::dbExecute(target_conn, glue::glue("DROP TABLE IF EXISTS {tmp3857};")), add = TRUE)

  # 5. Data bbox in web mercator (cheap: per-row min/max, no union)
  bb <- DBI::dbGetQuery(target_conn, glue::glue(
    "SELECT min(ST_XMin(mvt_src_geom)) xmin, min(ST_YMin(mvt_src_geom)) ymin,
            max(ST_XMax(mvt_src_geom)) xmax, max(ST_YMax(mvt_src_geom)) ymax
     FROM {tmp3857};"
  ))

  # 6. Tile grid covering the data for each requested zoom level
  tiles <- mvt_tile_grid(zoom_levels, bb)
  if (nrow(tiles) == 0) {
    cli::cli_abort("No tiles cover the data for the requested {.arg zoom_levels}.")
  }
  tmp_tiles <- ddbs_temp_table_name()
  DBI::dbWriteTable(target_conn, tmp_tiles, tiles, temporary = TRUE)
  on.exit(DBI::dbExecute(target_conn, glue::glue("DROP TABLE IF EXISTS {tmp_tiles};")), add = TRUE)

  # 7. Generate the vector tiles (one binary blob per z/x/y)
  tmp_out <- ddbs_temp_table_name()
  layer_q <- ddbs_quote_sql_string(target_conn, layer_name)
  DBI::dbExecute(target_conn, glue::glue(
    "CREATE TEMP TABLE {tmp_out} AS
     WITH prepped AS (
       SELECT t.z, t.x, t.y,
              struct_pack(
               {struct_fields}geom := ST_AsMVTGeom(
                  d.mvt_src_geom, ST_Extent(ST_TileEnvelope(t.z, t.x, t.y)),
                  {as.integer(extent)}, {as.integer(buffer)}, true)
              ) AS feature
       FROM {tmp_tiles} t
       JOIN {tmp3857} d ON ST_Intersects(d.mvt_src_geom, ST_TileEnvelope(t.z, t.x, t.y))
     )
     SELECT z, x, y, ST_AsMVT(feature, {layer_q}) AS tile
     FROM prepped
     WHERE feature.geom IS NOT NULL
     GROUP BY z, x, y;"
  ))
  on.exit(DBI::dbExecute(target_conn, glue::glue("DROP TABLE IF EXISTS {tmp_out};")), add = TRUE)

  n_tiles <- DBI::dbGetQuery(target_conn, glue::glue("SELECT count(*) n FROM {tmp_out};"))$n

  # 8. Data bbox in lon/lat (EPSG:4326) for the MBTiles metadata
  ll <- DBI::dbGetQuery(target_conn, glue::glue(
    "SELECT min(ST_XMin(g)) xmin, min(ST_YMin(g)) ymin,
            max(ST_XMax(g)) xmax, max(ST_YMax(g)) ymax
     FROM (SELECT ST_Transform(mvt_src_geom, 'EPSG:3857', 'EPSG:4326', always_xy := true) g
           FROM {tmp3857}) t;"
  ))

  # 9. Write the MBTiles SQLite container
  mvt_write_mbtiles_sqlite(target_conn, tmp_out, dsn, layer_name, attr_cols, zoom_levels, ll)

  if (!quiet) {
    cli::cli_alert_success("Wrote {n_tiles} tile{?s} to {.file {dsn}}.")
  }

  invisible(dsn)
}



# Internal helpers -------------------------------------------------------------

#' Normalize a `bounds` argument to c(xmin, ymin, xmax, ymax)
#'
#' @param bounds A numeric vector of length 4 or an object understood by
#'   \code{sf::st_bbox()}.
#'
#' @keywords internal
#' @noRd
mvt_normalize_bounds <- function(bounds) {

  if (inherits(bounds, c("bbox", "sf", "sfc"))) {
    bb <- sf::st_bbox(bounds)
    bbox <- c(xmin = bb[["xmin"]], ymin = bb[["ymin"]],
              xmax = bb[["xmax"]], ymax = bb[["ymax"]])
  } else if (is.numeric(bounds) && length(bounds) == 4) {
    nms <- names(bounds)
    if (!is.null(nms) && all(c("xmin", "ymin", "xmax", "ymax") %in% nms)) {
      bbox <- c(xmin = bounds[["xmin"]], ymin = bounds[["ymin"]],
                xmax = bounds[["xmax"]], ymax = bounds[["ymax"]])
    } else {
      bbox <- c(xmin = bounds[1], ymin = bounds[2],
                xmax = bounds[3], ymax = bounds[4])
    }
  } else {
    cli::cli_abort(
      "{.arg bounds} must be a numeric vector {.code c(xmin, ymin, xmax, ymax)} or an {.cls sf}/{.cls sfc}/{.cls bbox} object."
    )
  }

  if (anyNA(bbox)) {
    cli::cli_abort("{.arg bounds} must not contain missing values.")
  }
  if (bbox[["xmax"]] <= bbox[["xmin"]] || bbox[["ymax"]] <= bbox[["ymin"]]) {
    cli::cli_abort("{.arg bounds} must have {.code xmax > xmin} and {.code ymax > ymin}.")
  }

  bbox
}



#' Compute the XYZ tile grid covering a web-mercator bbox over a zoom range
#'
#' @param zoom_levels Integer vector of zoom levels.
#' @param bb A one-row data frame with `xmin`, `ymin`, `xmax`, `ymax` in
#'   EPSG:3857.
#'
#' @return A data frame with integer columns `z`, `x`, `y` (XYZ / Google scheme).
#' @keywords internal
#' @noRd
mvt_tile_grid <- function(zoom_levels, bb) {

  world <- 20037508.342789244  # half the web-mercator extent

  grids <- lapply(zoom_levels, function(z) {
    n  <- 2^z
    ts <- (2 * world) / n
    xmin_t <- max(0,     floor((bb$xmin + world) / ts))
    xmax_t <- min(n - 1, floor((bb$xmax + world) / ts))
    ## y is flipped: tile row 0 is at the top (north)
    ymin_t <- max(0,     floor((world - bb$ymax) / ts))
    ymax_t <- min(n - 1, floor((world - bb$ymin) / ts))
    if (xmax_t < xmin_t || ymax_t < ymin_t) return(NULL)
    expand.grid(
      z = as.integer(z),
      x = as.integer(seq(xmin_t, xmax_t)),
      y = as.integer(seq(ymin_t, ymax_t))
    )
  })

  out <- do.call(rbind, grids)
  out %||% data.frame(z = integer(), x = integer(), y = integer())
}



#' Build struct_pack property assignments, casting to MVT-supported types
#'
#' `ST_AsMVT` only supports VARCHAR, FLOAT, DOUBLE, INTEGER, BIGINT and BOOLEAN
#' property columns. Numeric types are cast to DOUBLE, other unsupported types to
#' VARCHAR, and BLOB columns are dropped.
#'
#' @param cols,types Character vectors of column names and DuckDB type names.
#'
#' @return A character vector of `"name" := d."name"[::CAST]` expressions.
#' @keywords internal
#' @noRd
mvt_property_casts <- function(cols, types) {

  supported <- c("VARCHAR", "FLOAT", "DOUBLE", "INTEGER", "BIGINT", "BOOLEAN")
  numeric_t <- c("TINYINT", "SMALLINT", "UTINYINT", "USMALLINT", "UINTEGER",
                 "UBIGINT", "HUGEINT", "UHUGEINT", "DECIMAL", "REAL", "NUMERIC")

  out <- character(0)
  for (i in seq_along(cols)) {
    nm      <- cols[i]
    base_ty <- toupper(sub("\\(.*", "", types[i]))  # strip DECIMAL(p,s) etc.

    if (base_ty == "BLOB") next

    expr <- if (base_ty %in% supported) {
      sprintf('"%s" := d."%s"', nm, nm)
    } else if (base_ty %in% numeric_t) {
      sprintf('"%s" := d."%s"::DOUBLE', nm, nm)
    } else {
      sprintf('"%s" := d."%s"::VARCHAR', nm, nm)
    }
    out <- c(out, expr)
  }

  out
}



#' Build the MBTiles `json` metadata value describing the vector layer
#'
#' @keywords internal
#' @noRd
mvt_vector_layers_json <- function(layer_name, attr_cols, minz, maxz) {

  numeric_t <- c("TINYINT", "SMALLINT", "INTEGER", "BIGINT", "UTINYINT",
                 "USMALLINT", "UINTEGER", "UBIGINT", "HUGEINT", "UHUGEINT",
                 "FLOAT", "DOUBLE", "DECIMAL", "REAL", "NUMERIC")

  fields <- list()
  for (i in seq_len(nrow(attr_cols))) {
    base_ty <- toupper(sub("\\(.*", "", attr_cols$column_type[i]))
    if (base_ty == "BLOB") next
    fields[[attr_cols$column_name[i]]] <-
      if (base_ty == "BOOLEAN") "Boolean"
      else if (base_ty %in% numeric_t) "Number"
      else "String"
  }

  obj <- list(vector_layers = list(list(
    id      = layer_name,
    fields  = fields,
    minzoom = minz,
    maxzoom = maxz
  )))

  ## force `fields` to render as a JSON object even when empty
  json <- jsonlite::toJSON(obj, auto_unbox = TRUE)
  as.character(json)
}



#' Write generated tiles + metadata into an MBTiles SQLite file via DuckDB
#'
#' @keywords internal
#' @noRd
mvt_write_mbtiles_sqlite <- function(conn, tiles_table, dsn, layer_name,
                                     attr_cols, zoom_levels, ll) {

  ## Ensure the sqlite extension is available
  tryCatch({
    DBI::dbExecute(conn, "INSTALL sqlite;")
    DBI::dbExecute(conn, "LOAD sqlite;")
  }, error = function(e) {
    cli::cli_abort(c(
      "Could not load the DuckDB {.pkg sqlite} extension required to write MBTiles.",
      "x" = conditionMessage(e)
    ))
  })

  dsn_q <- ddbs_quote_sql_string(conn, dsn)
  DBI::dbExecute(conn, glue::glue("ATTACH {dsn_q} AS mbtiles_out (TYPE sqlite);"))
  on.exit(try(DBI::dbExecute(conn, "DETACH mbtiles_out;"), silent = TRUE), add = TRUE)

  DBI::dbExecute(conn, "CREATE TABLE mbtiles_out.metadata (name TEXT, value TEXT);")
  DBI::dbExecute(conn,
    "CREATE TABLE mbtiles_out.tiles (zoom_level INTEGER, tile_column INTEGER, tile_row INTEGER, tile_data BLOB);")

  ## MBTiles uses the TMS scheme (y flipped): tile_row = 2^z - 1 - y
  DBI::dbExecute(conn, glue::glue(
    "INSERT INTO mbtiles_out.tiles
     SELECT z, x, (CAST(pow(2, z) AS INTEGER) - 1 - y), tile FROM {tiles_table};"
  ))

  ## Metadata rows
  minz <- min(zoom_levels)
  maxz <- max(zoom_levels)
  meta <- data.frame(
    name = c("name", "format", "minzoom", "maxzoom", "bounds", "center", "type", "json"),
    value = c(
      layer_name,
      "pbf",
      as.character(minz),
      as.character(maxz),
      sprintf("%f,%f,%f,%f", ll$xmin, ll$ymin, ll$xmax, ll$ymax),
      sprintf("%f,%f,%d", (ll$xmin + ll$xmax) / 2, (ll$ymin + ll$ymax) / 2, minz),
      "overlay",
      mvt_vector_layers_json(layer_name, attr_cols, minz, maxz)
    ),
    stringsAsFactors = FALSE
  )

  values_sql <- paste(
    sprintf("(%s, %s)",
            ddbs_quote_sql_string(conn, meta$name),
            ddbs_quote_sql_string(conn, meta$value)),
    collapse = ",\n"
  )
  DBI::dbExecute(conn, glue::glue(
    "INSERT INTO mbtiles_out.metadata (name, value) VALUES {values_sql};"
  ))

  invisible(TRUE)
}

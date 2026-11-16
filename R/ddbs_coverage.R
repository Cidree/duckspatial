#' Operations on polygonal coverages
#'
#' @description
#' Functions for working with polygonal coverages — sets of polygons that
#' share edges with no overlaps or gaps:
#'
#' * `ddbs_coverage_union()` - Union all polygons in a coverage into a single geometry
#' * `ddbs_coverage_invalid_edges()` - Detect edges not shared by exactly two polygons
#'
#' @template x
#' @param tolerance Numeric. Tolerance used to detect shared edges in
#' `ddbs_coverage_invalid_edges()`. Default is `0`.
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @details
#' ## ddbs_coverage_union(x)
#' Unions all polygons into one geometry. Faster than [ddbs_union()] on valid
#' coverages, at the cost of more memory.
#'
#' ## ddbs_coverage_invalid_edges(x, tolerance)
#' Returns `NULL` if `x` is not a polygonal coverage, or if it is already valid.
#' Use this to validate a coverage before relying on `ddbs_coverage_union()`.
#'
#' @template returns_mode
#'
#' @seealso [ddbs_union_funs] for general-purpose union/combine operations.
#'
#' @name ddbs_coverage_funs
#' @rdname ddbs_coverage_funs
#' 
#' @examples
#' \dontrun{
#' library(duckspatial)
#' library(sf)
#' 
#' # ddbs_coverage_invalid_edges
#'
#' ## Two squares meant to share the edge x=1, but one has a mismatched vertex
#' p1 <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
#' p2 <- st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,0.9), c(1,0), c(1,0)))) 
#' sfc <- st_sfc(p1, p2, crs = 4326)
#' sf_obj <- st_sf(id = 1:2, geom = sfc)
#'
#' ddbs_coverage_invalid_edges(sf_obj)
#' 
#' # ddbs_coverage_union
#' 
#' ## Load countries dataset
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## Union into a single geometry
#' ddbs_coverage_union(countries_ddbs)
#' }
NULL




#' @rdname ddbs_coverage_funs
#' @export
ddbs_coverage_union <- function(
  x,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_x_name(conn, x, name)
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  assert_geom_type(x = x, conn = conn, geom = "POLYGON", multi = TRUE)

  # 1. Prepare inputs
  x <- normalize_spatial_input(x, conn)

  crs_x    <- ddbs_crs(x, conn)
  sf_col_x <- attr(x, "sf_column")
  mode     <- get_mode(mode, name)

  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  on.exit(resolve_conn$cleanup(), add = TRUE)

  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)

  # 2. Prepare the query
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  st_function <- glue::glue("ST_CoverageUnion(array_agg({sql_ident(x_geom)}))")
  base.query <- glue::glue("
    SELECT 
      {build_geom_query(st_function, name, crs_x, mode)} AS {sql_ident(x_geom)}
    FROM {x_list$query_name};
  ")

  # 3. Output
  if (!is.null(name)) {
    create_duckdb_table(
      conn      = target_conn,
      name      = name,
      query     = base.query,
      overwrite = overwrite,
      quiet     = quiet
    )
  } else {
    ddbs_handle_query(
      query  = base.query,
      conn   = target_conn,
      mode   = mode,
      crs    = crs_x,
      x_geom = x_geom
    )
  }

}




#' @rdname ddbs_coverage_funs
#' @export
ddbs_coverage_invalid_edges <- function(
  x,
  tolerance = 0,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_numeric(tolerance, "tolerance")
  assert_conn_x_name(conn, x, name)
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  assert_geom_type(x = x, conn = conn, geom = "POLYGON", multi = TRUE)

  # 1. Prepare inputs
  x <- normalize_spatial_input(x, conn)

  crs_x    <- ddbs_crs(x, conn)
  sf_col_x <- attr(x, "sf_column")
  mode     <- get_mode(mode, name)

  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  on.exit(resolve_conn$cleanup(), add = TRUE)

  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)

  # 2. Prepare the query
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  st_function <- glue::glue("ST_CoverageInvalidEdges(array_agg({sql_ident(x_geom)}), {tolerance})")
  base.query <- glue::glue("
    SELECT {build_geom_query(st_function, name, crs_x, mode)} AS {sql_ident(x_geom)}
    FROM {x_list$query_name};
  ")

  # 3. Output
  if (!is.null(name)) {
    create_duckdb_table(
      conn      = target_conn,
      name      = name,
      query     = base.query,
      overwrite = overwrite,
      quiet     = quiet
    )
  } else {
    ddbs_handle_query(
      query  = base.query,
      conn   = target_conn,
      mode   = mode,
      crs    = crs_x,
      x_geom = x_geom
    )
  }

}
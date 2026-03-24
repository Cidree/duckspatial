
#' Generate random points within bounding boxes of geometries
#'
#' Creates random points within the bounding box of each geometry, which may 
#' fall outside the geometry itself.
#'
#' @template x
#' @param n Number of random points to generate within each geometry
#' @param seed A number for the random number generator
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_table(conn, argentina_ddbs, "argentina")
#'
#' ## generate 100 random points within each geometry
#' ddbs_generate_points("argentina", n = 100, conn)
#'
#' ## generate points without using a connection
#' ddbs_generate_points(argentina_ddbs, n = 100)
#' }
ddbs_generate_points <- function(
  x,
  n,
  seed = NULL,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_numeric(n, "n")
  assert_conn_x_name(conn, x, name)
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")


 # 1. Prepare inputs
  
  ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names)
  x <- normalize_spatial_input(x, conn)

  ## 1.2. Pre-extract attributes
  crs_x <- ddbs_crs(x, conn)
  mode  <- get_mode(mode, name)

  ## 1.3. Resolve spatial connections and handle imports
  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  ## register cleanup of the connection
  on.exit(resolve_conn$cleanup(), add = TRUE)

  ## 1.4. Get list with query names for the input data
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)


  # 2. Prepare the query
  
  ## 2.1. Get the bounding box of the input geometries
  bbox <- ddbs_bbox(x_list$query_name, by_feature = FALSE, conn = target_conn, quiet = TRUE)

  ## 2.2. Create the table and store it as a view
  view_name_tbl <- ddbs_temp_view_name()
  generate_points_query <- if (is.null(seed)) {
    glue::glue("ST_GeneratePoints({{min_x: {bbox$xmin}, min_y: {bbox$ymin}, max_x: {bbox$xmax}, max_y: {bbox$ymax}}}::BOX_2D, {n}) as geometry")
  } else {
    glue::glue("ST_GeneratePoints({{min_x: {bbox$xmin}, min_y: {bbox$ymin}, max_x: {bbox$xmax}, max_y: {bbox$ymax}}}::BOX_2D, {n}, {seed}) as geometry")
  }
  tmp.query   <- glue::glue("
    CREATE TEMP VIEW '{view_name_tbl}' AS 
    SELECT
      ST_X(point) AS x,
      ST_Y(point) AS y,
    FROM 
       {generate_points_query};
  ")
  DBI::dbExecute(target_conn, tmp.query)

  ## 2.3. Build base query  
  st_function <- "ST_Point(x, y)"
  base.query <- glue::glue("
    SELECT {build_geom_query(st_function, name, crs_x, mode)} as geometry
    FROM {view_name_tbl};
  ")  


  # 3. Table creation if name is provided, or 
  # create duckspatial_df or sf object if name is NULL
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
      query      = base.query,
      conn       = target_conn,
      mode       = mode,
      crs        = crs_x,
      x_geom     = "geometry"
    )
  }

}

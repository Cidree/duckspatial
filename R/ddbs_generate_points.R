

#' Generate random points within geometries
#'
#' Generates random points within geometries from a DuckDB table using the spatial extension.
#' Works similarly to generating random points within polygons in \code{sf}.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param n Number of random points to generate within each geometry
#' @param seed A number for the random number generator
#' @template conn_null
#' @template name
#' @template crs
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @template returns_output
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
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
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
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = NULL,
  overwrite = FALSE,
  quiet = FALSE
) {

  deprecate_crs(crs_column, crs)

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_numeric(n, "n")
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(output, "output")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")

  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (CRS and geometry column name)
  ## this step should be before normalize_spatial_input()
  crs_x    <- detect_crs(x)
  sf_col_x <- attr(x, "sf_column")

  ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names
  x <- normalize_spatial_input(x, conn)


  # 2. Manage connection to DB

  ## 2.1. Resolve connections and handle imports
  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  ## register cleanup of the connection
  on.exit(resolve_conn$cleanup(), add = TRUE)

  ## 2.2. Get query list of table names
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)
  
  bbox <- ddbs_bbox(x_list$query_name, conn = target_conn, quiet = TRUE)
  if (is.null(crs)) crs_data <- ddbs_crs(target_conn, x_list$query_name)$input else crs_data <- crs

  # 2. Create table as temp view
  ## 2.1. Create the table and store it as a view
  view_name <- paste0("temp-", uuid::UUIDgenerate())
  generate_points_query <- if (is.null(seed)) {
    glue::glue("ST_GeneratePoints({{min_x: {bbox$min_x}, min_y: {bbox$min_y}, max_x: {bbox$max_x}, max_y: {bbox$max_y}}}::BOX_2D, {n}) as geometry")
  } else {
    glue::glue("ST_GeneratePoints({{min_x: {bbox$min_x}, min_y: {bbox$min_y}, max_x: {bbox$max_x}, max_y: {bbox$max_y}}}::BOX_2D, {n}, {seed}) as geometry")
  }
  tmp.query   <- glue::glue("
    CREATE VIEW '{view_name}' AS 
    SELECT
      ST_X(point) AS x,
      ST_Y(point) AS y,
      '{crs_data}' AS {crs_column}
    FROM 
       {generate_points_query};
  ")
  DBI::dbExecute(target_conn, tmp.query)
  on.exit(DBI::dbExecute(target_conn, glue::glue('DROP VIEW IF EXISTS "{view_name}";')))

  # 3. if name is not NULL (i.e. no SF returned)
  if (!is.null(name)) {

      ## convenient names of table and/or schema.table
      name_list <- get_query_name(name)

      ## handle overwrite
      overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

      ## create query
      tmp.query <- glue::glue("
          CREATE TABLE {name_list$query_name} AS
          SELECT {crs_column}, ST_Point(x, y) as geometry FROM '{view_name}';
      ")

      ## execute query
      DBI::dbExecute(target_conn, tmp.query)
      feedback_query(quiet)
      return(invisible(TRUE))

  }

  # 4. Get data frame
  ## 4.1. create query
  tmp.query <- glue::glue("
    SELECT 
    {crs_column},
    ST_AsWKB(ST_Point(x, y)) as geometry 
    FROM '{view_name}'
  ")
  ## 4.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

  ## 5. convert to SF and return result
  data_sf <- ddbs_handle_output(
      data       = data_tbl,
      conn       = target_conn,
      output     = output,
      crs        = if (!is.null(crs)) crs else crs_x,
      crs_column = crs_column,
      x_geom     = "geometry"
  )

  feedback_query(quiet)
  return(data_sf)

}



#' Generate random points within geometries
#'
#' Generates random points within geometries from a DuckDB table using the spatial extension.
#' Works similarly to generating random points within polygons in \code{sf}.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param n Number of random points to generate within each geometry
#' @template conn_null
#' @template name
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
#' ## load packages
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## generate 100 random points within each geometry
#' ddbs_generate_points("argentina", n = 100, conn)
#'
#' ## generate points without using a connection
#' ddbs_generate_points(argentina_sf, n = 100)
ddbs_generate_points <- function(
  x,
  n,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  overwrite = FALSE,
  quiet = FALSE
) {

  deprecate_crs(crs_column, crs)

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_name(name)
  assert_numeric(n, "n")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)

  # 1. Manage connection to DB
  ## 1.1. check if connection is provided, otherwise create a temporary connection
  is_duckdb_conn <- dbConnCheck(conn)
  if (isFALSE(is_duckdb_conn)) {
    conn <- duckspatial::ddbs_create_conn()
    on.exit(duckdb::dbDisconnect(conn), add = TRUE)
  }
  ## 1.2. get query list of table names
  x_list <- get_query_list(x, conn)
  bbox   <- ddbs_bbox(x_list$query_name, conn = conn, quiet = TRUE)
  if (is.null(crs)) crs_data <- ddbs_crs(conn, x_list$query_name)$input else crs_data <- crs

  # 2. Create table as temp view
  ## 2.1. Create the table and store it as a view
  view_name <- paste0("temp-", uuid::UUIDgenerate())
  tmp.query   <- glue::glue("
    CREATE VIEW '{view_name}' AS 
    SELECT
      ST_X(point) AS x,
      ST_Y(point) AS y,
      '{crs_data}' AS {crs_column}
    FROM 
      ST_GeneratePoints({{min_x: {bbox$min_x}, min_y: {bbox$min_y}, max_x: {bbox$max_x}, max_y: {bbox$max_y}}}::BOX_2D, {n}) as geometry;
  ")
  DBI::dbExecute(conn, tmp.query)
  on.exit(DBI::dbExecute(conn, glue::glue('DROP VIEW IF EXISTS "{view_name}";')))

  # 3. if name is not NULL (i.e. no SF returned)
  if (!is.null(name)) {

      ## convenient names of table and/or schema.table
      name_list <- get_query_name(name)

      ## handle overwrite
      overwrite_table(name_list$query_name, conn, quiet, overwrite)

      ## create query
      tmp.query <- glue::glue("
          CREATE TABLE {name_list$query_name} AS
          SELECT {crs_column}, ST_Point(x, y) as geometry FROM '{view_name}';
      ")

      ## execute query
      DBI::dbExecute(conn, tmp.query)
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
  data_tbl <- DBI::dbGetQuery(conn, tmp.query)

  ## 5. convert to SF and return result
  data_sf <- convert_to_sf_wkb(
      data       = data_tbl,
      crs        = crs,
      crs_column = crs_column,
      x_geom     = "geometry"
  )

  feedback_query(quiet)
  return(data_sf)

}

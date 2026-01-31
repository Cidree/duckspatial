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

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)


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


  # 3. Prepare parameters for the query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)


  # 4. Get data as vector

  ## 4.1. create query
  tmp.query <- glue::glue("
      SELECT ST_AsText({x_geom}) as geometry
      FROM {x_list$query_name};
  ")

  ## 4.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)
  data_vec <- data_tbl$geometry

  feedback_query(quiet)
  return(data_vec)
}






#' @rdname ddbs_as_format
#' @export
ddbs_as_wkb <- function(
  x,
  conn = NULL,
  quiet = FALSE) {

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)


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


  # 3. Prepare parameters for the query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)


  # 4. Get data as list
  
  ## 4.1. create query
  tmp.query <- glue::glue("
      SELECT ST_AsWkb({x_geom}) as geometry
      FROM {x_list$query_name};
  ")

  ## 4.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query) 
  data_lst <- data_tbl$geometry

  feedback_query(quiet)
  return(data_lst)
}






#' @rdname ddbs_as_format
#' @export
ddbs_as_hexwkb <- function(
  x,
  conn = NULL,
  quiet = FALSE) {

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)


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


  # 3. Prepare parameters for the query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)


  # 4. Get data as list

  ## 4.1. create query
  tmp.query <- glue::glue("
      SELECT ST_AsHEXWKB({x_geom}) as geometry
      FROM {x_list$query_name};
  ")

  ## 4.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query) 
  data_vec <- data_tbl$geometry

  feedback_query(quiet)
  return(data_vec)
}








#' @rdname ddbs_as_format
#' @export
ddbs_as_geojson <- function(
  x,
  conn = NULL,
  quiet = FALSE) {

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)

  ## suggested packages
  rlang::check_installed(
    "geojson",
    "to convert to geojson format."
  )


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


  # 3. Prepare parameters for the query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)


  # 4. Get data as list

  ## 4.1. create query
  tmp.query <- glue::glue("
      SELECT ST_AsGeoJSON({x_geom}) as geometry
      FROM {x_list$query_name};
  ")

  ## 4.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query) 
  data_vec <- data_tbl$geometry

  feedback_query(quiet)
  return(data_vec)
}

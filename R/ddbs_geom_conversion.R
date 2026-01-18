


#' Convert geometries to Well-Known Text (WKT) format
#'
#' Converts spatial geometries to their Well-Known Text (WKT) representation.
#' This function wraps DuckDB's ST_AsText spatial function.
#'
#' @template x
#' @template conn_null
#' @template quiet
#'
#' @returns A character vector containing WKT representations of the geometries
#'
#' @details
#' Well-Known Text (WKT) is a text markup language for representing vector
#' geometry objects. This function is useful for exporting geometries in a
#' portable text format that can be used with other spatial tools and databases.
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' ## convert geometries to WKT
#' wkt_text <- ddbs_as_text(conn = conn, "argentina")
#'
#' ## convert without using a connection
#' wkt_text <- ddbs_as_text(argentina_sf)
#' }
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






#' Convert geometries to Well-Known Binary (WKB) format
#'
#' Converts spatial geometries to their Well-Known Binary (WKB) representation.
#' This function wraps DuckDB's ST_AsWkb spatial function.
#'
#' @template x
#' @template conn_null
#' @template quiet
#'
#' @returns A list of raw vectors, where each element contains the WKB 
#'  representation of a geometry
#'
#' @details
#' Well-Known Binary (WKB) is a binary representation of vector geometry objects.
#' WKB is more compact than WKT and is commonly used for efficient storage and
#' transfer of spatial data between systems. Each geometry is returned as a raw
#' vector of bytes.
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' ## convert geometries to WKB
#' wkb_list <- ddbs_as_wkb(conn = conn, "argentina")
#'
#' ## convert without using a connection
#' wkb_list <- ddbs_as_wkb(argentina_sf)
#' }
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






#' Convert geometries to hexadecimal Well-Known Binary (HEXWKB) format
#'
#' Converts spatial geometries to their hexadecimal Well-Known Binary (HEXWKB) 
#' representation. This function wraps DuckDB's ST_AsHEXWKB spatial function.
#'
#' @template x
#' @template conn_null
#' @template quiet
#'
#' @returns A character vector containing hexadecimal-encoded WKB representations 
#'   of the geometries
#'
#' @details
#' HEXWKB is a hexadecimal string representation of Well-Known Binary (WKB) format.
#' This encoding is human-readable (unlike raw WKB) while maintaining the compact
#' binary structure. HEXWKB is commonly used in databases and web services for
#' transmitting spatial data as text strings.
#'
#' @export
#'
#' @examples
#' \dontrun{
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
#' ## convert geometries to HEXWKB
#' hexwkb_text <- ddbs_as_hexwkb(conn = conn, "argentina")
#'
#' ## convert without using a connection
#' hexwkb_text <- ddbs_as_hexwkb(argentina_sf)
#' }
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

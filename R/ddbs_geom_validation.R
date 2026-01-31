
#' Check if geometries are simple
#'
#' Determines whether geometries are simple. That is, free of self-intersections and 
#' returns a boolean indicator of simplicity.
#'
#' @template x
#' @template conn_null
#' @template crs
#' @template quiet
#'
#' @returns A logical vector
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
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
#' ## check simplicity
#' ddbs_is_simple("argentina", conn)
#'
#' ## check simplicity without using a connection
#' ddbs_is_simple(argentina_ddbs)
#' }
ddbs_is_simple <- function(
  x,
  conn = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE) {
  
  deprecate_crs(crs_column, crs)

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_conn_character(conn, x)
  assert_logic(quiet, "quiet")


  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (CRS and geometry column name)
  ## this step should be before normalize_spatial_input()
  crs_x    <- ddbs_crs(x, conn)
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

  
  # 3. Make the query

  ## 3.1. Prepare parameters
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  ## 3.2. Prepare the query
  tmp.query <- glue::glue("
    SELECT ST_IsSimple({x_geom}) as issimple,
    FROM {x_list$query_name}
  ")

  ## 3.3. Get results
  data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
  feedback_query(quiet)
  return(data_vec[, 1])

}





#' Check if geometries are valid
#'
#' Determines whether geometries are valid. That is, whether they follow the rules 
#' of well-formed geometries (no self-intersections, proper ring orientation, etc.)
#' and returns a boolean indicator of validity.
#'
#' @template x
#' @template conn_null
#' @template crs
#' @template quiet
#'
#' @returns A logical vector
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
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
#' ## check validity
#' ddbs_is_valid("argentina", conn)
#'
#' ## check validity without using a connection
#' ddbs_is_valid(argentina_ddbs)
#' }
ddbs_is_valid <- function(
  x,
  conn = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE) {
  
  deprecate_crs(crs_column, crs)

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_conn_character(conn, x)
  assert_logic(quiet, "quiet")


  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (CRS and geometry column name)
  ## this step should be before normalize_spatial_input()
  crs_x    <- ddbs_crs(x, conn)
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


  # 3. Make the query

  ## 3.1. Prepare parameters
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  ## 3.2. Prepare the query
  tmp.query <- glue::glue("
    SELECT ST_IsValid({x_geom}) as isvalid,
    FROM {x_list$query_name}
  ")

  ## 3.3. Get results
  data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
  feedback_query(quiet)
  return(data_vec[, 1])
  
}

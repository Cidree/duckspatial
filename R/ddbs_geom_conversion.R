


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
ddbs_as_text <- function(
  x,
  conn = NULL,
  quiet = FALSE) {

  ## 0. Handle errors
  assert_xy(x, "x")
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

  ## 2. get name of geometry column
  x_geom <- get_geom_name(conn, x_list$query_name)
  x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
  assert_geometry_column(x_geom, x_list)

  # 3. Get data as vector
  ## 3.1. create query
  tmp.query <- glue::glue("
      SELECT ST_AsText({x_geom}) as {x_geom}
      FROM {x_list$query_name};
  ")
  ## 3.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(conn, tmp.query) |> 
    as.vector()

  feedback_query(quiet)
  return(data_tbl)
}
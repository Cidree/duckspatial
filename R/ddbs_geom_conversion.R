


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
ddbs_as_wkb <- function(
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

    # 3. Get data as list
    ## 3.1. create query
    tmp.query <- glue::glue("
        SELECT ST_AsWkb({x_geom}) as geometry
        FROM {x_list$query_name};
    ")
    ## 3.2. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query) 
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
ddbs_as_hexwkb <- function(
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

    # 3. Get data as list
    ## 3.1. create query
    tmp.query <- glue::glue("
        SELECT ST_AsHEXWKB({x_geom}) as geometry
        FROM {x_list$query_name};
    ")
    ## 3.2. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query) |> 
      as.vector()

    feedback_query(quiet)
    return(data_tbl)
}

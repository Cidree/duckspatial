


#' Spatial intersection predicate (ST_Intersects)
#'
#' Computes spatial intersections between two geometry datasets using DuckDB's
#' spatial extension. Returns a list where each element corresponds to a row of
#' `x`, containing the indices (or IDs) of rows in `y` that intersect it.
#'
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#'        Data is returned from this object.
#' @template conn_null
#' @param id_x Character; optional name of the column in `x` whose values will
#'   be used to name the list elements. If `NULL`, integer row numbers of `x` are used.
#' @param id_y Character; optional name of the column in `y` whose values will
#'   replace the integer indices returned in each element of the list.
#' @template quiet
#' 
#' @details
#' 
#' #' This function mirrors the behavior of [`sf::st_intersects()`][sf::st_intersects]:
#' it performs a pairwise comparison between all geometries in `x` and `y`.
#'
#' If `x` or `y` are not DuckDB tables, they are automatically copied into a
#' temporary in-memory DuckDB database (unless a connection is supplied via `conn`).
#'
#' `id_x` or `id_y` may be used to replace the default integer indices with the
#' values of an identifier column in `x` or `y`, respectively.
#'
#' @returns
#' A **list** of length equal to the number of rows in `x`.
#'
#' - Each element contains:
#'   - **integer vector** of row indices of `y` that intersect the corresponding
#'     geometry of `x`, or  
#'   - **character vector** if `id_y` is supplied.
#'
#' - The names of the list elements:
#'   - are integer row numbers of `x`, or  
#'   - the values of `id_x` if provided.
#'
#' @export
#'
#' @examples
#' ## Load packages
#' library(duckdb)
#' library(duckspatial)
#' library(dplyr)
#' library(sf)
#' 
#' ## create in-memory DuckDB database
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read countries data, and rivers of Spain
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |> 
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |> 
#'   st_transform(st_crs(countries_sf))
#' 
#' ## Store in DuckDB
#' ddbs_write_vector(conn, countries_sf, "countries")
#' ddbs_write_vector(conn, rivers_sf, "rivers")
#' 
#' ## Option 1: Basic intersection (returns list of indices)
#' ddbs_intersects(countries_sf, rivers_sf, conn)
#' 
#' ## Option 2: add ID column
#' ddbs_intersects(countries_sf, rivers_sf, id_x = "NAME_ENGL", id_y = "RIVER_NAME")
#' 
#' ## Option 3: use table names inside duckdb
#' ddbs_intersects("countries", "rivers", conn, "NAME_ENGL")
ddbs_intersects <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  quiet = FALSE) {

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_xy(y, "y")
  assert_logic(quiet, "quiet")

  # 1. Manage connection to DB
  ## 1.1. check if connection is provided, otherwise create a temporary connection
  is_duckdb_conn <- dbConnCheck(conn)
  if (isFALSE(is_duckdb_conn)) {
      conn <- duckspatial::ddbs_create_conn()  
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
  }
  ## 1.2. get query list of table names
  x_list <- get_query_list(x, conn)
  y_list <- get_query_list(y, conn)
  assert_crs(conn, x_list$query_name, y_list$query_name)

  ## 2. get name of geometry columns
  x_geom <- get_geom_name(conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)
  
  y_geom <- get_geom_name(conn, y_list$query_name)
  assert_geometry_column(y_geom, y_list)

  ## check if id column name exists in x or y
  assert_predicate_id(id_x, conn, x_list$query_name)  
  assert_predicate_id(id_y, conn, y_list$query_name)  

  # 3. Get data frame
  ## 3.1. create query
  tmp.query <- glue::glue("
    SELECT ST_Intersects(x.{x_geom}, y.{y_geom}) as predicate
    FROM {x_list$query_name} x
    CROSS JOIN {y_list$query_name} y
  ")     
  ## 3.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(conn, tmp.query)

  # 4. Reframe data
  result_lst <- reframe_predicate_data(
    conn   = conn,
    data   = data_tbl,
    x_list = x_list, 
    y_list = y_list,
    id_x   = id_x,
    id_y   = id_y
  )

  feedback_query(quiet)
  return(result_lst)
}


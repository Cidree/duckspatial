


#' Spatial predicate operations
#'
#' Computes spatial relationships between two geometry datasets using DuckDB's
#' spatial extension. Returns a list where each element corresponds to a row of
#' `x`, containing the indices (or IDs) of rows in `y` that satisfy the specified
#' spatial predicate.
#'
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#'        Data is returned from this object.
#' @template predicate
#' @template conn_null
#' @template conn_x_conn_y
#' @template predicate_args
#' @param distance a numeric value specifying the distance for ST_DWithin. Units correspond to
#' the coordinate system of the geometry (e.g. degrees or meters)
#' @template quiet
#'
#' @details
#'
#' This function provides a unified interface to all spatial predicate operations
#' in DuckDB's spatial extension. It performs pairwise comparisons between all
#' geometries in `x` and `y` using the specified predicate.
#'
#' ## Available Predicates
#'
#' - **intersects**: Geometries share at least one point
#' - **covers**: Geometry `x` completely covers geometry `y`
#' - **touches**: Geometries share a boundary but interiors do not intersect
#' - **disjoint**: Geometries have no points in common
#' - **within**: Geometry `x` is completely inside geometry `y`
#' - **dwithin**: Geometry `x` is completely within a distance of geometry `y`
#' - **contains**: Geometry `x` completely contains geometry `y`
#' - **overlaps**: Geometries share some but not all points
#' - **crosses**: Geometries have some interior points in common
#' - **equals**: Geometries are spatially equal
#' - **covered_by**: Geometry `x` is completely covered by geometry `y`
#' - **intersects_extent**: Bounding boxes of geometries intersect (faster but less precise)
#' - **contains_properly**: Geometry `x` contains geometry `y` without boundary contact
#' - **within_properly**: Geometry `x` is within geometry `y` without boundary contact
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
#'   - **integer vector** of row indices of `y` that satisfy the predicate with
#'     the corresponding geometry of `x`, or
#'   - **character vector** if `id_y` is supplied.
#'
#' - The names of the list elements:
#'   - are integer row numbers of `x`, or
#'   - the values of `id_x` if provided.
#'
#' If there's no match between `x` and `y` it returns `NULL`
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## Load packages
#' library(duckspatial)
#' library(dplyr)
#' 
#' ## create in-memory DuckDB database
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read countries data, and rivers
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' ) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' 
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson", 
#'   package = "duckspatial")
#' ) |>
#'   ddbs_transform(ddbs_crs(countries_ddbs))
#' 
#' ## Store in DuckDB
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#' ddbs_write_vector(conn, rivers_ddbs, "rivers")
#' 
#' ## Example 1: Check which rivers intersect each country
#' ddbs_predicate(countries_ddbs, rivers_ddbs, predicate = "intersects")
#' ddbs_intersects(countries_ddbs, rivers_ddbs)
#' 
#' ## Example 2: Find neighboring countries
#' ddbs_predicate(
#'   countries_ddbs, 
#'   countries_ddbs, 
#'   predicate = "touches",
#'   id_x = "NAME_ENGL", 
#'   id_y = "NAME_ENGL"
#' )
#' 
#' ddbs_touches(
#'   countries_ddbs, 
#'   countries_ddbs, 
#'   id_x = "NAME_ENGL", 
#'   id_y = "NAME_ENGL"
#' )
#' 
#' ## Example 3: Find rivers that don't intersect countries
#' ddbs_predicate(
#'   countries_ddbs, 
#'   rivers_ddbs, 
#'   predicate = "disjoint",
#'   id_x = "NAME_ENGL", 
#'   id_y = "RIVER_NAME"
#' )
#' 
#' ## Example 4: Use table names inside duckdb
#' ddbs_predicate("countries", "rivers", predicate = "within", conn, id_x = "NAME_ENGL")
#' ddbs_within("countries", "rivers", conn,  id_x = "NAME_ENGL")
#' }
#' @name ddbs_predicate
#' @rdname ddbs_predicate
NULL



#' @rdname ddbs_predicate
ddbs_predicate <- function(
  x,
  y,
  predicate = "intersects",
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  distance = NULL,
  quiet = FALSE) {

  
  ## 0. Handle errors
  assert_xy(x, "x")
  assert_xy(y, "y")
  assert_logic(quiet, "quiet")
  assert_logic(sparse, "sparse")

  ## Validate predicate early (it aborts on invalid)
  st_predicate <- get_st_predicate(predicate)


  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (CRS and geometry column name)
  ## this step should be before normalize_spatial_input()
  crs_x <- detect_crs(x)
  crs_y <- detect_crs(y)
  sf_col_x <- attr(x, "sf_column")
  sf_col_y <- attr(y, "sf_column")

  ## 1.2. Resolve conn_x/conn_y defaults from 'conn' for character inputs
  if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
  if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

  ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names
  x <- normalize_spatial_input(x, conn_x)
  y <- normalize_spatial_input(y, conn_y)


  # 2. Manage connection to DB

  ## 2.1. Resolve connections and handle imports
  resolve_conn <- resolve_spatial_connections(x, y, conn, conn_x, conn_y)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  y            <- resolve_conn$y
  ## register cleanup of the connection
  on.exit(resolve_conn$cleanup(), add = TRUE)

  ## 2.2. Get query list of table names
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)
  y_list <- get_query_list(y, target_conn)
  on.exit(y_list$cleanup(), add = TRUE)

  ## check if id column name exists in x or y
  assert_predicate_id(id_x, target_conn, x_list$query_name)
  assert_predicate_id(id_y, target_conn, y_list$query_name)

  ## CRS already extracted at start of function
  if (!is.null(crs_x) && !is.null(crs_y)) {
      if (!crs_equal(crs_x, crs_y)) {
        cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
      }
  } else {
      assert_crs(target_conn, x_list$query_name, y_list$query_name)
  }


  # 3. Prepare parameters for the query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
  assert_geometry_column(x_geom, x_list)
  assert_geometry_column(y_geom, y_list)

  ## 3.2. Get names of the rest of the columns
  x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

  
  # 4. Get data frame
  ## 4.1. create query
  if (st_predicate == "ST_DWithin") {

    ## if distance is not specified, it will use ST_Within
    if (is.null(distance)) {
      cli::cli_warn("{.val distance} wasn't specified. Using ST_Within.")
      distance <- 0
    }

    tmp.query <- glue::glue("
      SELECT {st_predicate}(x.{x_geom}, y.{y_geom}, {distance}) as predicate
      FROM {x_list$query_name} x
      CROSS JOIN {y_list$query_name} y
    ")

  } else {
    tmp.query <- glue::glue("
      SELECT {st_predicate}(x.{x_geom}, y.{y_geom}) as predicate
      FROM {x_list$query_name} x
      CROSS JOIN {y_list$query_name} y
    ")
  }
  ## 4.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

  # 5. Reframe data
  result_lst <- reframe_predicate_data(
    conn   = target_conn,
    data   = data_tbl,
    x_list = x_list,
    y_list = y_list,
    id_x   = id_x,
    id_y   = id_y,
    sparse = sparse
  )

  feedback_query(quiet)
  return(result_lst)
}





#' @rdname ddbs_predicate
ddbs_intersects <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "intersects",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_covers <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "covers",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_touches <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "touches",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_is_within_distance <- function(
  x,
  y,
  distance = NULL,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "dwithin",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    distance  = distance,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_disjoint <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x, 
    y         = y, 
    predicate = "disjoint", 
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x, 
    id_y      = id_y, 
    sparse    = sparse, 
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_within <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "within",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_contains <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "contains",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_overlaps <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "overlaps",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_crosses <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "crosses",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_equals <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "equals",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_covered_by <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "covered_by",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_intersects_extent <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "intersects_extent",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_contains_properly <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "contains_properly",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' @rdname ddbs_predicate
ddbs_within_properly <- function(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "within_properly",
    conn      = conn,
    conn_x    = conn_x,
    conn_y    = conn_y, 
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}

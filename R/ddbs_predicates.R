


#' Evaluate spatial predicates between geometries
#'
#' Determines which geometries in one dataset satisfy a specified spatial 
#' relationship with geometries in another dataset, such as intersection, 
#' containment, or touching.
#'
#' @template x
#' @template y
#' @template predicate
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @template predicate_args
#' @param distance a numeric value specifying the distance for ST_DWithin. Units correspond to
#' the coordinate system of the geometry (e.g. degrees or meters)
#' @template mode
#' @template overwrite
#' @template quiet
#' @param ... Passed to [ddbs_predicate]
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
#' @returns Depends on the \code{mode} argument (or global preference set by \code{\link{ddbs_options}}):
#' \itemize{
#'   \item \code{duckspatial} (default): A \code{tbl_duckdb_connection} (lazy data frame) backed by dbplyr/DuckDB.
#'   \item \code{sf}: An eagerly collected list.
#' }
#' When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).
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
  name = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  distance = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = TRUE) {

  
  ## 0. Handle errors
  assert_xy(x, "x")
  assert_xy(y, "y")
  assert_name(id_x, "id_x")
  assert_name(id_y, "id_y")
  assert_logic(sparse, "sparse")
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")

  ## Validate predicate early (it aborts on invalid)
  st_predicate <- get_st_predicate(predicate)


  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (CRS and geometry column name)
  ## this step should be before normalize_spatial_input()
  crs_x <- if (is.null(conn_x)) ddbs_crs(x, conn) else ddbs_crs(x, conn_x)
  crs_y <- if (is.null(conn_y)) ddbs_crs(y, conn) else ddbs_crs(y, conn_y)
  sf_col_x <- attr(x, "sf_column")
  sf_col_y <- attr(y, "sf_column")

  ## 1.2. Resolve conn_x/conn_y defaults from 'conn' for character inputs
  if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
  if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

  ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names
  x <- normalize_spatial_input(x, conn_x)
  y <- normalize_spatial_input(y, conn_y)

  ## 1.4. Get mode - If it's NULL, it will use the duckspatial.mode option
  mode <- get_mode(mode, name)


  # 2. Manage connection to DB

  ## 2.1. Resolve connections and handle imports
  resolve_conn <- resolve_spatial_connections(x, y, conn, conn_x, conn_y)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  y            <- resolve_conn$y
  ## register cleanup of the connection
  if (any(is.null(conn_x), is.null(conn_y))) {
      on.exit(resolve_conn$cleanup(), add = TRUE)   
  }

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

  
 # 4. Build predicate expression
  if (st_predicate == "ST_DWithin") {

    ## Warn if the distance arg wasn't specified, and give it a value of 0
    if (is.null(distance)) {
      cli::cli_warn("{.val distance} wasn't specified. Using ST_Within.")
      distance <- 0
    }
    
    ## check the CRS units to use the right function
    crs_units <- crs_x$units_gdal
    if (crs_units != "metre") {
      predicate_expr <- glue::glue("ST_DWithin_Spheroid(x.{x_geom}, y.{y_geom}, {distance})")
      if (crs_x$input != "EPSG:4326") {
        cli::cli_warn(
          "Inputs are in {.val {crs_x$input}}, not {.val EPSG:4326}. Distance calculations may be less accurate. Consider transforming to {.val EPSG:4326} or a projected CRS."
        )
      }
    } else {
      predicate_expr <- glue::glue("ST_DWithin(x.{x_geom}, y.{y_geom}, {distance})")
    }
    
    
  } else {
    predicate_expr <- glue::glue("{st_predicate}(x.{x_geom}, y.{y_geom})")
  }


  # 5. Build query and return based on mode
  ## - mode sf: it will return a list-like object
  ## - mode duckspatial: it will return a lazy-tbl object
  if (mode == "sf") {
    
    ## materialize full predicate matrix and reframe as sf/sparse when required
    tmp.query <- glue::glue("
      SELECT {predicate_expr} AS predicate
      FROM {x_list$query_name} x
      CROSS JOIN {y_list$query_name} y
    ")
    
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)
    
    result <- reframe_predicate_data(
      conn   = target_conn,
      data   = data_tbl,
      x_list = x_list,
      y_list = y_list,
      id_x   = id_x,
      id_y   = id_y,
      sparse = sparse
    )
    
  } else if (mode == "duckspatial") {
    
    ## Resolve identifiers
    x_id_expr <- if (is.null(id_x)) "row_number() OVER () AS id_x" else glue::glue("{id_x} AS id_x")
    y_id_expr <- if (is.null(id_y)) "row_number() OVER () AS id_y" else glue::glue("{id_y} AS id_y")

    ## Name for the table to be created
    view_name <- ddbs_temp_view_name()

     if (sparse) {
    
      ## long format - only TRUE pairs
      tmp.query <- glue::glue("
        CREATE TEMP TABLE {view_name} AS
        SELECT 
          x.id_x,
          y.id_y
        FROM (SELECT {x_id_expr}, * FROM {x_list$query_name}) x
        CROSS JOIN (SELECT {y_id_expr}, * FROM {y_list$query_name}) y
        WHERE {predicate_expr}
      ")
      
    } else {
      
      ## Wide format - all pairs with TRUE/FALSE
      ## need to fetch y_ids eagerly to build pivot columns
      y_ids <- DBI::dbGetQuery(
        target_conn,
        glue::glue("SELECT {y_id_expr} FROM {y_list$query_name}")
      )$id_y
      
      pivot_list <- paste(
        glue::glue("SUM(CASE WHEN id_y = '{y_ids}' AND predicate THEN 1 ELSE 0 END)::BOOLEAN AS \"{y_ids}\""),
        collapse = ",\n"
      )
      
      ## Generate the query
      tmp.query <- glue::glue("
        CREATE TEMP TABLE {view_name} AS
        WITH long AS (
          SELECT 
            x.id_x,
            y.id_y,
            {predicate_expr} AS predicate
          FROM (SELECT {x_id_expr}, * FROM {x_list$query_name}) x
          CROSS JOIN (SELECT {y_id_expr}, * FROM {y_list$query_name}) y
        )
        SELECT 
          id_x,
          {pivot_list}
        FROM long
        GROUP BY id_x
        ORDER BY id_x
      ")
      
     }
    
    ## Create a table, and return a pointer to that table
    DBI::dbExecute(target_conn, tmp.query)
    result <- dplyr::tbl(target_conn, view_name)
    
  }

  return(result)

}




#' @rdname ddbs_predicate
ddbs_intersects <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "intersects", ...)
}

#' @rdname ddbs_predicate
ddbs_covers <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "covers", ...)
}

#' @rdname ddbs_predicate
ddbs_touches <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "touches", ...)
}

#' @rdname ddbs_predicate
ddbs_is_within_distance <- function(x, y, distance = NULL, ...) {
  ddbs_predicate(x = x, y = y, predicate = "dwithin", distance = distance, ...)
}

#' @rdname ddbs_predicate
ddbs_disjoint <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "disjoint", ...)
}

#' @rdname ddbs_predicate
ddbs_within <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "within", ...)
}

#' @rdname ddbs_predicate
ddbs_contains <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "contains", ...)
}

#' @rdname ddbs_predicate
ddbs_overlaps <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "overlaps", ...)
}

#' @rdname ddbs_predicate
ddbs_crosses <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "crosses", ...)
}

#' @rdname ddbs_predicate
ddbs_equals <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "equals", ...)
}

#' @rdname ddbs_predicate
ddbs_covered_by <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "covered_by", ...)
}

#' @rdname ddbs_predicate
ddbs_intersects_extent <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "intersects_extent", ...)
}

#' @rdname ddbs_predicate
ddbs_contains_properly <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "contains_properly", ...)
}

#' @rdname ddbs_predicate
ddbs_within_properly <- function(x, y, ...) {
  ddbs_predicate(x = x, y = y, predicate = "within_properly", ...)
}



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
#'
#' ## Load packages
#' library(duckspatial)
#' library(dplyr)
#' library(sf)
#'
#' ## create in-memory DuckDB database
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ## Store in DuckDB
#' ddbs_write_vector(conn, countries_sf, "countries")
#' ddbs_write_vector(conn, rivers_sf, "rivers")
#'
#' ## Example 1: Check which rivers intersect each country
#' ddbs_predicate(countries_sf, rivers_sf, predicate = "intersects", conn)
#'
#' ## Example 2: Find neighboring countries
#' ddbs_predicate(countries_sf, countries_sf, predicate = "touches",
#'                id_x = "NAME_ENGL", id_y = "NAME_ENGL")
#'
#' ## Example 3: Find rivers that don't intersect countries
#' ddbs_predicate(countries_sf, rivers_sf, predicate = "disjoint",
#'                id_x = "NAME_ENGL", id_y = "RIVER_NAME")
#'
#' ## Example 4: Use table names inside duckdb
#' ddbs_predicate("countries", "rivers", predicate = "within", conn, "NAME_ENGL")
ddbs_predicate <- function(
  x,
  y,
  predicate = "intersects",
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  distance = NULL,
  quiet = FALSE) {

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_xy(y, "y")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x, y)

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

  ## get predicate
  st_predicate <- get_st_predicate(predicate)

  # 3. Get data frame
  ## 3.1. create query
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
  ## 3.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(conn, tmp.query)

  # 4. Reframe data
  result_lst <- reframe_predicate_data(
    conn   = conn,
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





#' Spatial intersects predicate
#'
#' Tests if geometries in `x` intersect geometries in `y`. Returns `TRUE` if
#' geometries share at least one point in common.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "intersects"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' intersect the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_intersects(countries_sf, rivers_sf, id_x = "NAME_ENGL")
ddbs_intersects <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "intersects",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial covers predicate
#'
#' Tests if geometries in `x` cover geometries in `y`. Returns `TRUE` if
#' geometry `x` completely covers geometry `y` (no point of `y` lies outside `x`).
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "covers"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' are covered by the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_covers(countries_sf, rivers_sf, id_x = "NAME_ENGL")
ddbs_covers <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "covers",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial touches predicate
#'
#' Tests if geometries in `x` touch geometries in `y`. Returns `TRUE` if
#' geometries share a boundary but their interiors do not intersect.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "touches"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' touch the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial"))
#' countries_filter_sf <- countries_sf |> filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' # Find neighboring countries
#' ddbs_touches(countries_filter_sf, countries_sf, id_x = "NAME_ENGL", id_y = "NAME_ENGL")
ddbs_touches <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "touches",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}






#' Within Distance predicate
#'
#' Tests if geometries in `x` are within a specified distance of `y`. Returns
#' `TRUE` if geometries are within the distance.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @param distance a numeric value specifying the distance for ST_DWithin. Units correspond to
#' the coordinate system of the geometry (e.g. degrees or meters)
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "dwithin"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' touch the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial"))
#' countries_filter_sf <- countries_sf |> filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' ## check countries within 1 degree of distance
#' ddbs_is_within_distance(countries_filter_sf, countries_sf, 1)
ddbs_is_within_distance <- function(
  x,
  y,
  distance = NULL,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "dwithin",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    distance  = distance,
    quiet     = quiet
  )

}





#' Spatial disjoint predicate
#'
#' Tests if geometries in `x` are disjoint from geometries in `y`. Returns `TRUE`
#' if geometries have no points in common.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "disjoint"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' are disjoint from the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_disjoint(countries_sf, rivers_sf, id_x = "NAME_ENGL")
ddbs_disjoint <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(x, y, "disjoint", conn, id_x, id_y, sparse, quiet)

}





#' Spatial within predicate
#'
#' Tests if geometries in `x` are within geometries in `y`. Returns `TRUE` if
#' geometry `x` is completely inside geometry `y`.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "within"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' contain the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_within(rivers_sf, countries_sf, id_x = "RIVER_NAME", id_y = "NAME_ENGL")
ddbs_within <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "within",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial contains predicate
#'
#' Tests if geometries in `x` contain geometries in `y`. Returns `TRUE` if
#' geometry `x` completely contains geometry `y`.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "contains"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' are contained by the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_contains(countries_sf, rivers_sf, id_x = "NAME_ENGL", id_y = "RIVER_NAME")
ddbs_contains <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "contains",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial overlaps predicate
#'
#' Tests if geometries in `x` overlap geometries in `y`. Returns `TRUE` if
#' geometries share some but not all points, and the intersection has the same
#' dimension as the geometries.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "overlaps"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' overlap the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' spain_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID == c("PT", "ES", "FR", "FI"))
#'
#' ddbs_overlaps(countries_sf, countries_sf)
ddbs_overlaps <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "overlaps",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial crosses predicate
#'
#' Tests if geometries in `x` cross geometries in `y`. Returns `TRUE` if
#' geometries have some but not all interior points in common.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "crosses"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' cross the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_crosses(rivers_sf, countries_sf, id_x = "RIVER_NAME", id_y = "NAME_ENGL")
ddbs_crosses <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "crosses",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial equals predicate
#'
#' Tests if geometries in `x` are spatially equal to geometries in `y`. Returns
#' `TRUE` if geometries are topologically equivalent (same shape and location).
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "equals"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' are equal to the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' ddbs_equals(countries_sf, countries_sf, id_x = "NAME_ENGL")
ddbs_equals <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "equals",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial covered by predicate
#'
#' Tests if geometries in `x` are covered by geometries in `y`. Returns `TRUE` if
#' geometry `x` is completely covered by geometry `y` (no point of `x` lies
#' outside `y`).
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "covered_by"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' cover the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_covered_by(rivers_sf, countries_sf, id_x = "RIVER_NAME", id_y = "NAME_ENGL")
ddbs_covered_by <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "covered_by",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial intersects extent predicate
#'
#' Tests if the bounding box of geometries in `x` intersect the bounding box of
#' geometries in `y`. Returns `TRUE` if the extents (bounding boxes) overlap.
#' This is faster than full geometry intersection but less precise.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "intersects_extent"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` whose
#' bounding box intersects the bounding box of the corresponding geometry in `x`.
#' See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' # Fast bounding box intersection check
#' ddbs_intersects_extent(countries_sf, rivers_sf, id_x = "NAME_ENGL")
ddbs_intersects_extent <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "intersects_extent",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial contains properly predicate
#'
#' Tests if geometries in `x` properly contain geometries in `y`. Returns `TRUE`
#' if geometry `y` is completely inside geometry `x` and does not touch its
#' boundary.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "contains_properly"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' are properly contained by the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_contains_properly(countries_sf, rivers_sf, id_x = "NAME_ENGL", id_y = "RIVER_NAME")
ddbs_contains_properly <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "contains_properly",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}





#' Spatial within properly predicate
#'
#' Tests if geometries in `x` are properly within geometries in `y`. Returns
#' `TRUE` if geometry `x` is completely inside geometry `y` and does not touch
#' its boundary.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @template conn_null
#' @template predicate_args
#' @template quiet
#'
#' @details
#' This is a convenience wrapper around [`ddbs_predicate()`] with
#' `predicate = "within_properly"`.
#'
#' @returns
#' A list where each element contains indices (or IDs) of geometries in `y` that
#' properly contain the corresponding geometry in `x`. See [`ddbs_predicate()`] for details.
#'
#' @seealso [ddbs_predicate()] for other spatial predicates.
#'
#' @export
#'
#' @examples
#'
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' library(sf)
#'
#' ## read countries data, and rivers
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
#'   st_transform(st_crs(countries_sf))
#'
#' ddbs_within_properly(countries_sf, rivers_sf, id_x = "NAME_ENGL", id_y = "RIVER_NAME")
ddbs_within_properly <- function(
  x,
  y,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE) {

  ddbs_predicate(
    x         = x,
    y         = y,
    predicate = "within_properly",
    conn      = conn,
    id_x      = id_x,
    id_y      = id_y,
    sparse    = sparse,
    quiet     = quiet
  )

}









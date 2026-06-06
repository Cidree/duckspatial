#' Locate geometries at specific M values
#'
#' Return the geometries at a specific M values or range of M values.
#'
#' @template x
#' @param measure A numeric value specifying the M value at which to locate a
#'   point along the geometry. Used only by \code{ddbs_locate_along}.
#' @param start_measure A numeric value specifying the lower bound of the M
#'   range.
#' @param end_measure A numeric value specifying the upper bound of the M
#'   range.
#' @param offset A numeric value specifying a lateral offset to apply
#'   perpendicular to the line direction at the located point(s). Default is
#'   \code{0} (no offset).
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' 
#' @details
#' 
#' - `ddbs_locate_along()`: returns a point or multi-point, containing the point(s) 
#'   at the geometry with the given measure
#' 
#' - `ddbs_locate_between()`: returns a geometry or geometry collection created by 
#'   filtering and interpolating vertices within a range of "M" values
#'
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## read data (must contain M-enabled linestring geometries)
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## Calculate the length of the rivers
#' rivers_agg_ddbs <- rivers_ddbs |> 
#'   ddbs_union_agg("RIVER_NAME") |> 
#'   ddbs_length()
#' 
#' ## Add M dimension to the rivers
#' rivers_m_ddbs <- rivers_agg_ddbs |> 
#'   ddbs_force_3d("length", dim = "M")
#' 
#' ## Locate rivers with M between 10000 and 20000
#' ddbs_locate_between(rivers_m_ddbs, 10000, 20000)
#' 
#' }
#' @name ddbs_locate
#' @rdname ddbs_locate
NULL



#' @rdname ddbs_locate
#' @export
ddbs_locate_along <- function(
    x,
    measure,
    offset = 0,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Validate inputs
    assert_xy(x, "x")
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_numeric(measure, "measure")
    assert_numeric(offset, "offset")


    # 1. Prepare inputs
    
    ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names)
    x <- normalize_spatial_input(x, conn)

    ## 1.2. Pre-extract attributes
    crs_x    <- ddbs_crs(x, conn)
    sf_col_x <- attr(x, "sf_column")
    mode     <- get_mode(mode, name)

    ## 1.3. Resolve spatial connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 1.4. Get list with query names for the input data
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 2. Prepare the query

    ## 2.1. Get the geometry column name (try to extract from attributes, if not 
    ## available get it from the database)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 2.2. Build the function arguments
    args <- sprintf(
        "%s, %s",
        x_geom,
        glue::glue("{measure}, {offset}")
    )

    ## 2.3. Build the base query (depends on the output type - sf, duckspatial_df, table)
    ## We drop empty geometries. Two steps are required:
    ## (1) apply ST_LocateAlong keeping the result as raw GEOMETRY so ST_IsEmpty works,
    ## (2) filter empties, then convert to the requested output format in the outer SELECT.
    st_function <- glue::glue("ST_LocateAlong({args})")
    base.query <- glue::glue("
        WITH located AS (
          SELECT * REPLACE ({st_function} AS {x_geom})
          FROM {x_list$query_name}
        )
        SELECT * REPLACE ({build_geom_query(x_geom, name, crs_x, mode)} AS {x_geom})
        FROM located
        WHERE NOT ST_IsEmpty({x_geom});
      ")
    


    # 3. Table creation if name is provided, or 
    # create duckspatial_df or sf object if name is NULL
    if (!is.null(name)) {
        create_duckdb_table(
          conn      = target_conn,
          name      = name,
          query     = base.query,
          overwrite = overwrite,
          quiet     = quiet
        )
    } else {
      ddbs_handle_query(
        query  = base.query,
        conn   = target_conn,
        mode   = mode,
        crs    = crs_x,
        x_geom = x_geom
      )
    }

}




#' @rdname ddbs_locate
#' @export
ddbs_locate_between <- function(
  x,
  start_measure,
  end_measure,
  offset = 0,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_x_name(conn, x, name)
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  assert_numeric(start_measure, "start_measure")
  assert_numeric(end_measure, "end_measure")
  assert_numeric(offset, "offset")


  # 1. Prepare inputs
  
  ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names)
  x <- normalize_spatial_input(x, conn)

  ## 1.2. Pre-extract attributes
  crs_x    <- ddbs_crs(x, conn)
  sf_col_x <- attr(x, "sf_column")
  mode     <- get_mode(mode, name)

  ## 1.3. Resolve spatial connections and handle imports
  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  ## register cleanup of the connection
  on.exit(resolve_conn$cleanup(), add = TRUE)

  ## 1.4. Get list with query names for the input data
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)


  # 2. Prepare the query

  ## 2.1. Get the geometry column name (try to extract from attributes, if not 
  ## available get it from the database)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  ## 2.2. Build the function arguments
  args <- sprintf(
    "%s, %s",
    x_geom,
    glue::glue("{start_measure}, {end_measure}, {offset}")
  )

  ## 2.3. Build the base query (depends on the output type - sf, duckspatial_df, table)
  ## We drop empty geometries. Two steps are required:
  ## (1) apply ST_LocateBetween keeping the result as raw GEOMETRY so ST_IsEmpty works,
  ## (2) filter empties, then convert to the requested output format in the outer SELECT.
  st_function <- glue::glue("ST_LocateBetween({args})")
  base.query <- glue::glue("
    WITH located AS (
      SELECT * REPLACE ({st_function} AS {x_geom})
      FROM {x_list$query_name}
    )
    SELECT * REPLACE ({build_geom_query(x_geom, name, crs_x, mode)} AS {x_geom})
    FROM located
    WHERE NOT ST_IsEmpty({x_geom});
  ")
  


  # 3. Table creation if name is provided, or 
  # create duckspatial_df or sf object if name is NULL
  if (!is.null(name)) {
    create_duckdb_table(
      conn      = target_conn,
      name      = name,
      query     = base.query,
      overwrite = overwrite,
      quiet     = quiet
    )
  } else {
    ddbs_handle_query(
      query  = base.query,
      conn   = target_conn,
      mode   = mode,
      crs    = crs_x,
      x_geom = x_geom
    )
  }

}
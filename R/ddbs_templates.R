

#' Template for unary functions without extra arguments (e.g. ST_Centroid)
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#' @param fun The duckdb function to use
#' @param other_params string with other function-specific parameters
#' 
#' @template returns_mode
#' @keywords internal
#' @noRd
template_unary_ops <- function(
  x,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE,
  fun,
  other_args = NULL) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_x_name(conn, x, name)
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")


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
  ## if other_args is NULL, use only the geometry column name
  ## if is not NULL, append the rest of the function arguments
  if (is.null(other_args)) {
    args <- x_geom
  } else {
    args <- sprintf(
      "%s, %s",
      x_geom,
      other_args
    )
  }

  ## 2.3. Other function-specific handling
  ## - ST_Buffer, check the units and warn if they aren't in meters
  if (tolower(fun) == "st_buffer") {
    crs_units <- crs_x$units_gdal
    if (crs_units != "metre") cli::cli_warn("The input CRS is in {crs_units}s. This function calculates the buffer in those units.")
  }

  ## 2.4. Build the base query (depends on the output type - sf, duckspatial_df, table)
  st_function <- glue::glue("{fun}({args})")
  base.query <- glue::glue("
    SELECT *
    REPLACE ({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom})
    FROM {x_list$query_name};
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






#' Template for geom converstion funs (e.g. ST_AsWKB)
#'
#' @template x
#' @template conn_null
#' @param fun The duckdb function to use
#' 
#' @returns Character vector or list
#' @keywords internal
#' @noRd
template_geometry_conversion <- function(
  x,
  conn = NULL,
  fun
) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_character(conn, x)


  # 1. Prepare inputs
  
  ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names)
  x <- normalize_spatial_input(x, conn)

  ## 1.2. Pre-extract attributes
  sf_col_x <- attr(x, "sf_column")

  ## 1.3. Resolve spatial connections and handle imports
  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = TRUE)
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


  # 3. Create the query and retrieve the results

  ## 3.1. create query
  tmp.query <- glue::glue("
      SELECT {fun}({x_geom}) as geometry
      FROM {x_list$query_name};
  ")

  ## 3.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)
  data_vec <- data_tbl$geometry

  return(data_vec)

}





#' Template for measure functions that return a vector (e.g. ST_Area)
#'
#' @template x
#' @template conn_null
#' @template name
#' @template new_column
#' @template mode
#' @template overwrite
#' @template quiet
#' @param fun The duckdb function to use
#' 
#' @returns When `new_column = NULL` it returns a `units` vector in \eqn{m^2}. When `new_column` is not NULL, the
#' output depends on the \code{mode} argument (or global preference set by \code{\link{ddbs_options}}):
#'   \itemize{
#'     \item \code{duckspatial} (default): A \code{duckspatial_df} (lazy spatial data frame) backed by dbplyr/DuckDB.
#'     \item \code{sf}: An eagerly collected \code{sf} object in R memory.
#' }
#' When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).
#' 
#' @keywords internal
#' @noRd
template_measure <- function(
  x,
  conn = NULL,
  name = NULL,
  new_column = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE,
  fun = c("ST_Area", "ST_Length", "ST_Perimeter")) {
  
  # Match and validate fun
  fun <- match.arg(fun)

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_x_name(conn, x, name)
  assert_conn_character(conn, x)
  assert_name(name)
  assert_character_scalar(new_column, "new_column")
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  
  if (!is.null(name) && is.null(new_column)) {
      cli::cli_abort("Please, specify the {.arg new_column} name.")
  }

  # 1. Prepare inputs
  
  ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names)
  x <- normalize_spatial_input(x, conn)

  ## 1.2. Pre-extract attributes
  crs_x     <- ddbs_crs(x, conn)
  sf_col_x  <- attr(x, "sf_column")
  mode      <- get_mode(mode, name)
  crs_units <- crs_x$units_gdal

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

  ## 3.2. Warn if the units aren't meters or EPSG:4326
  ## for EPSG:4326, we can use ST_*_Spheroid to get the measurement in meters
  ## so that will be an exception
  if (crs_units != "metre" && "EPSG:4326" != crs_x$input) {
      cli::cli_warn(
        "Input is in {.val {crs_x$input}}, not {.val EPSG:4326}. {fun} calculations may be less accurate. Consider transforming to {.val EPSG:4326} or a projected CRS."
      )
  }

  ## 3.3. Build the appropriate ST function based on fun and CRS
  ## Use spheroid version for geographic coordinates
  if (crs_units == "metre") {
    st_function <- glue::glue("{fun}({x_geom})")
  } else {
    # st_function <- glue::glue("{fun}_Spheroid({x_geom})") # when the issue #109 is solved
    st_function <- glue::glue("{fun}_Spheroid(ST_FlipCoordinates({x_geom}))")
  }
  
  ## 3.4. Determine units for output
  output_units <- switch(
    fun,
    "ST_Area"      = "m^2",
    "ST_Length"    = "metre",
    "ST_Perimeter" = "metre"
  )

  ## 3.5. Build the base query. For sf we will return an units vector
  if (mode == "sf") {
    base.query <- glue::glue("
      SELECT {st_function} AS {new_column}
      FROM {x_list$query_name};
    ")
  } else {
    base.query <- glue::glue("
      SELECT 
        * REPLACE ({build_geom_query(x_geom, name, crs_x, mode)} AS {x_geom}),
        {st_function} AS {new_column}
      FROM 
        {x_list$query_name};
    ")
  }


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
      query      = base.query,
      conn       = target_conn,
      mode       = mode,
      crs        = crs_x,
      x_geom     = x_geom,
      fun_group  = 2,
      units      = output_units
    )
  }

}






#' Template for unary functions without extra arguments (e.g. ST_IsValid)
#'
#' @template x
#' @template by_feature
#' @template conn_null
#' @template name
#' @template new_column
#' @template mode
#' @template overwrite
#' @template quiet
#' @param fun The duckdb function to use
#' 
#' @returns
#' \itemize{
#'   \item \code{mode = "duckspatial"} (default): A \code{duckspatial_df} (lazy spatial data frame) backed by dbplyr/DuckDB.
#'   \item \code{mode = "sf"}: An eagerly collected vector in R memory.
#'   \item When \code{name} is provided: writes the table in the DuckDB connection and returns \code{TRUE} (invisibly).
#' }
#' 
#' @keywords internal
#' @noRd
template_new_column <- function(
  x,
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  new_column = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE,
  fun) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_logic(by_feature, "by_feature")
  assert_name(name)
  assert_character_scalar(new_column, "new_column")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  assert_conn_x_name(conn, x, name)
  assert_conn_character(conn, x)

  if (!is.null(name) && is.null(new_column)) cli::cli_abort("Please, specify the {.arg new_column} name.")


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

  ## 2.2. Compute if by_feature = FALSE (returns always a single value)
  ## - For functions ST_Has*() - if 1 is TRUE, return TRUE
  ## - For functions ST_is_*() - if 1 is FALSE, return FALSE
  if (isFALSE(by_feature)) {
    ## Create the query
    tmp.query <- glue::glue("
      SELECT {fun}({x_geom}) as {new_column}
      FROM {x_list$query_name};
    ")

    ## Retrieve the data
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

    ## Return TRUE if any is TRUE
    if (tolower(fun) %in% c("st_hasz", "st_hasm")) {
      return(any(data_tbl[1, ]))
    } else {
      return(all(data_tbl[1, ]))
    }
  }

  ## 2.3. Build the base query (depends on the output type - sf, duckspatial_df, table)
  st_function <- glue::glue("{x_geom}")

  if (mode == "sf") {
    base.query <- glue::glue("
      SELECT {fun}({x_geom}) as {new_column},
      FROM {x_list$query_name};
    ")
  } else {
    base.query <- glue::glue("
      SELECT 
        * REPLACE ({build_geom_query(x_geom, name, crs_x, mode)} AS {x_geom}),
        {fun}({x_geom}) AS {new_column}
      FROM 
        {x_list$query_name};
    ")
  }


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
      x_geom = x_geom,
      fun_group  = 2,
      units      = NULL
    )
  }

}

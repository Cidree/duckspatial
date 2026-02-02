#' Template for geometry validation functions (e.g. ST_IsValid)
#'
#' @template x
#' @template conn_null
#' @template crs
#' @template quiet
#' @param fun The duckdb function to use
#' 
#' @returns A logical vector
#' @keywords internal
#' @noRd
template_geometry_validation <- function(
  x,
  conn = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE,
  fun) {
  
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
    SELECT {fun}({x_geom}) as res,
    FROM {x_list$query_name}
  ")

  ## 3.3. Get results
  data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
  feedback_query(quiet)
  return(data_vec[, 1])
  
}





#' Template for unary functions without extra arguments (e.g. ST_Centroid)
#'
#' @template x
#' @template conn_null
#' @template name
#' @template crs
#' @template output
#' @template overwrite
#' @template quiet
#' @param fun The duckdb function to use
#' @param other_params string with other function-specific parameters
#' 
#' @template returns_output
#' @keywords internal
#' @noRd
template_unary_ops <- function(
    x,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE,
    fun,
    other_args = NULL) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(output, "output")
    assert_logic(overwrite, "overwrite")
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


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)
  
    ## 3.3. Append all args
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
  
    ## 3.4. Function-specific handling
    if (tolower(fun) == "st_buffer") {
      crs_units <- crs_x$units_gdal
      if (crs_units != "metre") cli::cli_warn("The input CRS is in {crs_units}s. This function calculates the buffer in those units.")
    }
  
  
    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            {fun}({args}) as {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

  
    # 5. Get data frame
  
    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_AsWKB({fun}({args})) as {x_geom}
        FROM {x_list$query_name};
    ")
  
    ## 5.2. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

  
    # 6. convert to SF and return result
    data_sf <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}






#' Template for geom converstion funs (e.g. ST_AsWKB)
#'
#' @template x
#' @template conn_null
#' @template quiet
#' @param fun The duckdb function to use
#' 
#' @returns Character vector or list
#' @keywords internal
#' @noRd
template_geometry_conversion <- function(
  x,
  conn = NULL,
  quiet = FALSE,
  fun
) {

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)


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


  # 3. Prepare parameters for the query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)


  # 4. Get data as vector

  ## 4.1. create query
  tmp.query <- glue::glue("
      SELECT {fun}({x_geom}) as geometry
      FROM {x_list$query_name};
  ")

  ## 4.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)
  data_vec <- data_tbl$geometry

  feedback_query(quiet)
  return(data_vec)

}





#' Template for measure functions that return a vector (e.g. ST_Area)
#'
#' @template x
#' @template conn_null
#' @template name
#' @template new_column
#' @template crs
#' @template output
#' @template overwrite
#' @template quiet
#' @param fun The duckdb function to use
#' 
#' @returns When `new_column = NULL` it returns a `units` vector in meters. When `new_column` is not NULL, the
#' output depends on the \code{output} argument (or global preference set by \code{\link{ddbs_options}}):
#'   \itemize{
#'     \item \code{duckspatial_df} (default): A lazy spatial data frame backed by dbplyr/DuckDB.
#'     \item \code{sf}: An eagerly collected \code{sf} object in R memory.
#'     \item \code{tibble}: An eagerly collected \code{tibble} without geometry in R memory.
#'     \item \code{raw}: An eagerly collected \code{tibble} with WKB geometry (no conversion).
#'     \item \code{geoarrow}: An eagerly collected \code{tibble} with geometry converted to \code{geoarrow_vctr}.
#'   }
#'   When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).
#' 
#' @keywords internal
#' @noRd
template_measure <- function(
  x,
  conn = NULL,
  name = NULL,
  new_column = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = NULL,
  overwrite = FALSE,
  quiet = FALSE,
  fun = c("ST_Area", "ST_Length", "ST_Perimeter")) {
  
  # Match and validate fun
  fun <- match.arg(fun)
  
  deprecate_crs(crs_column, crs)

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(new_column, "new_column")
  assert_name(output, "output")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  
  if (!is.null(name) && is.null(new_column)) {
      cli::cli_abort("Please, specify the {.arg new_column} name.")
  }

  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (CRS and geometry column name)
  ## this step should be before normalize_spatial_input()
  crs_x    <- ddbs_crs(x, conn)
  sf_col_x <- attr(x, "sf_column")

  ## 1.2. Extract units, and warn if they aren't meters or EPSG:4326
  ## for EPSG:4326, we can use ST_*_Spheroid to get the measurement in meters
  ## so that will be an exception
  crs_units <- crs_x$units_gdal

  if (crs_units != "metre" && "EPSG:4326" != crs_x$input) {
      cli::cli_warn(
        "Input is in {.val {crs_x$input}}, not {.val EPSG:4326}. {fun} calculations may be less accurate. Consider transforming to {.val EPSG:4326} or a projected CRS."
      )
  }

  ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
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

  ## 3.2. Get names of the rest of the columns
  x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

  ## 3.3. Build the appropriate ST function based on fun and CRS
  ## Use spheroid version for geographic coordinates
  if (crs_units == "metre") {
      st_function <- glue::glue("{fun}({x_geom})")
  } else {
      st_function <- glue::glue("{fun}_Spheroid(ST_FlipCoordinates({x_geom}))")
  }
  
  ## 3.4. Determine units for output
  output_units <- switch(
    fun,
    "ST_Area" = "m^2",
    "ST_Length" = "metre",
    "ST_Perimeter" = "metre"
  )


  # 4. Handle new_column = NULL (return vector)
  if (is.null(new_column)) {
      tmp.query <- glue::glue("
          SELECT {st_function} as {fun},
          FROM {x_list$query_name};
      ")
      
      data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
      feedback_query(quiet)
  
      ## get vector, and convert it to units
      data_units <- units::as_units(data_vec[, 1], output_units)
  
      return(data_units)
  }


  # 5. if name is not NULL (create table in database)
  if (!is.null(name)) {

      ## convenient names of table and/or schema.table
      name_list <- get_query_name(name)

      ## handle overwrite
      overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

      ## create query
      tmp.query <- glue::glue("
          CREATE TABLE {name_list$query_name} AS
          SELECT 
            {x_rest}
            {st_function} AS {new_column},
            {x_geom}
          FROM 
            {x_list$query_name};
      ")
      ## execute query
      DBI::dbExecute(target_conn, tmp.query)
      feedback_query(quiet)
      return(invisible(TRUE))
  }

  # 6. Get data frame (default behavior)
  ## 6.1. create query
  tmp.query <- glue::glue("
      SELECT 
        {x_rest}
        {st_function} AS {new_column},
        ST_AsWKB({x_geom}) as {x_geom}
      FROM 
        {x_list$query_name};
  ")
  ## 6.2. retrieve results of the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

  ## 7. convert to target output
  data_sf <- ddbs_handle_output(
      data       = data_tbl,
      conn       = target_conn,
      output     = output,
      crs        = if (!is.null(crs)) crs else crs_x,
      crs_column = crs_column,
      x_geom     = x_geom
  )

  feedback_query(quiet)
  return(data_sf)

}






#' Template for ST_HasZ and ST_HasM
#'
#' @template x
#' @template by_feature
#' @template conn_null
#' @template quiet
#' @param fun The duckdb function to use
#' 
#' @returns Character vector or list
#' @keywords internal
#' @noRd
template_has <- function(
  x,
  by_feature = FALSE,
  conn = NULL,
  quiet = FALSE,
  fun
) {

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_logic(by_feature, "by_feature")
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


  # 3. Query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  ## 3.2. Create the base query
  tmp.query <- glue::glue("
    SELECT {fun}({x_geom}) as hasz
    FROM {x_list$query_name};
  ")

  ## 3.3. Get the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

  ## 3.4. Result depending on by_feature
  feedback_query(quiet)
  if (isTRUE(by_feature)) {
    return(data_tbl$hasz)
  } else {
    return(any(data_tbl$hasz))
  }

}

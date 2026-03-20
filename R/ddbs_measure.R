
#' Calculate geometric measurements
#'
#' Compute area, length, perimeter, or distance of geometries with automatic
#' method selection based on the coordinate reference system (CRS).
#'
#' @name ddbs_measure_funs
#' @rdname ddbs_measure_funs
#' @aliases ddbs_area ddbs_length ddbs_perimeter ddbs_distance
#'
#' @param x Input geometry (sf object, duckspatial_df, or table name in DuckDB)
#' @param y Second input geometry for distance calculations (sf object, duckspatial_df, or table name)
#' @param dist_type Character. Distance type to be calculated. By default it uses
#'   the best option for the input CRS (see details).
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @param id_x Character; optional name of the column in `x` whose values will
#' be used to name the list elements. If `NULL`, integer row numbers of `x` are used.
#' @param id_y Character; optional name of the column in `y` whose values will
#' replace the integer indices returned in each element of the list.
#' @template new_column
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @returns 
#' For \code{ddbs_area}, \code{ddbs_length}, and \code{ddbs_perimeter}:
#' \itemize{
#'   \item \code{mode = "duckspatial"} (default): A \code{duckspatial_df} (lazy spatial data frame) backed by dbplyr/DuckDB.
#'   \item \code{mode = "sf"}: An eagerly collected vector in R memory.
#'   \item When \code{name} is provided: writes the table in the DuckDB connection and returns \code{TRUE} (invisibly).
#' }
#' 
#' For \code{ddbs_distance}: A \code{units} matrix in meters with dimensions nrow(x), nrow(y).
#'
#' @details
#' These functions automatically select the appropriate calculation method based on the input CRS:
#'
#' \strong{For EPSG:4326 (geographic coordinates):}
#' \itemize{
#'   \item Uses \code{ST_*_Spheroid} functions (e.g., \code{ST_Area_Spheroid}, \code{ST_Length_Spheroid})
#'   \item Leverages GeographicLib library for ellipsoidal earth model calculations
#'   \item Highly accurate but slower than planar calculations
#'   \item For \code{ddbs_distance} with POINT geometries: defaults to \code{"haversine"}
#'   \item For \code{ddbs_distance} with other geometries: defaults to \code{"spheroid"}
#' }
#'
#' \strong{For projected CRS (e.g., UTM, Web Mercator):}
#' \itemize{
#'   \item Uses planar \code{ST_*} functions (e.g., \code{ST_Area}, \code{ST_Length})
#'   \item Faster performance with accurate results in meters
#'   \item For \code{ddbs_distance}: defaults to \code{"planar"}
#' }
#'
#' \strong{Distance calculation methods} (\code{dist_type} argument):
#' \itemize{
#'   \item \code{NULL} (default): Automatically selects best method for input CRS
#'   \item \code{"planar"}: Planar distance (for projected CRS)
#'   \item \code{"geos"}: Planar distance using GEOS library (for projected CRS)
#'   \item \code{"haversine"}: Great circle distance (requires EPSG:4326 and POINT geometries)
#'   \item \code{"spheroid"}: Ellipsoidal model using GeographicLib (most accurate, slowest)
#' }
#'
#' \strong{Distance type requirements:}
#' \itemize{
#'   \item \code{"planar"} and \code{"geos"}: Require projected coordinates (not degrees)
#'   \item \code{"haversine"} and \code{"spheroid"}: Require POINT geometries and EPSG:4326
#' }
#'
#' @section Performance:
#' Speed comparison (fastest to slowest):
#' \enumerate{
#'   \item Planar calculations on projected CRS
#'   \item Haversine (spherical approximation)
#'   \item Spheroid functions (ellipsoidal model)
#' }
#'
#' @references \url{https://geographiclib.sourceforge.io/}
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#' library(dplyr)
#' 
#' # Create a DuckDB connection
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' # ===== AREA CALCULATIONS =====
#' 
#' # Load polygon data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", package = "duckspatial")
#' ) |>
#'   ddbs_transform("EPSG:3857") |> 
#'   filter(NAME_ENGL != "Antarctica")
#' 
#' # Store in DuckDB
#' ddbs_write_table(conn, countries_ddbs, "countries")
#' 
#' # Calculate area (adds a new column - area by default)
#' ddbs_area("countries", conn)
#' 
#' # Calculate area with custom column name
#' ddbs_area("countries", conn, new_column = "area_sqm")
#' 
#' # Create new table with area calculations
#' ddbs_area("countries", conn, name = "countries_with_area", new_column = "area_sqm")
#' 
#' # Calculate area from sf object directly
#' ddbs_area(countries_ddbs)
#' 
#' # Calculate area using dplyr syntax
#' countries_ddbs |> 
#'   mutate(area = ddbs_area(geom))
#' 
#' # Calculate total area 
#' countries_ddbs |> 
#'   mutate(area = ddbs_area(geom)) |> 
#'   summarise(
#'     area = sum(area),
#'     geom = ddbs_union(geom)
#'   )
#' 
#' # ===== LENGTH CALCULATIONS =====
#' 
#' # Load line data
#' rivers_ddbs <- sf::read_sf(
#'   system.file("spatial/rivers.geojson", package = "duckspatial")
#' ) |> 
#'   as_duckspatial_df()
#' 
#' # Store in DuckDB
#' ddbs_write_table(conn, rivers_ddbs, "rivers")
#' 
#' # Calculate length (add a new column - length by default)
#' ddbs_length("rivers", conn)
#' 
#' # Calculate length with custom column name
#' ddbs_length(rivers_ddbs, new_column = "length_meters")
#' 
#' # Calculate length by river name
#' rivers_ddbs |> 
#'   ddbs_union_agg("RIVER_NAME") |> 
#'   ddbs_length()
#' 
#' # Add length within dplyr
#' rivers_ddbs |> 
#'   mutate(length = ddbs_length(geometry))
#' 
#' 
#' # ===== PERIMETER CALCULATIONS =====
#' 
#' # Calculate perimeter (returns sf object with perimeter column)
#' ddbs_perimeter(countries_ddbs)
#' 
#' # Calculate perimeter within dplyr
#' countries_ddbs |> 
#'   mutate(perim = ddbs_perimeter(geom))
#' 
#' 
#' # ===== DISTANCE CALCULATIONS =====
#' 
#' # Create sample points in EPSG:4326
#' n <- 10
#' points_sf <- data.frame(
#'   id = 1:n,
#'   x = runif(n, min = -180, max = 180),
#'   y = runif(n, min = -90, max = 90)
#' ) |>
#'   ddbs_as_spatial(coords = c("x", "y"), crs = "EPSG:4326")
#' 
#' # Option 1: Using sf objects (auto-selects haversine for EPSG:4326 points)
#' dist_matrix <- ddbs_distance(x = points_sf, y = points_sf)
#' head(dist_matrix)
#' 
#' # Option 2: Explicitly specify distance type
#' dist_matrix_harv <- ddbs_distance(
#'   x = points_sf,
#'   y = points_sf,
#'   dist_type = "haversine"
#' )
#' 
#' # Option 3: Using DuckDB tables
#' ddbs_write_table(conn, points_sf, "points", overwrite = TRUE)
#' dist_matrix_sph <- ddbs_distance(
#'   conn = conn,
#'   x = "points",
#'   y = "points",
#'   dist_type = "spheroid"  # Most accurate for geographic coordinates
#' )
#' head(dist_matrix_sph)
#' 
#' # Close connection
#' ddbs_stop_conn(conn)
#' }
NULL





#' @rdname ddbs_measure_funs
#' @export
ddbs_area <- function(
  x,
  new_column = "area",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
    
  template_measure(
    x = x,
    new_column = new_column,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_Area"
  )
}





#' @rdname ddbs_measure_funs
#' @export
ddbs_length <- function(
  x,
  new_column = "length",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  template_measure(
    x = x,
    new_column = new_column,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_Length"
  )
}





#' @rdname ddbs_measure_funs
#' @export
ddbs_perimeter <- function(
  x,
  new_column = "perimeter",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
    
  template_measure(
    x = x,
    new_column = new_column,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_Perimeter"
  )
}





#' @rdname ddbs_measure_funs
#' @export
ddbs_distance <- function(
  x,
  y,
  dist_type = NULL,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  # 0. Handle errors
  assert_xy(x, "x")
  assert_xy(y, "y")
  assert_name(id_x, "id_x")
  assert_name(id_y, "id_y")
  assert_name(dist_type, "dist_type")
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")


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
  on.exit(resolve_conn$cleanup(), add = TRUE)

  ## 2.2. Get query list of table names
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)
  y_list <- get_query_list(y, target_conn)
  on.exit(y_list$cleanup(), add = TRUE)
  
  ## check if id column name exists in x or y
  assert_predicate_id(id_x, target_conn, x_list$query_name)
  assert_predicate_id(id_y, target_conn, y_list$query_name)

  ## 2.3. CRS already extracted at start of function
  if (!is.null(crs_x) && !is.null(crs_y)) {
    if (!crs_equal(crs_x, crs_y)) {
      cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
    }
  } else {
    assert_crs(target_conn, x_list$query_name, y_list$query_name)
  }

  ## 2.4. Get crs units and geom type for next checks
  crs_units <- crs_x$units_gdal
  geom_type_x <- as.character(ddbs_geometry_type(x, conn = target_conn, by_feature = FALSE))
  geom_type_y <- as.character(ddbs_geometry_type(y, conn = target_conn, by_feature = FALSE))

  ## 2.4. Get the right distance type if user uses the default
  if (is.null(dist_type)) {
    if (crs_units == "degree") {
      ## Default to haversine if it's point and EPSG:4326
      if (crs_x$input == "EPSG:4326" && all(c(geom_type_x, geom_type_y) == "POINT")) {
        dist_type <- "haversine"
      } else {
        ## Default to spheroid if it's not POINT or if it's not EPSG:4326
        dist_type <- "spheroid"
      }
    } else {
      ## Otherwise, default to planar
      dist_type <- "planar"
    }
    if (!quiet) cli::cli_alert_info("Using {.arg dist_type = {.val {dist_type}}} by default.")
  }

  ## 2.5. Warnings/Errors for wrong election of dist_type
  ## Error: using an invalid distance type
  valid_types <- c("planar", "haversine", "geos", "spheroid")
  if (!dist_type %in% valid_types) {
      cli::cli_abort("{.arg dist_type} must be one of {.or {.val {valid_types}}}, not {.val {dist_type}}.")
  }

  ## Error: Using planar/geos on geographic coordinates
  if (crs_units == "degree" && dist_type %in% c("planar", "geos")) {
      cli::cli_abort(
          "When using {.arg dist_type = {.val {dist_type}}}, inputs must be in projected coordinates (e.g., UTM), not geographic (degrees)."
      )
  }

  ## Error: haversine/spheroid require POINT geometries
  if (dist_type %in% c("haversine", "spheroid") && !all(c(geom_type_x, geom_type_y) == "POINT")) {
      cli::cli_abort(
          "When using {.arg dist_type = {.val {dist_type}}}, inputs must be POINT geometries."
      )
  }

  ## Error: Using haversine/spheroid on projected coordinates
  if (crs_units == "metre" && dist_type %in% c("haversine", "spheroid")) {
      cli::cli_abort(
          "When using {.arg dist_type = {.val {dist_type}}}, inputs must be in {.val EPSG:4326} coordinates, not projected coordinates."
      )
  }

  ## Warning: Geographic CRS but not WGS84 (spheroid/haversine might be less accurate)
  if (crs_units == "degree" && dist_type %in% c("haversine", "spheroid") && crs_x$input != "EPSG:4326") {
      cli::cli_warn(
          "Inputs are in {.val {crs_x$input}}, not {.val EPSG:4326}. Distance calculations may be less accurate. Consider transforming to {.val EPSG:4326} or a projected CRS."
      )
  }


  # 3. Prepare parameters for the query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
  assert_geometry_column(x_geom, x_list)
  assert_geometry_column(y_geom, y_list)

  ## 3.2. Select the right DuckD's function
  st_distance_fun <- switch(
    dist_type,
    "planar"    = "ST_Distance",
    "geos"      = "ST_Distance_GEOS",
    "haversine" = "ST_Distance_Sphere",
    "spheroid"  = "ST_Distance_Spheroid"
  )

  ## 3.3. Select the right coordinates order
  # st_distance_fun <- glue::glue("{st_distance_fun}(x.{x_geom}, y.{y_geom})")
  if (dist_type %in% c("haversine", "spheroid")) {
    st_distance_fun <- glue::glue("{st_distance_fun}(ST_FlipCoordinates(x.{x_geom}), ST_FlipCoordinates( y.{y_geom}))")
  } else {
    st_distance_fun <- glue::glue("{st_distance_fun}(x.{x_geom}, y.{y_geom})")
  }
  
  ## 3.2. Create query and get results based on mode
  if (mode == "sf") {

    ## Create the query
    tmp.query <- glue::glue("
      SELECT {st_distance_fun} as distance
      FROM {x_list$query_name} x
      CROSS JOIN {y_list$query_name} y
    ")

    ## Retrieve results
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

    ## Cconvert to matrix
    ## Get number of rows
    nrowx <- get_nrow(target_conn, x_list$query_name)
    nrowy <- get_nrow(target_conn, y_list$query_name)

    ## Convert results to matrix -> to list
    ## Return sparse matrix
    dist_mat  <- matrix(
        data_tbl[["distance"]],
        nrow = nrowx,
        ncol = nrowy,
        byrow = TRUE
    )

    ## Set units and return the resulting matrix
    dist_mat <- units::set_units(dist_mat, "metre")
    return(dist_mat)

  } else {

    ## Subqueries for generating row ids
    x_id_expr <- if (is.null(id_x)) "row_number() OVER () AS id_x" else glue::glue("{id_x} AS id_x")
    y_id_expr <- if (is.null(id_y)) "row_number() OVER () AS id_y" else glue::glue("{id_y} AS id_y")

    ## Generate the query
    view_name <- ddbs_temp_view_name()
    tmp.query <- glue::glue("
        CREATE TEMP TABLE {view_name} AS
        SELECT 
            x.id_x,
            y.id_y,
            {st_distance_fun} AS distance
        FROM (SELECT {x_id_expr}, * FROM {x_list$query_name}) x
        CROSS JOIN (SELECT {y_id_expr}, * FROM {y_list$query_name}) y
    ")

    ## Create a table, and return a pointer to that table
    DBI::dbExecute(target_conn, tmp.query)
    dist_tbl <- dplyr::tbl(target_conn, view_name)
    return(dist_tbl)


  }
  
}

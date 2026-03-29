


#' Creates a buffer around geometries
#'
#' Computes a polygon that represents all locations within a specified distance from the
#' original geometry
#'
#' @template x
#' @param distance a numeric value specifying the buffer distance. Units correspond to
#' the coordinate system of the geometry (e.g. degrees or meters)
#' @param num_triangles an integer representing how many triangles will be produced to 
#' approximate a quarter circle. The larger the number, the smoother the resulting geometry. 
#' Default is 8.
#' @param cap_style a character string specifying the cap style. Must be one of 
#' "CAP_ROUND" (default), "CAP_FLAT", or "CAP_SQUARE". Case-insensitive.
#' @param join_style a character string specifying the join style. Must be one of 
#' "JOIN_ROUND" (default), "JOIN_MITRE", or "JOIN_BEVEL". Case-insensitive.
#' @param mitre_limit a numeric value specifying the mitre limit ratio. Only applies when 
#' \code{join_style} is "JOIN_MITRE". It is the ratio of the distance from the corner to 
#' the mitre point to the corner radius. Default is 1.0.
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## basic buffer
#' ddbs_buffer(conn = conn, "argentina", distance = 1)
#'
#' ## buffer with custom parameters
#' ddbs_buffer(conn = conn, "argentina", distance = 1, 
#'             num_triangles = 16, cap_style = "CAP_SQUARE")
#'
#' ## buffer without using a connection
#' ddbs_buffer(argentina_ddbs, distance = 1)
#' }
ddbs_buffer <- function(
  x,
  distance,
  num_triangles = 8L,
  cap_style = "CAP_ROUND",
  join_style = "JOIN_ROUND",
  mitre_limit = 1.0,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {

  
  # 0. Handle function-specific errors
  assert_numeric(distance, "distance")
  assert_integer_scalar(num_triangles, "num_triangles")
  assert_character_scalar(cap_style, "cap_style")
  assert_character_scalar(join_style, "join_style")
  assert_numeric(mitre_limit, "mitre_limit")

  ## Validate cap_style
  valid_cap_styles <- c("CAP_ROUND", "CAP_FLAT", "CAP_SQUARE")
  if (!toupper(cap_style) %in% valid_cap_styles) {
      cli::cli_abort("{.arg cap_style} must be one of: {.val {paste0(valid_cap_styles, collapse = ', ')}}.")
  }
  
  ## Validate join_style
  valid_join_styles <- c("JOIN_ROUND", "JOIN_MITRE", "JOIN_BEVEL")
  if (!toupper(join_style) %in% valid_join_styles) {
      cli::cli_abort("{.arg join_style} must be one of: {.val {paste0(valid_join_styles, collapse = ', ')}}.")
  }
  
  ## Check num_triangles is positive
  if (num_triangles < 1) cli::cli_abort("{.arg num_triangles} must be a positive integer")
  
  ## Check mitre_limit is striclty positive
  if (mitre_limit <= 0) cli::cli_abort("{.arg mitre_limit} must be a positive number")
  

  # 1. Build ST_Buffer parameters string
  buffer_args <- glue::glue("{distance}, {as.integer(num_triangles)}, '{toupper(cap_style)}', '{toupper(join_style)}', {mitre_limit}")
  

  # 2. Pass to template
  template_unary_ops(
    x = x,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_Buffer",
    other_args = buffer_args
  )

}





#' Calculates the centroid of geometries
#'
#' Returns the geometric center (centroid) of a geometry as a point, 
#' representing its average position.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## centroid
#' ddbs_centroid("argentina", conn)
#'
#' ## centroid without using a connection
#' ddbs_centroid(argentina_ddbs)
#' }
ddbs_centroid <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet     = FALSE) {
    
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_Centroid",
        other_args = NULL
    )

}





#' Make invalid geometries valid
#'
#' Attempts to correct invalid geometries so they conform to the rules of well-formed 
#' geometries (e.g., fixing self-intersections or improper rings) and returns the 
#' corrected geometries.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#'
#' ## make valid
#' ddbs_make_valid("countries", conn)
#'
#' ## make valid without using a connection
#' ddbs_make_valid(countries_ddbs)
#' }
ddbs_make_valid <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_MakeValid",
        other_args = NULL
    )
    
}






#' Simplify geometries
#'
#' Reduces the complexity of geometries by removing unnecessary vertices while preserving 
#' the overall shape.
#'
#' @template x
#' @param tolerance Tolerance distance for simplification. Larger values result in more 
#' simplified geometries.
#' @param preserve_topology If FALSE, uses the Douglas-Peucker algorithm, which reduces
#' the vertices by removing points that are within a given distance. If TRUE, uses a
#' topology-preserving variant of Douglas-Peucker that guarantees the output geometry
#' remains valid (slower).
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#'
#' ## simplify with tolerance of 0.01
#' ddbs_simplify("countries", tolerance = 0.01, conn = conn)
#'
#' ## simplify without using a connection
#' ddbs_simplify(countries_ddbs, tolerance = 0.01)
#' }
ddbs_simplify <- function(
    x,
    tolerance = 0,
    preserve_topology = FALSE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    

    # 0. Handle function-specific errors
    assert_positive_numeric(tolerance, "tolerance")
    assert_logic(preserve_topology, "preserve_topology")
  
    
    # 1. Build ST_Simplify parameters string
  
    ## 1.1. Choose the right function
    st_simplify_fun <- 
        if (isTRUE(preserve_topology)) "ST_SimplifyPreserveTopology" else "ST_Simplify"
  
    ## 1.2. Build tolerance parameter
    simplify_args <- tolerance
  
    # 2. Pass to template  
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = st_simplify_fun,
        other_args = simplify_args
    )

}






#' Extract the exterior ring of polygons
#'
#' Returns the outer boundary (exterior ring) of polygon geometries. For multi-polygons, 
#' returns the exterior ring of each individual polygon.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#'
#' ## extract exterior ring
#' ddbs_exterior_ring(conn = conn, "countries")
#'
#' ## extract exterior ring without using a connection
#' ddbs_exterior_ring(countries_ddbs)
#' }
ddbs_exterior_ring <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    # 0. Handle function-specific error
    assert_geom_type(x = x, conn = conn, geom = "POLYGON", multi = FALSE)
    
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_ExteriorRing",
        other_args = NULL
    )

}





#' Create a polygon from a single closed linestring
#'
#' Converts a single closed linestring geometry into a polygon. The linestring 
#' must be closed (first and last points identical). Does not work with 
#' MULTILINESTRING inputs - use [ddbs_polygonize()] or [ddbs_build_area()] instead.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @family polygon construction
#' @seealso [ddbs_polygonize()], [ddbs_build_area()]
#' @export
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## extract exterior ring as linestring, then convert back to polygon
#' ring_ddbs <- ddbs_exterior_ring(conn = conn, "argentina")
#' ddbs_make_polygon(conn = conn, ring_ddbs, name = "argentina_poly")
#'
#' ## create polygon without using a connection
#' ddbs_make_polygon(ring_ddbs)
#' }
ddbs_make_polygon <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    # 0. Handle function-specific error
    assert_geom_type(x = x, conn = conn, geom = "LINESTRING", multi = FALSE)
    
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_MakePolygon",
        other_args = NULL
    )
}





#' Compute the concave hull of geometries
#'
#' Returns the concave hull that tightly encloses the geometry, capturing its overall 
#' shape more closely than a convex hull.
#'
#' @template x
#' @param ratio Numeric. The ratio parameter dictates the level of concavity; `1`
#'        returns the convex hull, while `0` indicates to return the most concave
#'        hull possible. Defaults to `0.5`.
#' @param allow_holes Boolean. If `TRUE` (the default), it allows the output to
#'        contain holes.
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#' library(sf)
#'
#' # create points data
#' n <- 5
#' points_ddbs <- data.frame(
#'   id = 1,
#'   x = runif(n, min = -180, max = 180),
#'   y = runif(n, min = -90, max = 90)
#' ) |>
#'   ddbs_as_spatial(coords = c("x", "y")) |>
#'   ddbs_combine()
#'
#' # option 1: passing ddbs or sf objects
#' output1 <- duckspatial::ddbs_concave_hull(points_ddbs, mode = "sf")
#'
#' plot(output1)
#'
#'
#' # option 2: passing the name of a table in a duckdb db
#'
#' # creates a duckdb
#' conn <- duckspatial::ddbs_create_conn()
#'
#' # write sf to duckdb
#' ddbs_write_vector(conn, points_ddbs, "points_tbl")
#'
#' # spatial join
#' output2 <- duckspatial::ddbs_concave_hull(
#'  conn = conn,
#'  x = "points_tbl",
#'  mode = "sf"
#' )
#'
#' plot(output2)
#'
#' }
ddbs_concave_hull <- function(
    x,
    ratio = 0.5,
    allow_holes = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    # 0. Handle function-specific errors
    assert_numeric_interval(ratio, 0, 1, "ratio")
    assert_logic(allow_holes, "allow_holes")

    # 1. Build ST_ConcaveHull parameters string
    concavehull_args <- glue::glue("{ratio}, {toupper(allow_holes)}")    

    # 2. Pass to template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_ConcaveHull",
        other_args = concavehull_args
    )
}





#' Compute the convex hull of geometries
#'
#' Returns the convex hull that encloses the geometry, forming the smallest convex 
#' polygon that contains all points of the geometry.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' # read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' # option 1: passing sf objects
#' output1 <- duckspatial::ddbs_convex_hull(x = argentina_ddbs, mode = "sf")
#'
#' plot(output1["CNTR_NAME"])#' # store in duckdb
#'
#' # option 2: passing the name of a table in a duckdb db
#'
#' # creates a duckdb
#' conn <- duckspatial::ddbs_create_conn()
#'
#' # write sf to duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina_tbl")
#'
#' # spatial join
#' output2 <- duckspatial::ddbs_convex_hull(
#'  conn = conn,
#'  x = "argentina_tbl",
#'  mode = "sf"
#' )
#'
#' plot(output2["CNTR_NAME"])
#' }
ddbs_convex_hull <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_ConvexHull",
        other_args = NULL
    )

}





#' Transform the coordinate reference system of geometries
#'
#' Converts geometries to a different coordinate reference system (CRS), updating 
#' their coordinates accordingly.
#'
#' @template x
#' @param y Target CRS. Can be:
#'   \itemize{
#'     \item A character string with EPSG code (e.g., "EPSG:4326")
#'     \item An \code{sf} object (uses its CRS)
#'     \item Name of a DuckDB table (uses its CRS)
#'   }
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#' 
#' ## transform to different CRS using EPSG code
#' ddbs_transform("argentina", "EPSG:3857", conn)
#' 
#' ## transform to match CRS of another object
#' argentina_3857_ddbs <- ddbs_transform(argentina_ddbs, "EPSG:3857")
#' ddbs_write_vector(conn, argentina_3857_ddbs, "argentina_3857")
#' ddbs_transform("argentina", argentina_3857_ddbs, conn)
#' 
#' ## transform to match CRS of another DuckDB table
#' ddbs_transform("argentina", "argentina_3857", conn)
#' 
#' ## transform without using a connection
#' ddbs_transform(argentina_ddbs, "EPSG:3857")
#' }
ddbs_transform <- function(
    x,
    y,
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet     = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
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
    try(y <- normalize_spatial_input(y, conn_y), silent = TRUE)

    ## 1.4. Get mode - If it's NULL, it will use the duckspatial.mode option
    mode <- get_mode(mode, name)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y, conn, conn_x, conn_y, quiet = quiet)
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

    ## if CRS wasn't guessed earlier
    if (is.null(crs_x)) crs_x <- ddbs_crs(x_list$query_name, target_conn)
    if (is.null(crs_y)) {
        ## try to get from `y`. if it fails, it's not sf, nor duckspatial_df
        ## therefore, it might be a CRS or character string with CRS
        try(crs_y <- ddbs_crs(y_list$query_name, target_conn), silent = TRUE)

        if (is.null(crs_y)) crs_y <- sf::st_crs(y)
    }

    ## warn if the crs is the same
    if (crs_x$input == crs_y$input) cli::cli_warn("The CRS of `x` and `y` is the same.")
    
    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Build the base query
    ## always_xy assumes [northing, easting]
    st_function <- glue::glue("ST_Transform({x_geom}, '{crs_x$input}', '{crs_y$input}', always_xy := true)")
    base.query <- glue::glue("
        SELECT *
        REPLACE ({build_geom_query(st_function, name, crs_y, mode)} AS {x_geom})
        FROM 
            {x_list$query_name};
    ")

    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            {base.query};
        ")
        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Apply geospatial operation
    result <- ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = crs_y,
        x_geom     = x_geom
    )

    return(result)
}





#' Get the geometry type of features
#'
#' Returns the type of each geometry (e.g., POINT, LINESTRING, POLYGON) in the 
#' input features.
#'
#' @template x
#' @template by_feature
#' @template conn_null
#'
#' @returns A factor with geometry type(s)
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' # option 1: passing sf objects
#' # Get geometry type for each feature
#' ddbs_geometry_type(countries_ddbs)
#' 
#' # Get overall geometry type
#' ddbs_geometry_type(countries_ddbs, by_feature = FALSE)
#' }
ddbs_geometry_type <- function(
  x,
  by_feature = TRUE,
  conn = NULL) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_character(conn, x)
  assert_logic(by_feature, "by_feature")
  

  # 1. Prepare inputs
  
  ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names)
  x <- normalize_spatial_input(x, conn)

  ## 1.2. Pre-extract attributes
  crs_x    <- ddbs_crs(x, conn)
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


  ## 2.3. Create the query
  if (isTRUE(by_feature)) {
    tmp.query <- glue::glue("
          SELECT ST_GeometryType({x_geom}) as geometry
          FROM {x_list$query_name};
      ")
  } else {
    tmp.query <- glue::glue("
          SELECT DISTINCT ST_GeometryType({x_geom}) as geometry
          FROM {x_list$query_name};
      ")
  }
  
  ## 2.4 Return a factor vector
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)
  return(data_tbl$geometry)
    
}






#' Assemble polygons from multiple linestrings
#'
#' Takes a collection of linestrings or polygons and assembles them into polygons by 
#' finding all closed rings formed by the network. Returns a GEOMETRYCOLLECTION containing 
#' the resulting polygons.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @family polygon construction
#' @seealso [ddbs_make_polygon()], [ddbs_build_area()]
#' @export
ddbs_polygonize <- function(
    x,
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
  
    ## 2.2.  Build the base query (depends on the output type - sf, duckspatial_df, table)
    st_function <- glue::glue("ST_Polygonize(LIST({x_geom}))")
    base.query <- glue::glue("
      SELECT 
        {build_geom_query(st_function, name, crs_x, mode)} as {x_geom}
      FROM 
        {x_list$query_name};
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
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = crs_x,
        x_geom     = x_geom
        )
    }
    
}





#' Build polygon areas from multiple linestrings
#'
#' Constructs polygon or multipolygon geometries from a collection of linestrings, 
#' handling intersections and creating unified areas. Returns POLYGON or MULTIPOLYGON 
#' (not wrapped in a geometry collection). Requires MULTILINESTRING input - for 
#' single linestrings, use [ddbs_make_polygon()].
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @family polygon construction
#' @seealso [ddbs_make_polygon()], [ddbs_polygonize()]
#' @export
ddbs_build_area <- function(
  x,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  
  template_unary_ops(
    x = x,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet,
    fun = "ST_BuildArea",
    other_args = NULL
  )

}





#' Computes a Voronoi diagram from point geometries
#'
#' Returns a Voronoi diagram (Thiessen polygons) from a collection of points.
#' Each polygon represents the region closer to one point than to any other
#' point in the set. This function only works with MULTIPOINT geometries.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## create some points, and combine them to MULTIPOINT
#' set.seed(42)
#' n <- 1000
#' points_ddbs <- data.frame(
#'     id = 1:n,
#'     x = runif(n, min = -20, max = 20),
#'     y = runif(n, min = -20, max = 02)
#' ) |>
#'   ddbs_as_spatial(coords = c("x", "y"), crs = 4326) |> 
#'   ddbs_combine()
#' 
#' ## create voronoi diagrama
#' ddbs_voronoi(points_ddbs)
#' }
ddbs_voronoi <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {


    # 0. Handle function-specific errors
    assert_geom_type(x, conn, geom = "MULTIPOINT")

    # 1. Run the template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_VoronoiDiagram",
        other_args = NULL
    )

}




#' Extract the start or end point of a linestring geometry
#'
#' Returns the first or last point of a LINESTRING geometry. These functions only work
#' with LINESTRING geometries (not MULTILINESTRING or other geometry types).
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#'
#' @details
#' These functions wrap DuckDB Spatial's \code{ST_StartPoint} and \code{ST_EndPoint}.
#' Input geometries must be of type LINESTRING (MULTILINESTRING is not supported).
#' For each input feature, the first or last coordinate of the LINESTRING is returned
#' as a POINT geometry.
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, rivers_ddbs, "rivers")
#'
#' ## extract start points
#' ddbs_startpoint(conn = conn, "rivers")
#'
#' ## extract end points
#' ddbs_endpoint(conn = conn, "rivers")
#'
#' ## without using a connection
#' ddbs_startpoint(rivers_ddbs)
#' ddbs_endpoint(rivers_ddbs)
#' }
#' @name ddbs_endpoint_startpoint
#' @rdname ddbs_endpoint_startpoint
NULL



#' @rdname ddbs_endpoint_startpoint
#' @export
ddbs_startpoint <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Handle function-specific error
    assert_geom_type(x = x, conn = conn, geom = "LINESTRING", multi = FALSE)

    # 1. Run the template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_StartPoint",
        other_args = NULL
    )

}

#' @rdname ddbs_endpoint_startpoint
#' @export
ddbs_endpoint <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Handle function-specific error
    assert_geom_type(x = x, conn = conn, geom = "LINESTRING", multi = FALSE)

    # 1. Run the template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_EndPoint",
        other_args = NULL
    )

}





#' Flips the X and Y coordinates of geometries
#'
#' Returns a geometry with the X and Y coordinates swapped. This is useful
#' for correcting geometries where longitude and latitude are in the wrong
#' order, or for converting between coordinate systems with different axis
#' orders.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## flip coordinates
#' ddbs_flip_coordinates("argentina", conn)
#'
#' ## flip coordinates without using a connection
#' ddbs_flip_coordinates(argentina_ddbs)
#' }
ddbs_flip_coordinates <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_FlipCoordinates",
        other_args = NULL
    )

}





#' Drop geometry column from a duckspatial_df object
#'
#' Removes the geometry column from a \code{duckspatial_df} object, returning a 
#' lazy tibble without spatial information.
#'
#' @template x
#'
#' @return A lazy tibble backed by dbplyr
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## drop geometry column
#' countries_tbl <- ddbs_drop_geometry(countries_ddbs)
#' }
ddbs_drop_geometry <- function(x) {

  ## Get geometry column name
  geometry_col <- attr(x, "sf_column")

  ## Drop duckspatial_df class
  class(x) <- setdiff(class(x), c("duckspatial_df"))

  ## Unselect geometry column
  dplyr::select(x, -dplyr::all_of(geometry_col))
}





#' Convert geometries to multi-type
#'
#' Converts single geometries to their multi-type equivalent (e.g.,
#' \code{POLYGON} to \code{MULTIPOLYGON}). Geometries that are already multi-type are
#' returned unchanged.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## convert to multi-type
#' ddbs_multi(countries_ddbs)
#' }
ddbs_multi <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_Multi",
        other_args = NULL
    )

}

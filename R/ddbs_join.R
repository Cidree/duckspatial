#' Perform a spatial join of two geometries
#'
#' Combines two sets of geometries based on spatial relationships, such as 
#' intersection or containment, attaching attributes from one set to the other.
#'
#' @template x
#' @template y
#' @param join A geometry predicate function. Defaults to `"intersects"`. See
#' the details for other options.
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @param distance a numeric value specifying the distance for ST_DWithin. The units
#' should be specified in meters
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#'
#' @template spatial_join_predicates
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # RECOMMENDED: Efficient lazy workflow using ddbs_open_dataset
#' library(duckspatial)
#'
#' # Load data directly as lazy spatial data frames (CRS auto-detected)
#' countries <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", package = "duckspatial")
#' )
#'
#' # Create random points
#' n <- 100
#' points <- data.frame(
#'     id = 1:n,
#'     x = runif(n, min = -180, max = 180),
#'     y = runif(n, min = -90, max = 90)
#' ) |> 
#'   sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
#'   as_duckspatial_df()
#'
#' # Lazy join - computation stays in DuckDB
#' result <- ddbs_join(points, countries, join = "within")
#'
#' # Collect to sf when needed
#' result_sf <- dplyr::collect(result) |> sf::st_as_sf()
#' plot(result_sf["CNTR_NAME"])
#'
#'
#' # Alternative: using sf objects directly (legacy compatibility)
#' library(sf)
#'
#' countries_sf <- sf::st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#'
#' output <- duckspatial::ddbs_join(
#'     x = points,
#'     y = countries_sf,
#'     join = "within"
#' )
#'
#'
#' # Alternative: using table names in a duckdb connection
#' conn <- duckspatial::ddbs_create_conn()
#'
#' ddbs_write_table(conn, points, "points", overwrite = TRUE)
#' ddbs_write_table(conn, countries_sf, "countries", overwrite = TRUE)
#'
#' output2 <- ddbs_join(
#'     conn = conn,
#'     x = "points",
#'     y = "countries",
#'     join = "within"
#' )
#'
#' }
ddbs_join <- function(
    x,
    y,
    join = "intersects",
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    name = NULL,
    distance = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    # 0. Validate inputs
    assert_xy(x, "x")
    assert_xy(y, "y")
    assert_name(name)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    
    # Validate predicate early (it aborts on invalid)
    sel_pred <- get_st_predicate(join)
    
    # 1. Prepare inputs

    ## 1.1. Resolve conn_x/conn_y defaults from 'conn' for character inputs
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

    ## 1.2. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names)
    x <- normalize_spatial_input(x, conn_x)
    y <- normalize_spatial_input(y, conn_y)

    ## 1.3. Pre-extract attributes
    crs_x    <- ddbs_crs(x, conn_x)
    crs_y    <- ddbs_crs(y, conn_y)
    sf_col_x <- attr(x, "sf_column")
    sf_col_y <- attr(y, "sf_column")
    mode     <- get_mode(mode, name)

    ## 1.3. Resolve spatial connections and handle imports
    resolve_res <- resolve_spatial_connections(x, y, conn, conn_x, conn_y, quiet = quiet)
    # NOTE: Inline connection resolution logic was replaced by resolve_spatial_connections()
    # helper (defined in db_utils_not_exported.R) to maintain consistency with ddbs_join
    # and other two-input spatial functions. See tests/testthat/test-resolve_connections.R
    # for regression tests covering cross-connection scenarios.
    target_conn <- resolve_res$conn
    x           <- resolve_res$x
    y           <- resolve_res$y
    
    ## 1.4. register cleanup of the connection
    if (any(is.null(conn_x), is.null(conn_y))) {
        on.exit(resolve_res$cleanup(), add = TRUE)   
    }
    
    ## 1.5. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)
    y_list <- get_query_list(y, target_conn)
    on.exit(y_list$cleanup(), add = TRUE)
    
    ## 1.6. Validate the CRS of x and y
    if (!is.null(crs_x) && !is.null(crs_y)) {
       if (!crs_equal(crs_x, crs_y)) {
         cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
       }
    } else {
       assert_crs(target_conn, x_list$query_name, y_list$query_name)
    }


    # 2. Prepare the query

    ## 2.1. Get names of geometry columns (use saved sf_col_x/y from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)

    ## 2.2. Build predicate clause
    st_function <- glue::glue("v1.{x_geom}")
    
    st_predicate <- generate_predicate_clause(
        predicate = sel_pred,
        conn      = target_conn,
        x_list    = x_list,
        y_list    = y_list,
        x_geom    = x_geom,
        y_geom    = y_geom,
        distance  = distance,
        crs_x     = crs_x
    )

    ## 2.3 Build the base query
    base.query <- glue::glue("
        SELECT 
            v1.* REPLACE ({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom}),
            v2.* EXCLUDE ({y_geom})
        FROM 
            {x_list$query_name} v1
        JOIN 
            {y_list$query_name} v2
        ON 
            {st_predicate};
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



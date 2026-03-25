



#' Perform a spatial filter
#'
#' Filters geometries based on a spatial relationship with another geometry, 
#' such as intersection, containment, or proximity.
#'
#' @template x
#' @template y
#' @template predicate
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
#' argentina <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", package = "duckspatial")
#' )
#'
#' # Lazy filter - computation stays in DuckDB
#' neighbors <- ddbs_filter(countries, argentina, predicate = "touches")
#'
#' # Collect to sf when needed
#' neighbors_sf <- dplyr::collect(neighbors) |> sf::st_as_sf()
#'
#'
#' # Alternative: using sf objects directly (legacy compatibility)
#' library(sf)
#'
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' result <- ddbs_filter(countries_sf, argentina_sf, predicate = "touches")
#'
#'
#' # Alternative: using table names in a duckdb connection
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ddbs_write_table(conn, countries_sf, "countries")
#' ddbs_write_table(conn, argentina_sf, "argentina")
#'
#' ddbs_filter(conn = conn, "countries", "argentina", predicate = "touches")
#' }
ddbs_filter <- function(
    x,
    y,
    predicate = "intersects",
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
    sel_pred <- get_st_predicate(predicate)

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

    ## 2.3. Build the base query (SELECT DISTINCT to avoid duplicates from 
    ## one-to-many relationships)
    base.query <- glue::glue("
        SELECT DISTINCT 
            v1.* REPLACE({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom})
        FROM 
            {x_list$query_name} v1, 
            {y_list$query_name} v2
        WHERE 
            {st_predicate}
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

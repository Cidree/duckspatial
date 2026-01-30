



#' Performs spatial filter of two geometries
#'
#' Filters data spatially based on a spatial predicate
#'
#' @template x
#' @param y Y table with geometry column within the DuckDB database
#' @template predicate
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @template crs
#' @param distance a numeric value specifying the distance for ST_DWithin. Units correspond to
#' the coordinate system of the geometry (e.g. degrees or meters)
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @template returns_output
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
#' ddbs_write_vector(conn, countries_sf, "countries")
#' ddbs_write_vector(conn, argentina_sf, "argentina")
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
    crs = NULL,
    crs_column = "crs_duckspatial",
    distance = NULL,
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    # 1. Validate inputs
    assert_xy(x, "x")
    assert_xy(y, "y")
    assert_name(name)
    assert_name(output, "output")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    
    # Validate predicate early (it aborts on invalid)
    sel_pred <- get_st_predicate(predicate)

    # 2. Normalize inputs (coerce tbl_duckdb_connection, validate character)
    
    # Pre-extract CRS and sf_column (before normalize_spatial_input converts types)
    crs_x <- detect_crs(x)
    crs_y <- detect_crs(y)
    sf_col_x <- attr(x, "sf_column")
    sf_col_y <- attr(y, "sf_column")

    # Resolve conn_x/conn_y defaults from 'conn' for character inputs
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

    # Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, validate character table names
    x <- normalize_spatial_input(x, conn_x)
    y <- normalize_spatial_input(y, conn_y)
    
    # 3. Manage connection to DB
    ## 3.1. Resolve connections and handle imports
    resolve_res <- resolve_spatial_connections(x, y, conn, conn_x, conn_y)
    
    # NOTE: Inline connection resolution logic was replaced by resolve_spatial_connections()
    # helper (defined in db_utils_not_exported.R) to maintain consistency with ddbs_join
    # and other two-input spatial functions. See tests/testthat/test-resolve_connections.R
    # for regression tests covering cross-connection scenarios.
    
    target_conn <- resolve_res$conn
    x <- resolve_res$x
    y <- resolve_res$y
    
    # Register cleanup
    on.exit(resolve_res$cleanup(), add = TRUE)
    
    ## 3.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)
    y_list <- get_query_list(y, target_conn)
    on.exit(y_list$cleanup(), add = TRUE)
    
    # CRS already extracted at start of function
    
    if (!is.null(crs_x) && !is.null(crs_y)) {
       if (!crs_equal(crs_x, crs_y)) {
         cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
       }
    } else {
       assert_crs(target_conn, x_list$query_name, y_list$query_name)
    }

    # 4. Prepare parameters for query
    ## 4.1. predicate already validated early (sel_pred above)
    ## 4.2. get names of geometry columns (use saved sf_col_x/y from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
    
    ## 4.3. get columns to return from x
    x_rest_cols <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = FALSE)
    
    ## error if crs_column not found (conditional on saved crs_x)
    if (is.null(crs_x)) {
        assert_crs_column(crs_column, x_rest_cols)
    }

    ## 4.4. Format column lists for SQL
    x_rest <- if (length(x_rest_cols) > 0) paste0('v1."', x_rest_cols, '", ', collapse = '') else ""

    ## 5. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## if distance is not specified, it will use ST_Within
        if (sel_pred == "ST_DWithin") {

            if (is.null(distance)) {
                cli::cli_warn("{.val distance} wasn't specified. Using ST_Within.")
                distance <- 0
            }

            tmp.query <- glue::glue("
                CREATE TABLE {name_list$query_name} AS
                SELECT DISTINCT 
                    {x_rest} 
                    v1.{x_geom} AS {x_geom}
                FROM 
                    {x_list$query_name} v1, 
                    {y_list$query_name} v2
                WHERE 
                    {sel_pred}(v1.{x_geom}, v2.{y_geom}, {distance})
            ")

        } else {
            tmp.query <- glue::glue("
                CREATE TABLE {name_list$query_name} AS
                SELECT DISTINCT 
                    {x_rest} 
                    v1.{x_geom} AS {x_geom}
                FROM 
                    {x_list$query_name} v1, 
                    {y_list$query_name} v2
                WHERE 
                    {sel_pred}(v1.{x_geom}, v2.{y_geom})
            ")
        }

        ## execute filter query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    ## 6. Get data frame
    if (sel_pred == "ST_DWithin") {

        ## if distance is not specified, it will use ST_Within
        if (is.null(distance)) {
            cli::cli_warn("{.val distance} wasn't specified. Using ST_Within.")
            distance <- 0
        }

        data_tbl <- DBI::dbGetQuery(
            target_conn, glue::glue("
                SELECT DISTINCT 
                    {x_rest} 
                    ST_AsWKB(v1.{x_geom}) AS {x_geom}
                FROM 
                    {x_list$query_name} v1, 
                    {y_list$query_name} v2
                WHERE 
                    {sel_pred}(v1.{x_geom}, v2.{y_geom}, {distance})
            ")
        )

    } else {
        data_tbl <- DBI::dbGetQuery(
            target_conn, glue::glue("
                SELECT DISTINCT 
                    {x_rest} 
                    ST_AsWKB(v1.{x_geom}) AS {x_geom}
                FROM 
                    {x_list$query_name} v1, 
                    {y_list$query_name} v2
                WHERE 
                    {sel_pred}(v1.{x_geom}, v2.{y_geom})
            ")
        )
    }

    ## 7. Handle output based on output parameter
    result <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(result)

}

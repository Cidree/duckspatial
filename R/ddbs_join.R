#' Performs spatial joins of two geometries
#'
#' Performs spatial joins of two geometries, and returns a \code{sf} object
#' or creates a new table in a DuckDB database.
#'
#' @template x
#' @param y An `sf` spatial object. Alternatively, it can be a string with the
#'        name of a table with geometry column within the DuckDB database `conn`.
#' @param join A geometry predicate function. Defaults to `"intersects"`. See
#'        the details for other options.
#' @template conn_null
#' @template conn_x_conn_y
#'
#' @param name A character string of length one specifying the name of the table,
#'        or a character string of length two specifying the schema and table
#'        names. If it's `NULL` (the default), it will return the result as an
#'        \code{sf} object.
#' @template crs
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
#' ddbs_write_vector(conn, points, "points", overwrite = TRUE)
#' ddbs_write_vector(conn, countries_sf, "countries", overwrite = TRUE)
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
    crs = NULL,
    crs_column = "crs_duckspatial",
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
    
    # Validate join predicate early (reuses get_st_predicate which aborts on invalid)
    sel_pred <- get_st_predicate(join)
    
    # 2. Normalize inputs (coerce tbl_duckdb_connection, validate character)
    
    # Pre-extract CRS and sf_column (before normalize_spatial_input converts types)
    crs_x <- detect_crs(x)
    crs_y <- detect_crs(y)
    sf_col_x <- attr(x, "sf_column")
    sf_col_y <- attr(y, "sf_column")

    # Resolve conn_x/conn_y defaults from 'conn' for character inputs
    # (Only if conn was provided and we need a connection for a character input)
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

    # Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, validate character table names
    x <- normalize_spatial_input(x, conn_x)
    y <- normalize_spatial_input(y, conn_y)
    
     # 3. Manage connection to DB
    ## 3.1. Resolve connections and handle imports
    resolve_res <- resolve_spatial_connections(x, y, conn, conn_x, conn_y)
    
    target_conn <- resolve_res$conn
    x <- resolve_res$x
    y <- resolve_res$y
    
    # Register cleanup
    on.exit(resolve_res$cleanup(), add = TRUE)


    
    ## 3.3. Get query list of table names
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
    ## 4.2. get name of geometry column (use saved sf_col_x/y from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
    
    ## 4.3. Get non-geometry columns for x and y (both use same pattern)
    x_rest_cols <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = FALSE)
    y_rest_cols <- get_geom_name(target_conn, y_list$query_name, rest = TRUE, collapse = FALSE)
    
    ## error if crs_column not found (conditional on saved crs_x)
    if (is.null(crs_x)) {
       assert_crs_column(crs_column, x_rest_cols)
    }
    
    ## remove CRS column from y_rest_cols
    crs_idx <- grep(crs_column, y_rest_cols, fixed = TRUE)
    if (length(crs_idx) > 0) y_rest_cols <- y_rest_cols[-crs_idx]
    
    ## 4.4. Format column lists for SQL (symmetric handling)
    x_rest <- if (length(x_rest_cols) > 0) paste0('tbl_x."', x_rest_cols, '", ', collapse = '') else ""
    y_rest <- if (length(y_rest_cols) > 0) paste0('tbl_y."', y_rest_cols, '", ', collapse = '') else ""

    ## 5. if name is not NULL (i.e. no SF returned)
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
                {y_rest}
                tbl_x.{x_geom} AS {x_geom}
            FROM 
                {x_list$query_name} tbl_x
            JOIN 
                {y_list$query_name} tbl_y
            ON 
                {sel_pred}(tbl_x.{x_geom}, tbl_y.{y_geom})

        ")

        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    ## 6. create the base query
    tmp.query <- glue::glue("
        SELECT 
            {x_rest}
            {y_rest}
            ST_AsWKB(tbl_x.{x_geom}) AS {x_geom}
        FROM 
            {x_list$query_name} tbl_x
        JOIN 
            {y_list$query_name} tbl_y
        ON 
            {sel_pred}(tbl_x.{x_geom}, tbl_y.{y_geom})
    ")

    ## send the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

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



# has_rtree_index <- function(conn,  tbl_name){
#
#     temp_df <- DBI::dbGetQuery(
#         conn,
#         glue::glue("
#             SELECT *
#             FROM duckdb_indexes()
#             WHERE table_name = '{tbl_name}';
#           ")
#     )
#
#     check <- grepl(" RTREE ", temp_df$sql)
#     check <- ifelse(isTRUE(check), TRUE, FALSE)
#     return(check)
# }

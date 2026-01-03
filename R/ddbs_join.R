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
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    deprecate_crs(crs_column, crs)
    
    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x, y)
    
    # 0.5. Normalize inputs (coerce tbl_duckdb_connection, validate character)
    x <- prepare_spatial_input(x, conn)
    y <- prepare_spatial_input(y, conn)
    
    # Pre-extract CRS (before y might be converted to character)
    crs_x <- attr(x, "crs")
    crs_y <- attr(y, "crs")
    
    # Try auto-detection for tbl_duckdb_connection if CRS is NULL
    if (is.null(crs_x) && inherits(x, "tbl_duckdb_connection")) {
       crs_x <- suppressWarnings(ddbs_crs(x))
       if (is.na(crs_x)) crs_x <- NULL
    }
    if (is.null(crs_y) && inherits(y, "tbl_duckdb_connection")) {
       crs_y <- suppressWarnings(ddbs_crs(y))
       if (is.na(crs_y)) crs_y <- NULL
    }

     # 1. Manage connection to DB
    ## 1.1. Extract connections from inputs
    conn_x <- get_conn_from_input(x)
    conn_y <- get_conn_from_input(y)
    
    ## 1.2. Resolve which connection to use
    if (!is.null(conn)) {
      # User explicitly provided connection - use it
      target_conn <- conn
    } else if (!is.null(conn_x) && !is.null(conn_y)) {
      # Both inputs have connections  
      if (identical(conn_x, conn_y)) {
        # Same connection - great!
        target_conn <- conn_x
      } else {
        # Different connections - import y into x's connection
        cli::cli_warn(c(
          "{.arg x} and {.arg y} come from different DuckDB connections.",
          "i" = "Using connection from {.arg x}. Importing {.arg y} to this connection.",
          "i" = "This may require materializing data."
        ))
        target_conn <- conn_x
        
        # Import y from conn_y to conn_x
        import_result <- import_view_to_connection(
          target_conn = conn_x,
          source_conn = conn_y,
          source_object = y
        )
        
        # Replace y with imported view name as character (for get_query_list)
        y <- import_result$name
        
        # Register cleanup to drop imported view on exit
        on.exit(
          tryCatch({
            DBI::dbExecute(target_conn, glue::glue("DROP VIEW IF EXISTS {import_result$name}"))
          }, error = function(e) NULL),
          add = TRUE
        )
      }
    } else if (!is.null(conn_x)) {
      target_conn <- conn_x
    } else if (!is.null(conn_y)) {
      target_conn <- conn_y
    } else {
      target_conn <- ddbs_default_conn()
    }
    
    ## 1.3. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    y_list <- get_query_list(y, target_conn)
    
    # CRS already extracted at start of function

    
    if (!is.null(crs_x) && !is.null(crs_y)) {
       if (!crs_equal(crs_x, crs_y)) {
         cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
       }
    } else {
       assert_crs(target_conn, x_list$query_name, y_list$query_name)
    }

    # 2. Prepare params for query
    ## 2.1. select predicate
    sel_pred <- get_st_predicate(join)
    ## 2.2. get name of geometry column
    x_geom <- attr(x, "sf_column") %||% get_geom_name(target_conn, x_list$query_name)
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE, table_id = "tbl_x")
    y_geom <- attr(y, "sf_column") %||% get_geom_name(target_conn, y_list$query_name)
    y_rest <- get_geom_name(target_conn, y_list$query_name, rest = TRUE, collapse = FALSE)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
    ## error if crs_column not found
    ## error if crs_column not found (conditional on attribute)
    if (is.null(attr(x, "crs"))) {
       assert_crs_column(crs_column, x_rest)
    }
    ## remove CRS column from y_rest
    y_rest <- y_rest[-grep(crs_column, y_rest)]
    y_rest <- if (length(y_rest) > 0) paste0('tbl_y."', y_rest, '",', collapse = ' ') else ""

    ## 3. if name is not NULL (i.e. no SF returned)
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

    ## 4. create the base query
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

    ## 5. Handle output based on output parameter
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

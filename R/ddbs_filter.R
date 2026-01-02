



#' Performs spatial filter of two geometries
#'
#' Filters data spatially based on a spatial predicate
#'
#' @template x
#' @param y Y table with geometry column within the DuckDB database
#' @template predicate
#' @template conn_null
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
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    distance = NULL,
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

    #1. Manage connection to DB
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
    sel_pred <- get_st_predicate(predicate)
    ## 2.3. get geometry columns
    x_geom <- attr(x, "sf_column") %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- attr(y, "sf_column") %||% get_geom_name(target_conn, y_list$query_name)
    
    ## 2.4. get columns to return
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE, table_id = "v1")
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
    ## error if crs_column not found (only if we don't have it in attributes)
    if (is.null(attr(x, "crs"))) {
        assert_crs_column(crs_column, x_rest)
    }

    ## 3. if name is not NULL (i.e. no SF returned)
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
                    {sel_pred}(v2.{y_geom}, v1.{x_geom}, {distance})
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
                    {sel_pred}(v2.{y_geom}, v1.{x_geom})
            ")
        }

        ## execute filter query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    ## 4. Get data frame
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
                    {sel_pred}(v2.{y_geom}, v1.{x_geom}, {distance})
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
                    {sel_pred}(v2.{y_geom}, v1.{x_geom})
            ")
        )
    }

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


#' Calculates the intersection of two geometries
#'
#' Calculates the intersection of two geometries, and return a \code{sf} object
#' or creates a new table
#'
#' @template x
#' @param y A table with geometry column within the DuckDB database
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @template crs
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @template returns_output
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
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
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## intersection inside the connection
#' ddbs_intersection("countries", "argentina", conn)
#'
#' ## intersection without using a connection
#' ddbs_intersection(countries_ddbs, argentina_ddbs)
#' }
ddbs_intersection <- function(
    x,
    y,
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

    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x <- detect_crs(x)
    crs_y <- detect_crs(y)
    sf_col_x <- attr(x, "sf_column")
    sf_col_y <- attr(y, "sf_column")

    ## 1.2. Resolve conn_x/conn_y defaults from 'conn' for character inputs
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

    ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn_x)
    y <- normalize_spatial_input(y, conn_y)


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

    ## CRS already extracted at start of function
    if (!is.null(crs_x) && !is.null(crs_y)) {
        if (!crs_equal(crs_x, crs_y)) {
            cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
        }
    } else {
        assert_crs(target_conn, x_list$query_name, y_list$query_name)
    }


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE, table_id = "v1")


    # 4. if name is not NULL (i.e. no SF returned)
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
                ST_Intersection(v1.{x_geom}, 
                v2.{y_geom}) AS {x_geom}
            FROM 
                {x_list$query_name} v1,
                {y_list$query_name} v2
            WHERE 
                ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. Get data fram

    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT 
            {x_rest}
            ST_AsWKB(ST_Intersection(v1.{x_geom}, v2.{y_geom})) AS {x_geom}
        FROM 
            {x_list$query_name} v1, 
            {y_list$query_name} v2
        WHERE 
            ST_Intersects(v2.{y_geom}, v1.{x_geom})
    ")

    ## 5.2. retrieve results of the query
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






#' Calculates the difference of two geometries
#'
#' Calculates the geometric difference of two geometries, and returns a \code{sf}
#' object or creates a new table
#'
#' @template x
#' @param y A table with geometry column within the DuckDB database
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @template crs
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @template returns_output
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
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
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## difference with a connection
#' ddbs_difference("countries", "argentina", conn)
#'
#' ## difference without a connection
#' ddbs_difference(countries_ddbs, argentina_ddbs)
#' }
ddbs_difference <- function(
    x,
    y,
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

    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x <- detect_crs(x)
    crs_y <- detect_crs(y)
    sf_col_x <- attr(x, "sf_column")
    sf_col_y <- attr(y, "sf_column")

    ## 1.2. Resolve conn_x/conn_y defaults from 'conn' for character inputs
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

    ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn_x)
    y <- normalize_spatial_input(y, conn_y)


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

    ## CRS already extracted at start of function
    if (!is.null(crs_x) && !is.null(crs_y)) {
        if (!crs_equal(crs_x, crs_y)) {
            cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
        }
    } else {
        assert_crs(target_conn, x_list$query_name, y_list$query_name)
    }


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE, table_id = "v1")


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            WITH diff_geom AS (
                SELECT 
                    {x_rest}
                    ST_Difference(
                        ST_MakeValid(v1.{x_geom}),
                        ST_MakeValid(v2.{y_geom})
                    ) AS {x_geom}
                FROM 
                    {x_list$query_name} v1, 
                    {y_list$query_name} v2
            )
            SELECT * FROM diff_geom
            WHERE NOT ST_IsEmpty({x_geom});
        ")

        ## execute query
        DBI::dbExecute(target_conn, tmp.query)

        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. Get data frame

    ## 5.1. create query
    tmp.query <- glue::glue("
        WITH diff_geom AS (
            SELECT 
                {x_rest}
                ST_Difference(
                    ST_MakeValid(v1.{x_geom}),
                    ST_MakeValid(v2.{y_geom})
                ) AS {x_geom}
            FROM 
                {x_list$query_name} v1, 
                {y_list$query_name} v2
            WHERE NOT ST_IsEmpty(
                ST_Difference(
                    ST_MakeValid(v1.{x_geom}),
                    ST_MakeValid(v2.{y_geom})
                )
            )
        )
        SELECT 
            {x_rest}
            ST_AsWKB({x_geom}) AS {x_geom}
        FROM diff_geom v1;
    ")

    ## 5.2. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)


    # 6. convert to SF
    data_sf <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    ## return result
    feedback_query(quiet)
    return(data_sf)

}

#' Geometry binary operations
#'
#' Perform geometric set operations between two sets of geometries.
#'
#' @details
#' These functions perform different geometric set operations:
#' \describe{
#'   \item{`ddbs_intersection`}{Returns the geometric intersection of two sets 
#'   of geometries, producing the area, line, or point shared by both.}
#'   \item{`ddbs_difference`}{Returns the portion of the first geometry that 
#'   does not overlap with the second geometry.}
#'   \item{`ddbs_sym_difference`}{Returns the portions of both geometries 
#'   that do not overlap with each other. Equivalent to 
#'   `(A - B) UNION (B - A)`.}
#' }
#'
#' @template x
#' @template y
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#' library(sf)
#'
#' # Create two overlapping polygons for testing
#' poly1 <- st_polygon(list(matrix(c(
#'   0, 0,
#'   4, 0,
#'   4, 4,
#'   0, 4,
#'   0, 0
#' ), ncol = 2, byrow = TRUE)))
#'
#' poly2 <- st_polygon(list(matrix(c(
#'   2, 2,
#'   6, 2,
#'   6, 6,
#'   2, 6,
#'   2, 2
#' ), ncol = 2, byrow = TRUE)))
#'
#' x <- st_sf(id = 1, geometry = st_sfc(poly1), crs = 4326)
#' y <- st_sf(id = 2, geometry = st_sfc(poly2), crs = 4326)
#'
#' # Visualize the input polygons
#' plot(st_geometry(x), col = "lightblue", main = "Input Polygons")
#' plot(st_geometry(y), col = "lightcoral", add = TRUE, alpha = 0.5)
#'
#' # Intersection: only the overlapping area (2,2 to 4,4)
#' result_intersect <- ddbs_intersection(x, y)
#' plot(st_geometry(result_intersect), col = "purple", 
#'      main = "Intersection")
#'
#' # Difference: part of x not in y (L-shaped area)
#' result_diff <- ddbs_difference(x, y)
#' plot(st_geometry(result_diff), col = "lightblue", 
#'      main = "Difference (x - y)")
#'
#' # Symmetric Difference: parts of both that don't overlap
#' result_symdiff <- ddbs_sym_difference(x, y)
#' plot(st_geometry(result_symdiff), col = "orange", 
#'      main = "Symmetric Difference")
#'
#' # Using with database connection
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ddbs_write_vector(conn, x, "poly_x")
#' ddbs_write_vector(conn, y, "poly_y")
#'
#' # Perform operations with connection
#' ddbs_intersection("poly_x", "poly_y", conn)
#' ddbs_difference("poly_x", "poly_y", conn)
#' ddbs_sym_difference("poly_x", "poly_y", conn)
#'
#' # Save results to database table
#' ddbs_difference("poly_x", "poly_y", conn, name = "diff_result")
#' }
#'
#' @name ddbs_binary_funs
#' @rdname ddbs_binary_funs
NULL





#' @rdname ddbs_binary_funs
#' @export
ddbs_intersection <- function(
    x,
    y,
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    

    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
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
    if (any(is.null(conn_x), is.null(conn_y))) {
        on.exit(resolve_conn$cleanup(), add = TRUE)   
    }

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

    ## 3.2. Build the base query
    st_function <- glue::glue("ST_Intersection(v1.{x_geom}, v2.{y_geom})")
    base.query <- glue::glue("
        SELECT 
            v1.* REPLACE({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom})
        FROM 
            {x_list$query_name} v1,
            {y_list$query_name} v2
        WHERE 
            ST_Intersects(v2.{y_geom}, v1.{x_geom});
    ")


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS 
            {base.query}
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
        crs        = crs_x,
        x_geom     = x_geom
    )
    
    return(result)
}






#' @rdname ddbs_binary_funs
#' @export
ddbs_difference <- function(
    x,
    y,
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    

    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
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
    if (any(is.null(conn_x), is.null(conn_y))) {
        on.exit(resolve_conn$cleanup(), add = TRUE)   
    }

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

    ## 3.2. Build base query
    st_function <- glue::glue("{x_geom}")
    base.query <- glue::glue("
        WITH diff_geom AS (
            SELECT 
                v1.* REPLACE (
                    ST_Difference(
                        ST_MakeValid(v1.{x_geom}),
                        ST_MakeValid(v2.{y_geom})
                    ) AS {x_geom}
                )
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
            * REPLACE ({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom})
        FROM diff_geom;
    ")


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            {base.query}
        ")

        ## execute query
        DBI::dbExecute(target_conn, tmp.query)

        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. Apply geospatial operation
    result <- ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = crs_x,
        x_geom     = x_geom
    )

    return(result)

}





#' @rdname ddbs_binary_funs
#' @export
ddbs_sym_difference <- function(
    x,
    y,
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    

    # 0. Handle errors
    assert_xy(x, "x")
    assert_xy(y, "y")
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
    if (any(is.null(conn_x), is.null(conn_y))) {
        on.exit(resolve_conn$cleanup(), add = TRUE)   
    }

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

    ## 3.2. Build the base query
    st_function <- glue::glue("{x_geom}")
    base.query <- glue::glue("
        WITH symdiff_geom AS (
            SELECT 
                v1.* REPLACE (
                    ST_Union(
                        ST_Difference(
                            ST_MakeValid(v1.{x_geom}),
                            ST_MakeValid(v2.{y_geom})
                        ),
                        ST_Difference(
                            ST_MakeValid(v2.{y_geom}),
                            ST_MakeValid(v1.{x_geom})
                        )
                    ) AS {x_geom}
                ),
                v2.* EXCLUDE ({y_geom})
            FROM 
                {x_list$query_name} v1, 
                {y_list$query_name} v2
            WHERE NOT ST_IsEmpty(
                ST_Union(
                    ST_Difference(
                        ST_MakeValid(v1.{x_geom}),
                        ST_MakeValid(v2.{y_geom})
                    ),
                    ST_Difference(
                        ST_MakeValid(v2.{y_geom}),
                        ST_MakeValid(v1.{x_geom})
                    )
                )
            )
        )
        SELECT 
            * REPLACE ({build_geom_query(st_function, name, crs_x, mode)} AS {x_geom})
        FROM symdiff_geom;
    ")


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {
        name_list <- get_query_name(name)
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            {base.query}
        ")

        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. Apply geospatial operation
    result <- ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = crs_x,
        x_geom     = x_geom
    )

    return(result)
}

#' Get the boundary of geometries
#'
#' Returns the boundary of geometries as a new geometry, e.g., the edges of polygons 
#' or the start/end points of lines.
#'
#' @template x
#' @template conn_null
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
#' # read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' # store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' # boundary
#' b <- ddbs_boundary(x = "argentina", conn)
#' }
ddbs_boundary <- function(
    x,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(output, "output")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- detect_crs(x)
    sf_col_x <- attr(x, "sf_column")

    ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 2.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query 
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_Boundary({x_geom}) as {x_geom} 
            FROM {x_list$query_name};
        ")
        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. create the base query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_AsWKB(ST_Boundary({x_geom})) as {x_geom} 
        FROM {x_list$query_name};
    ")
    ## send the query
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





#' Get the envelope (bounding box) of geometries
#'
#' Returns the minimum axis-aligned rectangle that fully contains the geometry.
#'
#' @template x
#' @param by_feature Logical. If \code{TRUE}, returns one envelope per feature.
#' If \code{FALSE} (default), returns a single envelope for all geometries combined.
#' @template conn_null
#' @template name
#' @template crs
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @details
#' ST_Envelope returns the minimum bounding rectangle (MBR) of a geometry as a
#' polygon. For points and lines, this creates a rectangular polygon that
#' encompasses the geometry. For polygons, it returns the smallest rectangle
#' that contains the entire polygon.
#'
#' When \code{by_feature = FALSE}, all geometries are combined and a single envelope
#' is returned that encompasses the entire dataset.
#'
#' @template returns_output
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#'
#' # read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' # input as sf, and output as sf
#' env <- ddbs_envelope(x = argentina_ddbs, by_feature = TRUE)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' # store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' # envelope for each feature
#' env <- ddbs_envelope("argentina", conn, by_feature = TRUE)
#'
#' # single envelope for entire dataset
#' env_all <- ddbs_envelope("argentina", conn, by_feature = FALSE)
#'
#' # create a new table with envelopes
#' ddbs_envelope("argentina", conn, name = "argentina_bbox", by_feature = TRUE)
#' }
ddbs_envelope <- function(
    x,
    by_feature = FALSE,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_logic(by_feature, "by_feature")
    assert_name(name)
    assert_conn_character(conn, x)
    assert_name(output, "output")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- detect_crs(x)
    sf_col_x <- attr(x, "sf_column")

    ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 2.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)


    ## 3.3. Build envelope clause based on by_feature
    if (isTRUE(by_feature)) {
        st_envelope_clause <- glue::glue("ST_Envelope({x_geom})")
    } else {
        st_envelope_clause <- glue::glue("ST_Envelope(ST_Collect(LIST({x_geom})))")
    }


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query 
        if (isTRUE(by_feature)) {
            tmp.query <- glue::glue("
                SELECT {x_rest}
                {st_envelope_clause} as {x_geom}
                FROM {x_list$query_name};
            ")
        } else {
            tmp.query <- glue::glue("
                SELECT {st_envelope_clause} as {x_geom},
                FIRST({crs_column}) as {crs_column}
                FROM {x_list$query_name};
            ")
        }

        ## execute query
        DBI::dbExecute(target_conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. create the base query
    if (isTRUE(by_feature)) {
        tmp.query <- glue::glue("
            SELECT {x_rest}
            ST_AsWKB({st_envelope_clause}) as {x_geom}
            FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT ST_AsWKB({st_envelope_clause}) as {x_geom},
            FIRST({crs_column}) as {crs_column}
            FROM {x_list$query_name};
        ")
    }

    ## send the query
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





#' Get the bounding box of geometries
#'
#' Returns the minimal rectangle that encloses the geometry, typically used 
#' to summarize its spatial extent.
#'
#' @template x
#' @param by_feature Boolean. The function defaults to `FALSE`, and returns a
#'        single bounding box for `x`. If `TRUE`, it return one bounding box for
#'        each feature.
#' @template conn_null
#' @template name
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns A data frame or \code{TRUE} (invisibly) for table creation when name is not NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' # option 1: passing sf objects
#' ddbs_bbox(argentina_ddbs)
#'
#'
#' ## option 2: passing the names of tables in a duckdb db
#'
#' # creates a duckdb write sf to it
#' conn <- duckspatial::ddbs_create_conn()
#' ddbs_write_vector(conn, argentina_ddbs, "argentina_tbl", overwrite = TRUE)
#'
#' output2 <- ddbs_bbox(
#'     conn = conn,
#'     x = "argentina_tbl",
#'     name = "argentina_bbox"
#' )
#'
#' DBI::dbReadTable(conn, "argentina_bbox")
#' }
ddbs_bbox <- function(
    x,
    by_feature = FALSE,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    # 0. Handle errors
    assert_xy(x, "x")
    assert_logic(by_feature, "by_feature")
    assert_conn_character(conn, x)
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_connflict(conn, xy = x, ref = "x")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- detect_crs(x)
    sf_col_x <- attr(x, "sf_column")

    ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 2.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

    ## 3.3. Build base query - set the extent_clause
    if (isTRUE(by_feature)) {
        st_extent_clause <- glue::glue("ST_Extent({x_geom})")
    } else {
        st_extent_clause <- glue::glue("ST_Extent(ST_Collect(LIST({x_geom})))")
    }

    tmp.query <- glue::glue(
        "SELECT
            ST_XMin(ext) AS min_x,
            ST_YMin(ext) AS min_y,
            ST_XMax(ext) AS max_x,
            ST_YMax(ext) AS max_y
         FROM (
            SELECT {st_extent_clause} AS ext
            FROM {x_list$query_name}
            );"
        )


    # 5. if name is not NULL (i.e. no data frame returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## execute area query
        DBI::dbExecute(target_conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))

        if (isFALSE(quiet)) {
            cli::cli_alert_success("Query successful")
        }

        return(invisible(TRUE))
    }

    # 4. Get data frame
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

    # class(data_tbl) <- "bbox"

    if (isFALSE(quiet)) cli::cli_alert_success("Query successful")
    return(data_tbl)
}

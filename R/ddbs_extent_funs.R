#' Get the boundary of geometries
#'
#' Returns the boundary of geometries as a new geometry, e.g., the edges of polygons 
#' or the start/end points of lines.
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
#' ddbs_write_table(conn, argentina_ddbs, "argentina")
#'
#' # boundary
#' b <- ddbs_boundary(x = "argentina", conn)
#' }
ddbs_boundary <- function(
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
        fun = "ST_Boundary",
        other_args = NULL
    )
    
}





#' Get the envelope (bounding box) of geometries
#'
#' Returns the minimum axis-aligned rectangle that fully contains the geometry.
#'
#' @template x
#' @template by_feature
#' @template conn_null
#' @template name
#' @template mode
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
#' @template returns_mode
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
#' ddbs_write_table(conn, argentina_ddbs, "argentina")
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
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {


    ## 0. Handle errors
    assert_xy(x, "x")
    assert_logic(by_feature, "by_feature")
    assert_name(name)
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- ddbs_crs(x, conn)
    sf_col_x <- attr(x, "sf_column")

    ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn)

    ## 1.3. Get mode - If it's NULL, it will use the duckspatial.mode option
    mode <- get_mode(mode, name)


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
    x_rest <- if (isTRUE(by_feature)) {
        get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    } else {
        ""
    }

    ## 3.3. Build envelope clause based on by_feature
    if (isTRUE(by_feature)) {
        st_envelope_clause <- glue::glue("ST_Envelope({x_geom})")
    } else {
        st_envelope_clause <- glue::glue("ST_Envelope_Agg({x_geom})")
    }

    ## 3.4. Build base query
    base.query <- glue::glue("
        SELECT 
            {x_rest}
            {build_geom_query(st_envelope_clause, name, crs_x, mode)} as {x_geom}
        FROM 
            {x_list$query_name};
    ")


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query 
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS {base.query}
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





#' Get the bounding box of geometries
#'
#' Returns the minimal rectangle that encloses the geometry
#'
#' @template x
#' @template by_feature
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @returns 
#' A `bbox` numeric vector with `by_feature = FALSE` 
#' A `data.frame` or `lazy tbl` when `by_feature = TRUE`
#' 
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
#' ## option 2: passing the names of tables in a duckdb db
#'
#' # creates a duckdb write sf to it
#' conn <- duckspatial::ddbs_create_conn()
#' ddbs_write_table(conn, argentina_ddbs, "argentina_tbl", overwrite = TRUE)
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
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Handle errors
    assert_xy(x, "x")
    assert_logic(by_feature, "by_feature")
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_connflict(conn, xy = x, ref = "x")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- ddbs_crs(x, conn)
    sf_col_x <- attr(x, "sf_column")

    ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn)

     ## 1.3. Get mode - If it's NULL, it will use the duckspatial.mode option
    mode <- get_mode(mode, name)


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

    ## 3.3. Build base query - set the extent_clause
    if (isTRUE(by_feature)) {
        st_extent_clause <- glue::glue("ST_Extent({x_geom})")
    } else {
        st_extent_clause <- glue::glue("ST_Extent_Agg({x_geom})")
    }

    base.query <- glue::glue("
        SELECT
            ST_XMin(ext) AS xmin,
            ST_YMin(ext) AS ymin,
            ST_XMax(ext) AS xmax,
            ST_YMax(ext) AS ymax
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
        DBI::dbExecute(target_conn, glue::glue("CREATE TABLE {name_list$query_name} AS {base.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Apply geospatial operation based on mode
    if (mode == "sf" | isFALSE(by_feature)) {
      
        if (isTRUE(by_feature)) {
          
            return(DBI::dbGetQuery(target_conn, base.query))
          
        } else {
          
            ## Get data as a data frame
            data_tbl <- DBI::dbGetQuery(target_conn, base.query)
        
            ## Convert to bbox class
            bbox_vec <- structure(
                unlist(data_tbl),
                names = c("xmin", "ymin", "xmax", "ymax"),
                class = "bbox",
                crs   = crs_x
            )      
            return(bbox_vec)
          
        }
      
    } else {

        ## Generate the query
        view_name <- ddbs_temp_view_name()
        tmp.query <- glue::glue("CREATE TEMP TABLE {view_name} AS {base.query}")

        ## Create a table, and return a pointer to that table
        DBI::dbExecute(target_conn, tmp.query)
        data_tbl <- dplyr::tbl(target_conn, view_name)
        return(data_tbl)

    }
    
}

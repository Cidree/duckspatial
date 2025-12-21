#' Returns the boundary of geometries
#'
#' Returns the boundary of geometries from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' # read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' # store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' # boundary
#' b <- ddbs_boundary("argentina", conn)
#' }
ddbs_boundary <- function(
    x,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

    # 1. Manage connection to DB
    ## 1.1. check if connection is provided, otherwise create a temporary connection
    is_duckdb_conn <- dbConnCheck(conn)
    if (isFALSE(is_duckdb_conn)) {
      conn <- duckspatial::ddbs_create_conn()
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
    }
    ## 1.2. get query list of table names
    x_list <- get_query_list(x, conn)

    ## 2. get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Boundary({x_geom}}) as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Boundary({x_geom}) as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    ## 4. create the base query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Boundary({x_geom})) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_AsText(ST_Boundary({x_geom})) as {x_geom} FROM {x_list$query_name};
        ")
    }
    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF and return result
    data_sf <- convert_to_sf(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}





#' Returns the envelope (bounding box) of geometries
#'
#' Returns the minimum bounding rectangle (envelope) of geometries from a DuckDB table
#' using the spatial extension. Returns the result as an \code{sf} object or creates
#' a new table in the database.
#'
#' @template x
#' @param by_feature Logical. If \code{TRUE}, returns one envelope per feature.
#' If \code{FALSE} (default), returns a single envelope for all geometries combined.
#' @template conn_null
#' @template name
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @details
#' ST_Envelope returns the minimum bounding rectangle (MBR) of a geometry as a polygon.
#' For points and lines, this creates a rectangular polygon that encompasses the geometry.
#' For polygons, it returns the smallest rectangle that contains the entire polygon.
#'
#' When \code{by_feature = FALSE}, all geometries are combined and a single envelope
#' is returned that encompasses the entire dataset.
#'
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' # read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' # input as sf, and output as sf
#' env <- ddbs_envelope(x = argentina_sf, by_feature = TRUE)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' # store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
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
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(by_feature, "by_feature")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

    # 1. Manage connection to DB
    ## 1.1. check if connection is provided, otherwise create a temporary connection
    is_duckdb_conn <- dbConnCheck(conn)
    if (isFALSE(is_duckdb_conn)) {
      conn <- duckspatial::ddbs_create_conn()
      on.exit(duckdb::dbDisconnect(conn), add = TRUE)
    }
    ## 1.2. get query list of table names
    x_list <- get_query_list(x, conn)

    ## 2. get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 3. Build envelope clause based on by_feature
    if (isTRUE(by_feature)) {
        st_envelope_clause <- glue::glue("ST_Envelope({x_geom})")
    } else {
        st_envelope_clause <- glue::glue("ST_Envelope(ST_Collect(LIST({x_geom})))")
    }


    ## 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (isTRUE(by_feature)) {
            if (length(x_rest) == 0) {
                tmp.query <- glue::glue("
                SELECT {st_envelope_clause} as {x_geom}
                FROM {x_list$query_name};
            ")
            } else {
                tmp.query <- glue::glue("
                SELECT {paste0(x_rest, collapse = ', ')}, {st_envelope_clause} as {x_geom}
                FROM {x_list$query_name};
            ")
            }
        } else {
            tmp.query <- glue::glue("
            SELECT {st_envelope_clause} as {x_geom},
            FIRST({crs_column}) as {crs_column}
            FROM {x_list$query_name};
        ")
        }

        ## execute query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    ## 5. create the base query
    if (isTRUE(by_feature)) {
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
                SELECT ST_AsText({st_envelope_clause}) as {x_geom}
                FROM {x_list$query_name};
            ")
        } else {
            tmp.query <- glue::glue("
                SELECT {paste0(x_rest, collapse = ', ')},
                ST_AsText({st_envelope_clause}) as {x_geom}
                FROM {x_list$query_name};
            ")
        }
    } else {
        tmp.query <- glue::glue("
            SELECT ST_AsText({st_envelope_clause}) as {x_geom},
            FIRST({crs_column}) as {crs_column}
            FROM {x_list$query_name};
        ")
    }

    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 6. convert to SF and return result
    data_sf <- convert_to_sf(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}





#' Returns the minimal bounding box enclosing the input geometry
#'
#' Returns the minimal bounding box enclosing the input geometry from a `sf` object
#' or a DuckDB table. Returns the result as an \code{sf} object or creates a new
#' table in the database.
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
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' # option 1: passing sf objects
#' ddbs_bbox(argentina_sf)
#'
#'
#' ## option 2: passing the names of tables in a duckdb db
#'
#' # creates a duckdb write sf to it
#' conn <- duckspatial::ddbs_create_conn()
#' ddbs_write_vector(conn, argentina_sf, "argentina_tbl", overwrite = TRUE)
#'
#' output2 <- ddbs_bbox(
#'     conn = conn,
#'     x = "argentina_tbl",
#'     name = "argentina_bbox"
#' )
#'
#' DBI::dbReadTable(conn, "argentina_bbox")
#'
ddbs_bbox <- function(x,
                      by_feature = FALSE,
                      conn = NULL,
                      name = NULL,
                      crs = NULL,
                      crs_column = "crs_duckspatial",
                      overwrite = FALSE,
                      quiet = FALSE) {

    # 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(by_feature, "by_feature")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_connflict(conn, xy = x, ref = "x")

    # 1. Manage connection to DB
    ## 1.1. check if connection is provided
    is_duckdb_conn <- dbConnCheck(conn)

    ## 1.2. prepares info for running the function on a temporary db
    if (isFALSE(is_duckdb_conn)) {

        # create conn
        conn <- duckspatial::ddbs_create_conn()

        # write tables, and get convenient names for x
        duckspatial::ddbs_write_vector(conn, data = x, name = "tbl_x", quiet = TRUE, temp_view = TRUE)
        x_list <- get_query_name("tbl_x")

    } else {
        x_list <- get_query_name(x)
    }

    ## 2. get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    assert_geometry_column(x_geom, x_list)


    # 3. Build base query

    # set the extent_clause
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



    ## 3. if name is not NULL (i.e. no data frame returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        if (overwrite) {
            DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name_list$query_name};"))

            if (isFALSE(quiet)) {
                cli::cli_alert_info("Table <{name_list$query_name}> dropped")
            }
        }

        ## execute area query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))

        if (isFALSE(quiet)) {
            cli::cli_alert_success("Query successful")
        }

        return(invisible(TRUE))
    }

    # 4. Get data frame
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    # class(data_tbl) <- "bbox"

    if (isFALSE(quiet)) cli::cli_alert_success("Query successful")
    return(data_tbl)
}

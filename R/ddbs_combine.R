#' Union of geometries
#'
#' Computes the union of geometries from a `sf` objects or a DuckDB tables using.
#' This is equivalent to \code{sf::st_union()}. The function supports three modes:
#' (1) union all geometries from a single object into one geometry,
#' (2) union geometries from a single object grouped by one or more columns,
#' (3) union geometries from two different objects.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param y optional. A second table name, \code{sf} object, or DuckDB connection
#' to compute the pairwise union between geometries in \code{x} and \code{y}.
#' Default is \code{NULL}
#' @param by optional. Character vector specifying one or more column names to
#' group by when computing unions. Geometries will be unioned within each group.
#' Default is \code{NULL}
#' @template conn_null
#' @template name
#' @template crs
#' @param crs_column character string specifying the name of the CRS column.
#' Default is \code{"crs_duckspatial"}
#' @template overwrite
#' @template quiet
#'
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
#' \dontrun{
#' # load packages
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' # read data
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))
#'
#' # store in duckdb
#' ddbs_write_vector(conn, rivers_sf, "rivers")
#'
#' # union all geometries into one
#' ddbs_union(conn = conn, "rivers")
#'
#' # union without using a connection
#' ddbs_union(rivers_sf)
#'
#' # union geometries grouped by a column
#' ddbs_union(conn = conn, "rivers", by = "RIVER_NAME")
#'
#' # store result in a new table
#' ddbs_union(conn = conn, "rivers", name = "rivers_union")
#' }
ddbs_union <- function(
    x,
    y = NULL,
    by = NULL,
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
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = FALSE)
    assert_geometry_column(x_geom, x_list)

    ## Handle ST_Union(x, y) - pairwise union of two geometries
    if (!is.null(y)) {
        ## Check y
        assert_xy(y, "y")
        assert_conn_character(conn, y)

        ## Get y query list
        y_list <- get_query_list(y, conn)

        ## Get y geometry column
        y_geom <- get_geom_name(conn, y_list$query_name)
        # y_rest <- get_geom_name(conn, y_list$query_name, rest = TRUE)
        assert_geometry_column(y_geom, y_list)

        ## 3. if name is not NULL (i.e. no SF returned)
        if (!is.null(name)) {
            ## convenient names of table and/or schema.table
            name_list <- get_query_name(name)

            ## handle overwrite
            overwrite_table(name_list$query_name, conn, quiet, overwrite)

            ## create query for pairwise union (no st_as_text)
            ## assuming row-wise union based on row number
            if (crs_column %in% x_rest) {
                tmp.query <- glue::glue("
                    SELECT
                        ROW_NUMBER() OVER () as row_id,
                        x.{crs_column},
                        ST_Union(x.{x_geom}, y.{y_geom}) as {x_geom}
                    FROM
                        (SELECT ROW_NUMBER() OVER () as rn, * FROM {x_list$query_name}) x
                    JOIN
                        (SELECT ROW_NUMBER() OVER () as rn, * FROM {y_list$query_name}) y
                    ON x.rn = y.rn;
                ")
            } else {
                tmp.query <- glue::glue("
                    SELECT
                        ROW_NUMBER() OVER () as row_id,
                        ST_Union(x.{x_geom}, y.{y_geom}) as {x_geom}
                    FROM
                        (SELECT ROW_NUMBER() OVER () as rn, * FROM {x_list$query_name}) x
                    JOIN
                        (SELECT ROW_NUMBER() OVER () as rn, * FROM {y_list$query_name}) y
                    ON x.rn = y.rn;
                ")
            }

            ## execute union query
            DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
            feedback_query(quiet)
            return(invisible(TRUE))
        }

        ## 4. create the base query with ST_AsWKB
        if (crs_column %in% x_rest) {
            tmp.query <- glue::glue("
                SELECT
                    ROW_NUMBER() OVER () as row_id,
                    x.{crs_column},
                    ST_AsWKB(ST_Union(x.{x_geom}, y.{y_geom})) as {x_geom}
                FROM
                    (SELECT ROW_NUMBER() OVER () as rn, * FROM {x_list$query_name}) x
                JOIN
                    (SELECT ROW_NUMBER() OVER () as rn, * FROM {y_list$query_name}) y
                ON x.rn = y.rn;
            ")
        } else {
            tmp.query <- glue::glue("
                SELECT
                    ROW_NUMBER() OVER () as row_id,
                    ST_AsWKB(ST_Union(x.{x_geom}, y.{y_geom})) as {x_geom}
                FROM
                    (SELECT ROW_NUMBER() OVER () as rn, * FROM {x_list$query_name}) x
                JOIN
                    (SELECT ROW_NUMBER() OVER () as rn, * FROM {y_list$query_name}) y
                ON x.rn = y.rn;
            ")
        }

        ## send the query
        data_tbl <- DBI::dbGetQuery(conn, tmp.query)

        ## 5. convert to SF and return result
        data_sf <- convert_to_sf_wkb(
            data       = data_tbl,
            crs        = crs,
            crs_column = crs_column,
            x_geom     = x_geom
        )

        feedback_query(quiet)
        return(data_sf)
    }

    ## Handle ST_Union_Agg(x) - aggregate union
    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (is.null(by)) {
            # Union all geometries into a single geometry
            # Keep crs_column if it exists
            if (crs_column %in% x_rest) {
                tmp.query <- glue::glue("
                    SELECT FIRST({crs_column}) as {crs_column}, ST_Union_Agg({x_geom}) as {x_geom}
                    FROM {x_list$query_name};
                ")
            } else {
                tmp.query <- glue::glue("
                    SELECT ST_Union_Agg({x_geom}) as {x_geom} FROM {x_list$query_name};
                ")
            }
        } else {
            # Union geometries grouped by specified columns
            by_cols <- paste0(by, collapse = ", ")

            # Remove crs_column from other columns to handle it separately
            other_cols <- setdiff(x_rest, c(by, crs_column))

            if (crs_column %in% x_rest) {
                if (length(other_cols) == 0) {
                    tmp.query <- glue::glue("
                        SELECT {by_cols}, FIRST({crs_column}) as {crs_column}, ST_Union_Agg({x_geom}) as {x_geom}
                        FROM {x_list$query_name}
                        GROUP BY {by_cols};
                    ")
                } else {
                    other_cols_agg <- paste0("FIRST(", other_cols, ") as ", other_cols, collapse = ", ")
                    tmp.query <- glue::glue("
                        SELECT {by_cols}, {other_cols_agg}, FIRST({crs_column}) as {crs_column}, ST_Union_Agg({x_geom}) as {x_geom}
                        FROM {x_list$query_name}
                        GROUP BY {by_cols};
                    ")
                }
            } else {
                if (length(other_cols) == 0) {
                    tmp.query <- glue::glue("
                        SELECT {by_cols}, ST_Union_Agg({x_geom}) as {x_geom}
                        FROM {x_list$query_name}
                        GROUP BY {by_cols};
                    ")
                } else {
                    other_cols_agg <- paste0("FIRST(", other_cols, ") as ", other_cols, collapse = ", ")
                    tmp.query <- glue::glue("
                        SELECT {by_cols}, {other_cols_agg}, ST_Union_Agg({x_geom}) as {x_geom}
                        FROM {x_list$query_name}
                        GROUP BY {by_cols};
                    ")
                }
            }
        }

        ## execute union query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    ## 4. create the base query
    if (is.null(by)) {
        # Union all geometries into a single geometry
        if (crs_column %in% x_rest) {
            tmp.query <- glue::glue("
                SELECT FIRST({crs_column}) as {crs_column}, ST_AsWKB(ST_Union_Agg({x_geom})) as {x_geom}
                FROM {x_list$query_name};
            ")
        } else {
            tmp.query <- glue::glue("
                SELECT ST_AsWKB(ST_Union_Agg({x_geom})) as {x_geom} FROM {x_list$query_name};
            ")
        }
    } else {
        # Union geometries grouped by specified columns
        by_cols <- paste0(by, collapse = ", ")

        # Remove crs_column from other columns to handle it separately
        other_cols <- setdiff(x_rest, c(by, crs_column))

        if (crs_column %in% x_rest) {
            if (length(other_cols) == 0) {
                tmp.query <- glue::glue("
                    SELECT {by_cols}, FIRST({crs_column}) as {crs_column}, ST_AsWKB(ST_Union_Agg({x_geom})) as {x_geom}
                    FROM {x_list$query_name}
                    GROUP BY {by_cols};
                ")
            } else {
                other_cols_agg <- paste0("FIRST(", other_cols, ") as ", other_cols, collapse = ", ")
                tmp.query <- glue::glue("
                    SELECT {by_cols}, {other_cols_agg}, FIRST({crs_column}) as {crs_column}, ST_AsWKB(ST_Union_Agg({x_geom})) as {x_geom}
                    FROM {x_list$query_name}
                    GROUP BY {by_cols};
                ")
            }
        } else {
            if (length(other_cols) == 0) {
                tmp.query <- glue::glue("
                    SELECT {by_cols}, ST_AsWKB(ST_Union_Agg({x_geom})) as {x_geom}
                    FROM {x_list$query_name}
                    GROUP BY {by_cols};
                ")
            } else {
                other_cols_agg <- paste0("FIRST(", other_cols, ") as ", other_cols, collapse = ", ")
                tmp.query <- glue::glue("
                    SELECT {by_cols}, {other_cols_agg}, ST_AsWKB(ST_Union_Agg({x_geom})) as {x_geom}
                    FROM {x_list$query_name}
                    GROUP BY {by_cols};
                ")
            }
        }
    }

    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF and return result
    data_sf <- convert_to_sf_wkb(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}





#' Combine geometries into a single MULTI-geometry
#'
#' Combines all geometries from a `sf` object or a DuckDB table into a single
#' MULTI-geometry using the spatial extension. This is equivalent to
#' \code{sf::st_combine()}. Returns the result as an \code{sf} object or creates
#' a new table in the database.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template crs
#' @param crs_column character string specifying the name of the CRS column.
#' Default is \code{"crs_duckspatial"}
#' @template overwrite
#' @template quiet
#'
#' @returns an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
#' \dontrun{
#' # load packages
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' # read data
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#'
#' # store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#'
#' # combine all geometries into one
#' ddbs_combine(conn = conn, "countries")
#'
#' # combine without using a connection
#' ddbs_combine(countries_sf)
#'
#' # store result in a new table
#' ddbs_combine(conn = conn, "countries", name = "countries_combined")
#' }
ddbs_combine <- function(
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
    assert_geometry_column(x_geom, x_list)



    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create the query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT
                ST_Collect(LIST({x_geom})) as {x_geom},
                FIRST({crs_column}) as {crs_column}
            FROM
                {x_list$query_name};
        ")

        ## execute the query
        DBI::dbExecute(conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    
    }

    ## 4. if name is NULL (sf returned)

    ## create the query
    tmp.query <- glue::glue("
        SELECT
            ST_AsWKB(ST_Collect(LIST({x_geom}))) as {x_geom},
            FIRST({crs_column}) as {crs_column}
        FROM
            {x_list$query_name};
    ")

    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF and return result
    data_sf <- convert_to_sf_wkb(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}




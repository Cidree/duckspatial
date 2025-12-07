

#' Calculates the area of geometries
#'
#' Calculates the area of geometries from a DuckDB table or a `sf` object
#' Returns the result as an \code{sf} object with an area column or creates a new table in the database.
#' Note: Area units depend on the CRS of the input geometries (e.g., square meters for projected CRS, 
#' or degrees for geographic CRS).
#'
#' @template x
#' @template conn_null
#' @template name
#' @template new_column
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns a vector, an \code{sf} object or \code{TRUE} (invisibly) for table creation
#' @export
#'
#' @examples
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#' 
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial")) |> 
#'     st_transform("EPSG:3857")
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#' 
#' ## calculate area (returns sf object with area column)
#' ddbs_area("argentina", conn)
#' 
#' ## calculate area with custom column name
#' ddbs_area("argentina", conn, new_column = "area_sqm")
#' 
#' ## create a new table with area calculations
#' ddbs_area("argentina", conn, name = "argentina_with_area")
#' 
#' ## calculate area in a sf object
#' ddbs_area(argentina_sf)
ddbs_area <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Handle errors
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
    

    # 2. Get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 3. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_Area({x_geom}) as area, 
            FROM {x_list$query_name}
          ")

          data_vec <- DBI::dbGetQuery(conn, tmp.query)
          return(data_vec[, 1])
    }

    ## 4. if name is not NULL (i.e. no data frame returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Area({x_geom}) AS {new_column}, {x_geom}
            FROM {x_list$query_name}
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Area({x_geom}) AS {new_column}, {x_geom}
            FROM {x_list$query_name}
        ")
        }
        ## execute area query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Get data frame
    ## 5.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_Area({x_geom}) AS {new_column}, ST_AsText({x_geom}) as {x_geom}
            FROM {x_list$query_name}
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Area({x_geom}) AS {new_column}, ST_AsText({x_geom}) as {x_geom}
            FROM {x_list$query_name}
        ")
    }
    ## 5.2. retrieve results of the query
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






#' Calculates the length of geometries
#'
#' Calculates the length of geometries from a DuckDB table or a `sf` object
#' Returns the result as an \code{sf} object with a length column or creates a new table in the database.
#' Note: Length units depend on the CRS of the input geometries (e.g., meters for projected CRS, 
#' or degrees for geographic CRS).
#'
#' @template x
#' @template conn_null
#' @template name
#' @template new_column
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
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read data
#' rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, rivers_sf, "rivers")
#' 
#' ## calculate length (returns sf object with length column)
#' ddbs_length("rivers", conn)
#' 
#' ## calculate length with custom column name
#' ddbs_length("rivers", conn, new_column = "length_meters")
#' 
#' ## create a new table with length calculations
#' ddbs_length("rivers", conn, name = "rivers_with_length")
#' 
#' ## calculate length in a sf object (without a connection)
#' ddbs_length(rivers_sf)
ddbs_length <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Handle errors
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
    
    # 2. Get name of geometry column
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 3. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_Length({x_geom}) as length, 
            FROM {x_list$query_name}
          ")

          data_vec <- DBI::dbGetQuery(conn, tmp.query)
          return(data_vec[, 1])
    }

    ## 4. if name is not NULL (i.e. no data frame returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Length({x_geom}) AS {new_column}, {x_geom}
            FROM {x_list$query_name}
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Length({x_geom}) AS {new_column}, {x_geom}
            FROM {x_list$query_name}
        ")
        }
        ## execute length query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)

        return(invisible(TRUE))
    }

    # 5. Get data frame
    ## 5.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_Length({x_geom}) AS {new_column}, ST_AsText({x_geom}) as {x_geom}
            FROM {x_list$query_name}
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Length({x_geom}) AS {new_column}, ST_AsText({x_geom}) as {x_geom}
            FROM {x_list$query_name}
        ")
    }
    ## 5.2. retrieve results of the query
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

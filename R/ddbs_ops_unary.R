


#' Creates a buffer around geometries
#'
#' Calculates the buffer of geometries from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param distance a numeric value specifying the buffer distance. Units correspond to
#' the coordinate system of the geometry (e.g. degrees or meters)
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
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## buffer
#' ddbs_buffer(conn = conn, "argentina", distance = 1)
#' 
#' ## buffer without using a connection
#' ddbs_buffer(argentina_sf, distance = 1)
#' }
ddbs_buffer <- function(
    x,
    distance,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_numeric(distance, "distance")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

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
            SELECT ST_Buffer({x_geom}, {distance}) as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Buffer({x_geom}, {distance}) as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Buffer({x_geom}, {distance})) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_AsText(ST_Buffer({x_geom}, {distance})) as {x_geom} FROM {x_list$query_name};
        ")
    }
    ## 4.2. retrieve results from the query
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





#' Calculates the centroid of geometries
#'
#' Calculates the centroids of geometries from a DuckDB table using the spatial extension.
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
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## centroid
#' ddbs_centroid("argentina", conn)
#' 
#' ## centroid without using a connection
#' ddbs_centroid(argentina_sf)
#' }
ddbs_centroid <- function(x,
                        conn = NULL,
                        name = NULL,
                        crs = NULL,
                        crs_column = "crs_duckspatial",
                        overwrite = FALSE,
                        quiet     = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

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
            SELECT ST_Centroid({x_geom}}) as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Centroid({x_geom}) as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Centroid({x_geom})) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_AsText(ST_Centroid({x_geom})) as {x_geom} FROM {x_list$query_name};
        ")
    }
    ## 4.2. retrieve results from the query
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





#' Check if geometries are valid
#'
#' Checks the validity of geometries from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object with a boolean validity column or creates 
#' a new table in the database.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template new_column
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns a vector, an \code{sf} object with validity information or \code{TRUE} (invisibly) for table creation
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
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## check validity
#' ddbs_is_valid("argentina", conn)
#' 
#' ## check validity without using a connection
#' ddbs_is_valid(argentina_sf)
#' }
ddbs_is_valid <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

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
    
    ## 3. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_IsValid({x_geom}) as isvalid, 
            FROM {x_list$query_name}
          ")

          data_vec <- DBI::dbGetQuery(conn, tmp.query)
          return(data_vec[, 1])
    }

    ## 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_IsValid({x_geom}) as {new_column}, 
            {x_geom}
            FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, 
            ST_IsValid({x_geom}) as {new_column}, 
            {x_geom} 
            FROM {x_list$query_name};
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Get data frame
    ## 5.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_IsValid({x_geom}) as {new_column}, 
            ST_AsText({x_geom}) as {x_geom}
            FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, 
            ST_IsValid({x_geom}) as {new_column}, 
            ST_AsText({x_geom}) as {x_geom} 
            FROM {x_list$query_name};
        ")
    }
    ## 5.2. retrieve results from the query
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





#' Make invalid geometries valid
#'
#' Attempts to make invalid geometries valid from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template crs
#' @param crs_column Name of the column to store CRS information. Default is "crs_duckspatial".
#' @template overwrite
#' @template quiet
#'
#' @returns an \code{sf} object with valid geometries or \code{TRUE} (invisibly) for table creation
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
#' ## read data
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#'
#' ## make valid
#' ddbs_make_valid("countries", conn)
#' 
#' ## make valid without using a connection
#' ddbs_make_valid(countries_sf)
#' }
ddbs_make_valid <- function(
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
            SELECT ST_MakeValid({x_geom}) as {x_geom}
            FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, 
            ST_MakeValid({x_geom}) as {x_geom} 
            FROM {x_list$query_name};
        ")
        }
        ## execute query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_MakeValid({x_geom})) as {x_geom}
            FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, 
            ST_AsText(ST_MakeValid({x_geom})) as {x_geom} 
            FROM {x_list$query_name};
        ")
    }
    ## 4.2. retrieve results from the query
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





#' Check if geometries are simple
#'
#' Checks if geometries are simple (no self-intersections) from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object with a boolean simplicity column or creates 
#' a new table in the database.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template new_column 
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns a vector, an \code{sf} object with simplicity information or \code{TRUE} (invisibly) for table creation
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
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## check simplicity
#' ddbs_is_simple("argentina", conn)
#' 
#' ## check simplicity without using a connection
#' ddbs_is_simple(argentina_sf)
#' }
ddbs_is_simple <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

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

    ## 3. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_IsSimple({x_geom}) as issimple, 
            FROM {x_list$query_name}
          ")

          data_vec <- DBI::dbGetQuery(conn, tmp.query)
          return(data_vec[, 1])
    }

    ## 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

      ## convenient names of table and/or schema.table
      name_list <- get_query_name(name)

      ## handle overwrite
      overwrite_table(name_list$query_name, conn, quiet, overwrite)

      ## create query (no st_as_text)
      if (length(x_rest) == 0) {
          tmp.query <- glue::glue("
            SELECT ST_IsSimple({x_geom}) as {new_column}, 
            {x_geom}
            FROM {x_list$query_name};
        ")
      } else {
          tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, 
            ST_IsSimple({x_geom}) as {new_column}, 
            {x_geom} 
            FROM {x_list$query_name};
        ")
      }
        ## execute query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
      
    }

    # 5. Get data frame
    ## 5.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_IsSimple({x_geom}) as {new_column}, 
            ST_AsText({x_geom}) as {x_geom}
            FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, 
            ST_IsSimple({x_geom}) as {new_column}, 
            ST_AsText({x_geom}) as {x_geom} 
            FROM {x_list$query_name};
        ")
    }
    ## 5.2. retrieve results from the query
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





#' Simplify geometries
#'
#' Simplifies geometries from a DuckDB table using the Douglas-Peucker algorithm via the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @template conn_null
#' @template name
#' @param tolerance Tolerance distance for simplification. Larger values result in more simplified geometries.
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns an \code{sf} object with simplified geometries or \code{TRUE} (invisibly) for table creation
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
#' ## read data
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#'
#' ## simplify with tolerance of 0.01
#' ddbs_simplify("countries", conn, tolerance = 0.01)
#' 
#' ## simplify without using a connection
#' ddbs_simplify(countries_sf, tolerance = 0.01)
#' }
ddbs_simplify <- function(
    x,
    conn = NULL,
    name = NULL,
    tolerance,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    if (missing(tolerance)) {
        stop("tolerance parameter is required")
    }

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
            SELECT ST_Simplify({x_geom}, {tolerance}) as {x_geom}
            FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, 
            ST_Simplify({x_geom}, {tolerance}) as {x_geom} 
            FROM {x_list$query_name};
        ")
        }
        ## execute query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Simplify({x_geom}, {tolerance})) as {x_geom}
            FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, 
            ST_AsText(ST_Simplify({x_geom}, {tolerance})) as {x_geom} 
            FROM {x_list$query_name};
        ")
    }
    ## 4.2. retrieve results from the query
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

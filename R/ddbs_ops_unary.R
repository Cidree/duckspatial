


#' Creates a buffer around geometries
#'
#' Calculates the buffer of geometries from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param distance a numeric value specifying the buffer distance. Units correspond to
#' the coordinate system of the geometry (e.g. degrees or meters)
#' @param num_triangles an integer representing how many triangles will be produced to 
#' approximate a quarter circle. The larger the number, the smoother the resulting geometry. 
#' Default is 8.
#' @param cap_style a character string specifying the cap style. Must be one of 
#' "CAP_ROUND" (default), "CAP_FLAT", or "CAP_SQUARE". Case-insensitive.
#' @param join_style a character string specifying the join style. Must be one of 
#' "JOIN_ROUND" (default), "JOIN_MITRE", or "JOIN_BEVEL". Case-insensitive.
#' @param mitre_limit a numeric value specifying the mitre limit ratio. Only applies when 
#' \code{join_style} is "JOIN_MITRE". It is the ratio of the distance from the corner to 
#' the mitre point to the corner radius. Default is 1.0.
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
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## basic buffer
#' ddbs_buffer(conn = conn, "argentina", distance = 1)
#'
#' ## buffer with custom parameters
#' ddbs_buffer(conn = conn, "argentina", distance = 1, 
#'             num_triangles = 16, cap_style = "CAP_SQUARE")
#'
#' ## buffer without using a connection
#' ddbs_buffer(argentina_ddbs, distance = 1)
#' }
ddbs_buffer <- function(
    x,
    distance,
    num_triangles = 8L,
    cap_style = "CAP_ROUND",
    join_style = "JOIN_ROUND",
    mitre_limit = 1.0,
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
    assert_name(name)
    assert_numeric(distance, "distance")
    assert_integer_scalar(num_triangles, "num_triangles")
    assert_character_scalar(cap_style, "cap_style")
    assert_character_scalar(join_style, "join_style")
    assert_numeric(mitre_limit, "mitre_limit")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)
  
    ## Validate cap_style
    valid_cap_styles <- c("CAP_ROUND", "CAP_FLAT", "CAP_SQUARE")
    if (!toupper(cap_style) %in% valid_cap_styles) {
        cli::cli_abort("{.arg cap_style} must be one of: {.val {paste0(valid_cap_styles, collapse = ', ')}}.")
    }
    
    ## Validate join_style
    valid_join_styles <- c("JOIN_ROUND", "JOIN_MITRE", "JOIN_BEVEL")
    if (!toupper(join_style) %in% valid_join_styles) {
        cli::cli_abort("{.arg join_style} must be one of: {.val {paste0(valid_join_styles, collapse = ', ')}}.")
    }
    
    ## Check num_triangles is positive
    if (num_triangles < 1) cli::cli_abort("{.arg num_triangles} must be a positive integer")
    
    ## Check mitre_limit is striclty positive
    if (mitre_limit <= 0) cli::cli_abort("{.arg mitre_limit} must be a positive number")
    

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

    ## 3.3. Build ST_Buffer parameters string
    buffer_params <- sprintf(
        "%s, %s, %d, '%s', '%s', %s",
        x_geom,
        distance,
        as.integer(num_triangles),
        toupper(cap_style),
        toupper(join_style),
        mitre_limit
    )

    # 4. if name is not NULL
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest} ST_Buffer({buffer_params}) as {x_geom} 
            FROM {x_list$query_name};
        ")
        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. Get data frame

    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest} 
        ST_AsWKB(ST_Buffer({buffer_params})) as {x_geom} 
        FROM {x_list$query_name};
    ")

    ## 5.2. retrieve results from the query
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





#' Calculates the centroid of geometries
#'
#' Calculates the centroids of geometries from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
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
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## centroid
#' ddbs_centroid("argentina", conn)
#'
#' ## centroid without using a connection
#' ddbs_centroid(argentina_ddbs)
#' }
ddbs_centroid <- function(
    x,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet     = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

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

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest} 
            ST_Centroid({x_geom}) as {x_geom} 
            FROM {x_list$query_name};
        ")
        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Get data frame

    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_AsWKB(ST_Centroid({x_geom})) as {x_geom} 
        FROM {x_list$query_name};
    ")

    ## 5.2. retrieve results from the query
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
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @returns When `new_column = NULL` it returns a logical vector. When `new_column` is not NULL, the
#' output depends on the \code{output} argument (or global preference set by \code{\link{ddbs_options}}):
#'   \itemize{
#'     \item \code{duckspatial_df} (default): A lazy spatial data frame backed by dbplyr/DuckDB.
#'     \item \code{sf}: An eagerly collected \code{sf} object in R memory.
#'     \item \code{tibble}: An eagerly collected \code{tibble} without geometry in R memory.
#'     \item \code{raw}: An eagerly collected \code{tibble} with WKB geometry (no conversion).
#'     \item \code{geoarrow}: An eagerly collected \code{tibble} with geometry converted to \code{geoarrow_vctr}.
#'   }
#'   When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## check validity
#' ddbs_is_valid("argentina", conn)
#'
#' ## check validity without using a connection
#' ddbs_is_valid(argentina_ddbs)
#' }
ddbs_is_valid <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

    if (!is.null(name) && is.null(new_column)) cli::cli_abort("Please, specify the {.arg new_column} name.")

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


    # 3. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_IsValid({x_geom}) as isvalid,
            FROM {x_list$query_name}
          ")

          data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
          feedback_query(quiet)
          return(data_vec[, 1])
    }


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_IsValid({x_geom}) as {new_column},
            {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. Get data frame

    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_IsValid({x_geom}) as {new_column},
        ST_AsWKB({x_geom}) as {x_geom}
        FROM {x_list$query_name};
    ")

    ## 5.2. retrieve results from the query
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





#' Make invalid geometries valid
#'
#' Attempts to make invalid geometries valid from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
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
#' ## load package
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
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#'
#' ## make valid
#' ddbs_make_valid("countries", conn)
#'
#' ## make valid without using a connection
#' ddbs_make_valid(countries_ddbs)
#' }
ddbs_make_valid <- function(
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
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

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

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_MakeValid({x_geom}) as {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. Get data frame

    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_AsWKB(ST_MakeValid({x_geom})) as {x_geom}
        FROM {x_list$query_name};
    ")

    ## 5.2. retrieve results from the query
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
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @returns When `new_column = NULL` it returns a logical vector. When `new_column` is not NULL, the
#' output depends on the \code{output} argument (or global preference set by \code{\link{ddbs_options}}):
#'   \itemize{
#'     \item \code{duckspatial_df} (default): A lazy spatial data frame backed by dbplyr/DuckDB.
#'     \item \code{sf}: An eagerly collected \code{sf} object in R memory.
#'     \item \code{tibble}: An eagerly collected \code{tibble} without geometry in R memory.
#'     \item \code{raw}: An eagerly collected \code{tibble} with WKB geometry (no conversion).
#'     \item \code{geoarrow}: An eagerly collected \code{tibble} with geometry converted to \code{geoarrow_vctr}.
#'   }
#'   When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## check simplicity
#' ddbs_is_simple("argentina", conn)
#'
#' ## check simplicity without using a connection
#' ddbs_is_simple(argentina_ddbs)
#' }
ddbs_is_simple <- function(
    x,
    conn = NULL,
    name = NULL,
    new_column = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

    if (!is.null(name) && is.null(new_column)) cli::cli_abort("Please, specify the {.arg new_column} name.")

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


    # 4. Handle new column = NULL
    if (is.null(new_column)) {
        tmp.query <- glue::glue("
            SELECT ST_IsSimple({x_geom}) as issimple,
            FROM {x_list$query_name}
          ")

          data_vec <- DBI::dbGetQuery(target_conn, tmp.query)
          feedback_query(quiet)
          return(data_vec[, 1])
    }

    ## 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {
        
        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_IsSimple({x_geom}) as {new_column},
            {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    
    }


    # 4. Get data frame

    ## 4.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_IsSimple({x_geom}) as {new_column},
        ST_AsWKB({x_geom}) as {x_geom}
        FROM {x_list$query_name};
    ")

    ## 4.2. retrieve results from the query
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
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @template returns_output
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
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
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#'
#' ## simplify with tolerance of 0.01
#' ddbs_simplify("countries", tolerance = 0.01, conn)
#'
#' ## simplify without using a connection
#' ddbs_simplify(countries_ddbs, tolerance = 0.01)
#' }
ddbs_simplify <- function(
    x,
    tolerance,
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
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)
    if (missing(tolerance)) cli::cli_abort("tolerance parameter is required")

    
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


    # 4. if name is not NULL
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_Simplify({x_geom}, {tolerance}) as {x_geom}
            FROM {x_list$query_name};
        ")

        ## execute query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    
    }


    # 5. Get data frame

    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_AsWKB(ST_Simplify({x_geom}, {tolerance})) as {x_geom}
        FROM {x_list$query_name};
    ")

    ## 5.2. retrieve results from the query
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






#' Extracts the exterior ring of polygon geometries
#'
#' Returns the exterior ring (outer boundary) of polygon geometries from a DuckDB table 
#' using the spatial extension. For multi-polygons, returns the exterior ring of each 
#' polygon component. Returns the result as an \code{sf} object or creates a new table 
#' in the database.
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
#' ## load package
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
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_ddbs, "countries")
#'
#' ## extract exterior ring
#' ddbs_exterior_ring(conn = conn, "countries")
#'
#' ## extract exterior ring without using a connection
#' ddbs_exterior_ring(countries_ddbs)
#' }
ddbs_exterior_ring <- function(
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
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

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

  
    # 4. if name is not NULL
    if (!is.null(name)) {
        
        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_ExteriorRing({x_geom}) as {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

  
    # 5. Get data frame
  
    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_AsWKB(ST_ExteriorRing({x_geom})) as {x_geom}
        FROM {x_list$query_name};
    ")
  
    ## 5.2. retrieve results from the query
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





#' Creates polygons from linestring geometries
#'
#' Constructs polygon geometries from linestring geometries in a DuckDB table using 
#' the spatial extension. The input linestrings must be closed (first and last points 
#' must be identical). Returns the result as an \code{sf} object or creates a new table 
#' in the database.
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
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#'
#' ## extract exterior ring as linestring, then convert back to polygon
#' ring_ddbs <- ddbs_exterior_ring(conn = conn, "argentina")
#' ddbs_make_polygon(conn = conn, ring_ddbs, name = "argentina_poly")
#'
#' ## create polygon without using a connection
#' ddbs_make_polygon(ring_ddbs)
#' }
ddbs_make_polygon <- function(
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
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

  
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

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_MakePolygon({x_geom}) as {x_geom}
            FROM {x_list$query_name};
        ")
        ## execute query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

  
    # 5. Get data frame
  
    ## 5.1. create query
    tmp.query <- glue::glue("
        SELECT {x_rest}
        ST_AsWKB(ST_MakePolygon({x_geom})) as {x_geom}
        FROM {x_list$query_name};
    ")
  
    ## 5.2. retrieve results from the query
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





#' Returns the concave hull enclosing the geometry
#'
#' Returns the concave hull enclosing the geometry from an \code{sf} object or
#' from a DuckDB table using the spatial extension. Returns the result as an
#' \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param ratio Numeric. The ratio parameter dictates the level of concavity; `1`
#'        returns the convex hull, while `0` indicates to return the most concave
#'        hull possible. Defaults to `0.5`.
#' @param allow_holes Boolean. If `TRUE` (the default), it allows the output to
#'        contain holes.
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
#' ## load package
#' library(duckspatial)
#' library(sf)
#'
#' # create points data
#' n <- 5
#' points_ddbs <- data.frame(
#'   id = 1,
#'   x = runif(n, min = -180, max = 180),
#'   y = runif(n, min = -90, max = 90)
#' ) |>
#'   ddbs_as_spatial(coords = c("x", "y"), crs = 4326) |>
#'   ddbs_combine()
#'
#' # option 1: passing ddbs or sf objects
#' output1 <- duckspatial::ddbs_concave_hull(points_ddbs, output = "sf")
#'
#' plot(output1)
#'
#'
#' # option 2: passing the name of a table in a duckdb db
#'
#' # creates a duckdb
#' conn <- duckspatial::ddbs_create_conn()
#'
#' # write sf to duckdb
#' ddbs_write_vector(conn, points_ddbs, "points_tbl")
#'
#' # spatial join
#' output2 <- duckspatial::ddbs_concave_hull(
#'  conn = conn,
#'  x = "points_tbl",
#'  output = "sf"
#' )
#'
#' plot(output2)
#'
#' }
ddbs_concave_hull <- function(
    x,
    ratio = 0.5,
    allow_holes = TRUE,
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
    assert_numeric_interval(ratio, 0, 1, "ratio")
    assert_logic(allow_holes, "allow_holes")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

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
            ST_ConcaveHull({x_geom}, {ratio}, {allow_holes}) as {x_geom} 
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
        ST_AsWKB(ST_ConcaveHull({x_geom}, {ratio}, {allow_holes})) as {x_geom} 
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





#' Returns the convex hull enclosing the geometry
#'
#' Returns the convex hull enclosing the geometry from an \code{sf} object or
#' from a DuckDB table using the spatial extension. Returns the result as an
#' \code{sf} object or creates a new table in the database.
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
#' ## load package
#' library(duckspatial)
#' library(sf)
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
#' # option 1: passing sf objects
#' output1 <- duckspatial::ddbs_convex_hull(x = argentina_ddbs, output = "sf")
#'
#' plot(output1["CNTR_NAME"])#' # store in duckdb
#'
#' # option 2: passing the name of a table in a duckdb db
#'
#' # creates a duckdb
#' conn <- duckspatial::ddbs_create_conn()
#'
#' # write sf to duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina_tbl")
#'
#' # spatial join
#' output2 <- duckspatial::ddbs_convex_hull(
#'  conn = conn,
#'  x = "argentina_tbl",
#'  output = "sf"
#' )
#'
#' plot(output2["CNTR_NAME"])
#' }
ddbs_convex_hull <- function(
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
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

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

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {x_rest}
            ST_ConvexHull({x_geom}) as {x_geom} 
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
        ST_AsWKB(ST_ConvexHull({x_geom})) as {x_geom} 
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





#' Transform coordinate reference system of geometries
#'
#' Transforms geometries from a DuckDB table to a different coordinate reference system
#' using the spatial extension. Works similarly to \code{sf::st_transform()}.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param y Target CRS. Can be:
#'   \itemize{
#'     \item A character string with EPSG code (e.g., "EPSG:4326")
#'     \item An \code{sf} object (uses its CRS)
#'     \item Name of a DuckDB table (uses its CRS)
#'   }
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
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#' 
#' ## transform to different CRS using EPSG code
#' ddbs_transform("argentina", "EPSG:3857", conn)
#' 
#' ## transform to match CRS of another object
#' argentina_3857_ddbs <- ddbs_transform(argentina_ddbs, "EPSG:3857")
#' ddbs_write_vector(conn, argentina_3857_ddbs, "argentina_3857")
#' ddbs_transform("argentina", argentina_3857_ddbs, conn)
#' 
#' ## transform to match CRS of another DuckDB table
#' ddbs_transform("argentina", "argentina_3857", conn)
#' 
#' ## transform without using a connection
#' ddbs_transform(argentina_ddbs, "EPSG:3857")
#' }
ddbs_transform <- function(
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
    quiet     = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

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
    try(y <- normalize_spatial_input(y, conn_y), silent = TRUE)


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

    ## if CRS wasn't guessed earlier
    if (is.null(crs_x)) crs_x <- ddbs_crs(x_list$query_name, target_conn)
    if (is.null(crs_y)) {
        ## try to get from `y`. if it fails, it's not sf, nor duckspatial_df
        ## therefore, it might be a CRS or character string with CRS
        try(crs_y <- ddbs_crs(y_list$query_name, target_conn), silent = TRUE)

        if (is.null(crs_y)) crs_y <- sf::st_crs(y)
    }

    ## warn if the crs is the same
    if (crs_x$input == crs_y$input) cli::cli_warn("The CRS of `x` and `y` is the same.")


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = FALSE)

    ## remove CRS column from x_rest
    x_rest <- setdiff(x_rest, crs_column)
    
    x_rest <- if (length(x_rest) > 0) {
        x_rest <- paste0('"', x_rest, '"', collapse = ', ')
        x_rest <- paste0(x_rest, ", ")
    } else {
        ""
    }

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT 
                {x_rest}
                '{crs_y$input}' AS '{crs_column}',
                ST_Transform({x_geom}, '{crs_x$input}', '{crs_y$input}') as {x_geom} 
            FROM 
                {x_list$query_name};
        ")
        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    ## always_xy assumes [northing, easting]
    tmp.query <- glue::glue("
        SELECT 
            {x_rest}
            '{crs_y$input}' AS '{crs_column}',
            ST_AsWKB(ST_Transform({x_geom}, '{crs_x$input}', '{crs_y$input}', always_xy := true)) as {x_geom} 
        FROM 
            {x_list$query_name};
    ")
    ## 4.2. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

    ## 5. convert to SF and return result
    data_sf <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = crs_y,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}





#' Returns the geometry type of input features
#'
#' Returns the geometry type(s) from a `sf` object or a DuckDB table. 
#'
#' @template x
#' @template conn_null
#' @param by_feature Boolean. If `TRUE` (default), returns the geometry type for 
#'        each feature. If `FALSE`, returns a single geometry type summary for 
#'        the entire dataset.
#' @template quiet
#'
#' @returns A factor with geometry type(s)
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' # option 1: passing sf objects
#' # Get geometry type for each feature
#' ddbs_geometry_type(countries_ddbs)
#' 
#' # Get overall geometry type
#' ddbs_geometry_type(countries_ddbs, by_feature = FALSE)
#' }
ddbs_geometry_type <- function(
  x,
  conn = NULL,
  by_feature = TRUE,
  quiet = FALSE) {

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)
  assert_logic(by_feature, "by_feature")
  

  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (geometry column name)
  ## this step should be before normalize_spatial_input()
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

  ## Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)


  # 4. create the base query
  if (isTRUE(by_feature)) {
    tmp.query <- glue::glue("
          SELECT ST_GeometryType({x_geom}) as geometry
          FROM {x_list$query_name};
      ")
  } else {
    tmp.query <- glue::glue("
          SELECT DISTINCT ST_GeometryType({x_geom}) as geometry
          FROM {x_list$query_name};
      ")
  }
  
  ## send the query, and return a factor vector
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)
  feedback_query(quiet)
  return(data_tbl$geometry)
}
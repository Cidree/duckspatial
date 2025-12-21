




#' Rotate geometries around centroid
#'
#' Rotates geometries from from a `sf` object or a DuckDB table. Returns the
#' result as an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param angle a numeric value specifying the rotation angle
#' @param units character string specifying angle units: "degrees" (default) or "radians"
#' @template by_feature
#' @param center_x numeric value for the X coordinate of rotation center. If NULL,
#' rotates around the centroid of each geometry
#' @param center_y numeric value for the Y coordinate of rotation center. If NULL,
#' rotates around the centroid of each geometry
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
#' ## rotate 45 degrees
#' ddbs_rotate(conn = conn, "argentina", angle = 45)
#'
#' ## rotate 90 degrees around a specific point
#' ddbs_rotate(conn = conn, "argentina", angle = 90, center_x = -64, center_y = -34)
#'
#' ## rotate without using a connection
#' ddbs_rotate(argentina_sf, angle = 45)
#' }
ddbs_rotate <- function(
    x,
    angle,
    units = c("degrees", "radians"),
    by_feature = FALSE,
    center_x = NULL,
    center_y = NULL,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_numeric(angle, "angle")
    units <- match.arg(units)
    assert_logic(by_feature, "by_feature")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_conn_character(conn, x)

    ## validate center coordinates
    if (!is.null(center_x) && !is.numeric(center_x)) {
        cli::cli_abort("center_x must be numeric", call. = FALSE)
    }
    if (!is.null(center_y) && !is.numeric(center_y)) {
        cli::cli_abort("center_y must be numeric", call. = FALSE)
    }
    if ((!is.null(center_x) && is.null(center_y)) ||
        (is.null(center_x) && !is.null(center_y))) {
        cli::cli_abort("Both center_x and center_y must be provided together or both NULL", call. = FALSE)
    }

    ## validate by_feature and center interaction
    if (!is.null(center_x) && !by_feature) {
        cli::cli_abort("center_x and center_y cannot be used when by_feature = FALSE", call. = FALSE)
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
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 2.1. Convert angle to radians if needed
    if (units == "degrees") {
        angle_rad <- angle * pi / 180
    } else {
        angle_rad <- angle
    }

    ## 2.2. Calculate rotation matrix parameters
    cos_angle <- cos(angle_rad)
    sin_angle <- sin(angle_rad)

    ## 2.3. Build rotation query
    if (by_feature) {
        # Rotate each feature around its own centroid or specified center
        if (is.null(center_x)) {
            # Rotate around each geometry's centroid
            rotation_expr <- glue::glue(
                "ST_Affine(
                    ST_Translate({x_geom}, -ST_X(ST_Centroid({x_geom})), -ST_Y(ST_Centroid({x_geom}))),
                    {cos_angle}, {-sin_angle}, {sin_angle}, {cos_angle},
                    ST_X(ST_Centroid({x_geom})), ST_Y(ST_Centroid({x_geom}))
                )"
            )
        } else {
            # Rotate around specified center point
            rotation_expr <- glue::glue(
                "ST_Affine(
                    ST_Translate({x_geom}, {-center_x}, {-center_y}),
                    {cos_angle}, {-sin_angle}, {sin_angle}, {cos_angle},
                    {center_x}, {center_y}
                )"
            )
        }
    } else {
        # Rotate all features together around the dataset's overall centroid
        rotation_expr <- glue::glue(
            "ST_Affine(
                ST_Translate({x_geom},
                    -(SELECT ST_X(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name}),
                    -(SELECT ST_Y(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name})),
                {cos_angle}, {-sin_angle}, {sin_angle}, {cos_angle},
                (SELECT ST_X(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name}),
                (SELECT ST_Y(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name})
            )"
        )
    }

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT {rotation_expr} as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {x_rest}, {rotation_expr} as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute rotation query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText({rotation_expr}) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {x_rest}, ST_AsText({rotation_expr}) as {x_geom} FROM {x_list$query_name};
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





#' Rotate 3D geometries around an axis
#'
#' Rotates 3D geometries from from a `sf` object or a DuckDB table around the X,
#' Y, or Z axis. Returns the result as an \code{sf} object or creates a new table
#' in the database.
#'
#' @template x
#' @param angle a numeric value specifying the rotation angle
#' @param units character string specifying angle units: "degrees" (default) or "radians"
#' @param axis character string specifying the rotation axis: "x", "y", or "z" (default = "x").
#' The geometry rotates around this axis
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
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read 3D data
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#'
#' ## rotate 45 degrees around X axis (pitch)
#' ddbs_rotate_3d(conn = conn, "countries", angle = 45, axis = "x")
#'
#' ## rotate 90 degrees around Y axis (yaw)
#' ddbs_rotate_3d(conn = conn, "countries", angle = 30, axis = "y")
#'
#' ## rotate 180 degrees around Z axis (roll)
#' ddbs_rotate_3d(conn = conn, "countries", angle = 180, axis = "z")
#'
#' ## rotate without using a connection
#' ddbs_rotate_3d(countries_sf, angle = 45, axis = "z")
#' }
ddbs_rotate_3d <- function(
    x,
    angle,
    units = c("degrees", "radians"),
    axis = "x",
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_numeric(angle, "angle")
    units <- match.arg(units)
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
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 2.1. Convert angle to radians if needed
    if (units == "degrees") {
        angle_rad <- angle * pi / 180
    } else {
        angle_rad <- angle
    }

    ## 2.2. Build rotation expression
    rotation_expr <- glue::glue("ST_Rotate{axis}({x_geom}, {angle_rad})")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT {rotation_expr} as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {x_rest}, {rotation_expr} as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute rotation query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText({rotation_expr}) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {x_rest}, ST_AsText({rotation_expr}) as {x_geom} FROM {x_list$query_name};
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






#' Shift geometries by X and Y offsets
#'
#' Shifts (translates) geometries from a `sf` object or a DuckDB table. Returns
#' the result as an \code{sf} object or creates a new  table in the database.
#' This function is equivalent to \code{terra::shift()}.
#'
#' @template x
#' @param dx numeric value specifying the shift in the X direction (longitude/easting)
#' @param dy numeric value specifying the shift in the Y direction (latitude/northing)
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
#' ## shift 10 degrees east and 5 degrees north
#' ddbs_shift(conn = conn, "argentina", dx = 10, dy = 5)
#'
#' ## shift without using a connection
#' ddbs_shift(argentina_sf, dx = 10, dy = 5)
#' }
ddbs_shift <- function(
    x,
    dx = 0,
    dy = 0,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_numeric(dx, "dx")
    assert_numeric(dy, "dy")
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
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 2.1. Build shift expression using ST_Affine
    # Identity matrix (no rotation/scaling) with translation offsets
    shift_expr <- glue::glue("ST_Affine({x_geom}, 1, 0, 0, 1, {dx}, {dy})")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT {shift_expr} as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {x_rest}, {shift_expr} as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute shift query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText({shift_expr}) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {x_rest}, ST_AsText({shift_expr}) as {x_geom} FROM {x_list$query_name};
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





#' Flip geometries horizontally or vertically
#'
#' Flips (reflects) geometries around the centroid. Returns the result as an
#' \code{sf} object or creates a new table in the database. This function is
#' equivalent to \code{terra::flip()}.
#'
#' @template x
#' @param direction character string specifying the flip direction: "horizontal" (default)
#' or "vertical". Horizontal flips across the Y-axis (left-right), vertical flips across
#' the X-axis (top-bottom)
#' @template by_feature
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
#' ## flip all features together as a whole (default)
#' ddbs_flip(conn = conn, "argentina", direction = "horizontal", by_feature = FALSE)
#'
#' ## flip each feature independently
#' ddbs_flip(conn = conn, "argentina", direction = "horizontal", by_feature = TRUE)
#'
#' ## flip without using a connection
#' ddbs_flip(argentina_sf, direction = "horizontal")
#' }
ddbs_flip <- function(
    x,
    direction = c("horizontal", "vertical"),
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
    direction <- match.arg(direction)
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
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 2.1. Build flip expression using ST_Affine
    if (by_feature) {
        # Flip each feature around its own centroid
        if (direction == "horizontal") {
            # Flip left-right around each feature's centroid X
            flip_expr <- glue::glue(
                "ST_Affine(
                    ST_Translate({x_geom}, -ST_X(ST_Centroid({x_geom})), 0),
                    -1, 0, 0, 1,
                    ST_X(ST_Centroid({x_geom})), 0
                )"
            )
        } else {
            # Flip top-bottom around each feature's centroid Y
            flip_expr <- glue::glue(
                "ST_Affine(
                    ST_Translate({x_geom}, 0, -ST_Y(ST_Centroid({x_geom}))),
                    1, 0, 0, -1,
                    0, ST_Y(ST_Centroid({x_geom}))
                )"
            )
        }
    } else {
        # Flip all features together around the dataset's overall centroid
        # Need to calculate the centroid of all geometries combined
        if (direction == "horizontal") {
            # Flip left-right around overall centroid X
            flip_expr <- glue::glue(
                "ST_Affine(
                    ST_Translate({x_geom},
                        -(SELECT ST_X(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name}),
                        0),
                    -1, 0, 0, 1,
                    (SELECT ST_X(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name}),
                    0
                )"
            )
        } else {
            # Flip top-bottom around overall centroid Y
            flip_expr <- glue::glue(
                "ST_Affine(
                    ST_Translate({x_geom},
                        0,
                        -(SELECT ST_Y(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name})),
                    1, 0, 0, -1,
                    0,
                    (SELECT ST_Y(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name})
                )"
            )
        }
    }

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT {flip_expr} as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {x_rest}, {flip_expr} as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute flip query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText({flip_expr}) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {x_rest}, ST_AsText({flip_expr}) as {x_geom} FROM {x_list$query_name};
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





#' Scale geometries by X and Y factors
#'
#' Scales geometries around the centroid of the geometry. Returns the result as
#' an \code{sf} object or creates a new table in the database.
#'
#' @template x
#' @param x_scale numeric value specifying the scaling factor in the X direction (default = 1)
#' @param y_scale numeric value specifying the scaling factor in the Y direction (default = 1)
#' @template by_feature
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
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#'
#' ## scale to 150% in both directions
#' ddbs_scale(conn = conn, "countries", x_scale = 1.5, y_scale = 1.5)
#'
#' ## scale to 200% horizontally, 50% vertically
#' ddbs_scale(conn = conn, "countries", x_scale = 2, y_scale = 0.5)
#'
#' ## scale all features together (default)
#' ddbs_scale(countries_sf, x_scale = 1.5, y_scale = 1.5, by_feature = FALSE)
#'
#' ## scale each feature independently
#' ddbs_scale(countries_sf, x_scale = 1.5, y_scale = 1.5, by_feature = TRUE)
#'
#' }
ddbs_scale <- function(
    x,
    x_scale = 1,
    y_scale = 1,
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
    assert_numeric(x_scale, "x_scale")
    assert_numeric(y_scale, "y_scale")
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
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 2.1. Build scale expression using ST_Scale
    if (by_feature) {
        # Scale each feature around its own centroid
        # ST_Scale scales around origin (0,0), so translate to origin, scale, translate back
        scale_expr <- glue::glue(
            "ST_Translate(
                ST_Scale(
                    ST_Translate({x_geom}, -ST_X(ST_Centroid({x_geom})), -ST_Y(ST_Centroid({x_geom}))),
                    {x_scale}, {y_scale}
                ),
                ST_X(ST_Centroid({x_geom})), ST_Y(ST_Centroid({x_geom}))
            )"
        )
    } else {
        # Scale all features together around the dataset's overall centroid
        scale_expr <- glue::glue(
            "ST_Translate(
                ST_Scale(
                    ST_Translate({x_geom},
                        -(SELECT ST_X(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name}),
                        -(SELECT ST_Y(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name})),
                    {x_scale}, {y_scale}
                ),
                (SELECT ST_X(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name}),
                (SELECT ST_Y(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name})
            )"
        )
    }

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT {scale_expr} as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {x_rest}, {scale_expr} as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute scale query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText({scale_expr}) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {x_rest}, ST_AsText({scale_expr}) as {x_geom} FROM {x_list$query_name};
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





#' Shear geometries
#'
#' Applies a shear transformation to geometries from a `sf` object or a DuckDB
#' table. Returns the result as an \code{sf} object or creates a new table in the
#' database. Shearing skews the geometry by shifting coordinates proportionally.
#'
#' @template x
#' @param x_shear numeric value specifying the shear factor in the X direction (default = 0).
#' For each unit in Y, X coordinates are shifted by this amount
#' @param y_shear numeric value specifying the shear factor in the Y direction (default = 0).
#' For each unit in X, Y coordinates are shifted by this amount
#' @template by_feature
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
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#'
#' ## shear in X direction (creates italic-like effect)
#' ddbs_shear(conn = conn, "countries", x_shear = 0.3, y_shear = 0)
#'
#' ## shear in Y direction
#' ddbs_shear(conn = conn, "countries", x_shear = 0, y_shear = 0.3)
#'
#' ## shear in both directions
#' ddbs_shear(conn = conn, "countries", x_shear = 0.2, y_shear = 0.2)
#'
#' ## shear without using a connection
#' ddbs_shear(countries_sf, x_shear = 0.3, y_shear = 0)
#' }
ddbs_shear <- function(
    x,
    x_shear = 0,
    y_shear = 0,
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
    assert_numeric(x_shear, "x_shear")
    assert_numeric(y_shear, "y_shear")
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
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
    assert_geometry_column(x_geom, x_list)

    ## 2.1. Build shear expression using ST_Affine
    # Shear matrix: a=1, b=x_shear, d=y_shear, e=1
    if (by_feature) {
        # Shear each feature around its own centroid
        shear_expr <- glue::glue(
            "ST_Affine(
                ST_Translate({x_geom}, -ST_X(ST_Centroid({x_geom})), -ST_Y(ST_Centroid({x_geom}))),
                1, {x_shear}, {y_shear}, 1,
                ST_X(ST_Centroid({x_geom})), ST_Y(ST_Centroid({x_geom}))
            )"
        )
    } else {
        # Shear all features together around the dataset's overall centroid
        shear_expr <- glue::glue(
            "ST_Affine(
                ST_Translate({x_geom},
                    -(SELECT ST_X(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name}),
                    -(SELECT ST_Y(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name})),
                1, {x_shear}, {y_shear}, 1,
                (SELECT ST_X(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name}),
                (SELECT ST_Y(ST_Centroid(ST_Union_Agg({x_geom}))) FROM {x_list$query_name})
            )"
        )
    }

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, conn, quiet, overwrite)

        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT {shear_expr} as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {x_rest}, {shear_expr} as {x_geom} FROM {x_list$query_name};
        ")
        }
        ## execute shear query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText({shear_expr}) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {x_rest}, ST_AsText({shear_expr}) as {x_geom} FROM {x_list$query_name};
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







#' Rotate geometries around their centroid
#'
#' Rotates geometries by a specified angle around their centroid (or another center), 
#' preserving their shape.
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
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#'
#' ## store in duckdb
#' ddbs_write_table(conn, argentina_ddbs, "argentina")
#'
#' ## rotate 45 degrees
#' ddbs_rotate(conn = conn, "argentina", angle = 45)
#'
#' ## rotate 90 degrees around a specific point
#' ddbs_rotate(conn = conn, "argentina", angle = 90, center_x = -64, center_y = -34)
#'
#' ## rotate without using a connection
#' ddbs_rotate(argentina_ddbs, angle = 45)
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
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_numeric(angle, "angle")
    units <- match.arg(units)
    assert_logic(by_feature, "by_feature")
    assert_name(name)
    assert_conn_character(conn, x)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

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
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)


    ## 3.3. Convert angle to radians if needed
    if (units == "degrees") {
        angle_rad <- angle * pi / 180
    } else {
        angle_rad <- angle
    }

    ## 3.4. Calculate rotation matrix parameters
    cos_angle <- cos(angle_rad)
    sin_angle <- sin(angle_rad)

    ## 3.5. Build rotation query
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

    ## 3.6. Build base query
    base.query <- glue::glue("
      SELECT {x_rest}
      {build_geom_query(rotation_expr, mode)} as {x_geom}
      FROM {x_list$query_name};
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
        ## execute rotation query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Apply geospatial operation
  
    
  
    ## 5.1. Create the query based on output
    result <- ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    return(result)
}





#' Rotate 3D geometries around an axis
#'
#' Rotates 3D geometries by a specified angle around the X, Y, or Z axis, 
#' preserving their shape.
#'
#' @template x
#' @param angle a numeric value specifying the rotation angle
#' @param units character string specifying angle units: "degrees" (default) or "radians"
#' @param axis character string specifying the rotation axis: "x", "y", or "z" (default = "x").
#' The geometry rotates around this axis
#' @template conn_null
#' @template name
#' @template crs
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
#' library(dplyr)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read 3D data
#' countries_ddbs <- ddbs_open_dataset(
#'  system.file("spatial/countries.geojson", 
#'  package = "duckspatial")
#' ) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' ## store in duckdb
#' ddbs_write_table(conn, countries_ddbs, "countries")
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
#' ddbs_rotate_3d(countries_ddbs, angle = 45, axis = "z")
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
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

  
    # 0. Handle function-specific errors
    assert_numeric(angle, "angle")
    units <- match.arg(units)
    assert_name(units, "units")
    assert_name(axis, "axis")
  
  
    # 1. Build ST_Rotate parameters string
    #  Convert angle to radians if needed
  
    if (units == "degrees") {
        rotate_3d_args <- angle * pi / 180
    } else {
        rotate_3d_args <- angle
    }
  
    # 2. Pass to template
    template_unary_ops(
      x = x,
      conn = conn,
      name = name,
      crs = crs,
      crs_column = crs_column,
      mode = mode,
      overwrite = overwrite,
      quiet = quiet,
      fun = glue::glue("ST_Rotate{axis}"),
      other_args = rotate_3d_args
    )

}






#' Shift geometries by X and Y offsets
#'
#' Translates geometries by specified X and Y distances, moving 
#' them without altering their shape or orientation.
#'
#' @template x
#' @param dx numeric value specifying the shift in the X direction (longitude/easting)
#' @param dy numeric value specifying the shift in the Y direction (latitude/northing)
#' @template conn_null
#' @template name
#' @template crs
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
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_table(conn, argentina_ddbs, "argentina")
#'
#' ## shift 10 degrees east and 5 degrees north
#' ddbs_shift(conn = conn, "argentina", dx = 10, dy = 5)
#'
#' ## shift without using a connection
#' ddbs_shift(argentina_ddbs, dx = 10, dy = 5)
#' }
ddbs_shift <- function(
    x,
    dx = 0,
    dy = 0,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    

    # 0. Handle function-specific errors
    assert_numeric(dx, "dx")
    assert_numeric(dy, "dy")
  
  
    # 1. Build shift expression using ST_Affine
    # Identity matrix (no rotation/scaling) with translation offsets
    shift_args <- glue::glue("1, 0, 0, 1, {dx}, {dy}")
  
  
    # 2. Pass to template
    template_unary_ops(
      x = x,
      conn = conn,
      name = name,
      crs = crs,
      crs_column = crs_column,
      mode = mode,
      overwrite = overwrite,
      quiet = quiet,
      fun = "ST_Affine",
      other_args = shift_args
    )

  
}





#' Flip geometries horizontally or vertically
#'
#' Reflects geometries across their centroid. By default, flipping is applied 
#' relative to the centroid of all geometries; if `by_feature = TRUE`, each 
#' geometry is flipped relative to its own centroid.
#'
#' @template x
#' @param direction character string specifying the flip direction: "horizontal" (default)
#' or "vertical". Horizontal flips across the Y-axis (left-right), vertical flips across
#' the X-axis (top-bottom)
#' @template by_feature
#' @template conn_null
#' @template name
#' @template crs
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
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_table(conn, argentina_ddbs, "argentina")
#'
#' ## flip all features together as a whole (default)
#' ddbs_flip(conn = conn, "argentina", direction = "horizontal", by_feature = FALSE)
#'
#' ## flip each feature independently
#' ddbs_flip(conn = conn, "argentina", direction = "horizontal", by_feature = TRUE)
#'
#' ## flip without using a connection
#' ddbs_flip(argentina_ddbs, direction = "horizontal")
#' }
ddbs_flip <- function(
    x,
    direction = c("horizontal", "vertical"),
    by_feature = FALSE,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    direction <- match.arg(direction)
    assert_name(direction, "direction")
    assert_logic(by_feature, "by_feature")
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(name)
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
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

    ## 3.3. Build flip expression using ST_Affine
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

    ## 3.4. Build base query
    base.query <- glue::glue("
      SELECT {x_rest}
      {build_geom_query(flip_expr, mode)} as {x_geom}
      FROM {x_list$query_name};
    ")
  

    # 4. if name is not NULL
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
        ## execute flip query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }


    # 5. Apply geospatial operation
    result <- ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    return(result)
}





#' Scale geometries by X and Y factors
#'
#' Resizes geometries by specified X and Y scale factors. By default, scaling is 
#' performed relative to the centroid of all geometries; if `by_feature = TRUE`, 
#' each geometry is scaled relative to its own centroid.
#'
#' @template x
#' @param x_scale numeric value specifying the scaling factor in the X direction (default = 1)
#' @param y_scale numeric value specifying the scaling factor in the Y direction (default = 1)
#' @template by_feature
#' @template conn_null
#' @template name
#' @template crs
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
#' library(dplyr)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' ) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' ## store in duckdb
#' ddbs_write_table(conn, countries_ddbs, "countries")
#'
#' ## scale to 150% in both directions
#' ddbs_scale(conn = conn, "countries", x_scale = 1.5, y_scale = 1.5)
#'
#' ## scale to 200% horizontally, 50% vertically
#' ddbs_scale(conn = conn, "countries", x_scale = 2, y_scale = 0.5)
#'
#' ## scale all features together (default)
#' ddbs_scale(countries_ddbs, x_scale = 1.5, y_scale = 1.5, by_feature = FALSE)
#'
#' ## scale each feature independently
#' ddbs_scale(countries_ddbs, x_scale = 1.5, y_scale = 1.5, by_feature = TRUE)
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
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_numeric(x_scale, "x_scale")
    assert_numeric(y_scale, "y_scale")
    assert_logic(by_feature, "by_feature")
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(name)
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
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

    ## 3.3. Build scale expression using ST_Scale
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

    ## 3.4. Build base query
    base.query <- glue::glue("
      SELECT {x_rest}
      {build_geom_query(scale_expr, mode)} as {x_geom}
      FROM {x_list$query_name};
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
        ## execute scale query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Apply geospatial operation
    result <- ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    return(result)
}





#' Shear geometries
#'
#' Applies a shear transformation to geometries, shifting coordinates proportionally 
#' in the X and Y directions. By default, shearing is applied relative to the centroid 
#' of all geometries; if `by_feature = TRUE`, each geometry is sheared relative to its 
#' own centroid.
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
#' library(dplyr)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' ) |>
#'   filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
#'
#' ## store in duckdb
#' ddbs_write_table(conn, countries_ddbs, "countries")
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
#' ddbs_shear(countries_ddbs, x_shear = 0.3, y_shear = 0)
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
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    # 0. Handle errors
    assert_xy(x, "x")
    assert_numeric(x_shear, "x_shear")
    assert_numeric(y_shear, "y_shear")
    assert_logic(by_feature, "by_feature")
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(name)
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
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

    ## 3.3. Build shear expression using ST_Affine
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

    ## 3.4. Build base query
    base.query <- glue::glue("
      SELECT {x_rest}
      {build_geom_query(shear_expr, mode)} as {x_geom}
      FROM {x_list$query_name};
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
        ## execute shear query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    
    # 5. Apply geospatial operation
    result <- ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    return(result)
}


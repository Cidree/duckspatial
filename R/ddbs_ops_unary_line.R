
## Line operations:
## - ddbs_line_merge
## - ddbs_line_interpolate
## - ddbs_line_locate_point
## - ddbs_line_substring
## - ddbs_endpoint
## - ddbs_startpoint
## - ddbs_build_area
## - ddbs_polygonize
## - ddbs_make_polygon



#' Extract the start point of a linestring geometry
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `ddbs_startpoint()` was renamed to \code{\link{ddbs_line_startpoint}}.
#'
#' @inheritParams ddbs_endpoint_startpoint
#' @template returns_mode
#' @export
#' @keywords internal
ddbs_startpoint <- function(
  x,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  lifecycle::deprecate_soft(
    when    = "1.1.0",
    what    = "ddbs_startpoint()",
    with    = "ddbs_line_startpoint()"
  )

  ddbs_line_startpoint(
    x = x,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet
  )

}




#' Extract the end point of a linestring geometry
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `ddbs_endpoint()` was renamed to \code{\link{ddbs_line_endpoint}}.
#'
#' @inheritParams ddbs_endpoint_startpoint
#' @template returns_mode
#' @export
#' @keywords internal
ddbs_endpoint <- function(
  x,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  lifecycle::deprecate_soft(
    when    = "1.1.0",
    what    = "ddbs_endpoint()",
    with    = "ddbs_line_endpoint()"
  )

  ddbs_line_endpoint(
    x = x,
    conn = conn,
    name = name,
    mode = mode,
    overwrite = overwrite,
    quiet = quiet
  )

}


#' Extract the start or end point of a linestring geometry
#'
#' Returns the first or last point of a LINESTRING geometry. These functions only work
#' with LINESTRING geometries (not MULTILINESTRING or other geometry types).
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#'
#' @details
#' These functions wrap DuckDB Spatial's \code{ST_StartPoint} and \code{ST_EndPoint}.
#' Input geometries must be of type LINESTRING (MULTILINESTRING is not supported).
#' For each input feature, the first or last coordinate of the LINESTRING is returned
#' as a POINT geometry.
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, rivers_ddbs, "rivers")
#'
#' ## extract start points
#' ddbs_line_startpoint(conn = conn, "rivers")
#'
#' ## extract end points
#' ddbs_line_endpoint(conn = conn, "rivers")
#'
#' ## without using a connection
#' ddbs_line_startpoint(rivers_ddbs)
#' ddbs_line_endpoint(rivers_ddbs)
#' }
#' @name ddbs_endpoint_startpoint
#' @rdname ddbs_endpoint_startpoint
NULL



#' @rdname ddbs_endpoint_startpoint
#' @export
ddbs_line_startpoint <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Handle function-specific error
    assert_geom_type(x = x, conn = conn, geom = "LINESTRING", multi = FALSE)

    # 1. Run the template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_StartPoint",
        other_args = NULL
    )

}

#' @rdname ddbs_endpoint_startpoint
#' @export
ddbs_line_endpoint <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Handle function-specific error
    assert_geom_type(x = x, conn = conn, geom = "LINESTRING", multi = FALSE)

    # 1. Run the template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_EndPoint",
        other_args = NULL
    )

}



#' Interpolates a point or points along a line geometry
#'
#' Returns either a single point at a specified position along a line, or
#' multiple equally-spaced points along a line, depending on the value of
#' \code{intervals}. When \code{intervals = FALSE}, this wraps
#' \code{ST_LineInterpolatePoint}; when \code{intervals = TRUE}, it wraps
#' \code{ST_LineInterpolatePoints}.
#'
#' @template x
#' @param fraction a numeric value between 0 and 1. When
#' \code{intervals = FALSE}, specifies the position along the line to
#' interpolate, where \code{0} is the start and \code{1} is the end.
#' When \code{intervals = TRUE}, specifies the spacing between interpolated
#' points as a proportion of the total line length. Defaults to \code{0.5}.
#' @param intervals a logical value. If \code{FALSE} (default), returns a
#' single \code{POINT} at the position given by \code{fraction}. If
#' \code{TRUE}, returns a \code{MULTIPOINT} of equally-spaced points along
#' the line at intervals defined by \code{fraction}.
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
#' ## load package
#' library(duckspatial)
#'
#' ## read data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## return the midpoint of a line (default)
#' ddbs_line_interpolate(rivers_ddbs)
#'
#' ## return the point 25% along the line
#' ddbs_line_interpolate(rivers_ddbs, fraction = 0.25)
#'
#' ## return equally-spaced points every 10% of the line length
#' ddbs_line_interpolate(rivers_ddbs, fraction = 0.1, intervals = TRUE)
#'
#' ## return equally-spaced points every 50% of the line length (i.e. midpoint and end)
#' ddbs_line_interpolate(rivers_ddbs, fraction = 0.5, intervals = TRUE)
#' }
ddbs_line_interpolate <- function(
    x,
    fraction = 0.5,
    intervals = FALSE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
  
    # 0. Handle function-specific errors
    assert_numeric(fraction, "fraction")
    assert_numeric_interval(fraction, 0, 1)
    assert_logic(intervals, "intervals")
  
    # 1. Build ST_Buffer parameters string
    other_args <- glue::glue("{fraction}, {intervals}")
  
    # 2. Pass to template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_LineInterpolatePoints",
        other_args = other_args
    )

}





#' Extract a substring of a line geometry
#'
#' Returns the portion of a line between two fractional positions along its
#' length
#'
#' @template x
#' @param start a numeric value between 0 and 1. Specifies the starting
#' position along the line as a proportion of its total length, where
#' \code{0} is the beginning of the line. Defaults to \code{0}.
#' @param end a numeric value between 0 and 1. Specifies the ending
#' position along the line as a proportion of its total length, where
#' \code{1} is the end of the line. Must be greater than or equal to
#' \code{start}. Defaults to \code{0.5}.
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
#' ## load package
#' library(duckspatial)
#'
#' ## read data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## return the first half of each line (default)
#' ddbs_line_substring(rivers_ddbs)
#'
#' ## return the middle third of each line
#' ddbs_line_substring(rivers_ddbs, start = 0.33, end = 0.67)
#'
#' ## return the last quarter of each line
#' ddbs_line_substring(rivers_ddbs, start = 0.75, end = 1)
#' }
ddbs_line_substring <- function(
    x,
    start = 0,
    end = 0.5,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
  
    # 0. Handle function-specific errors
    assert_numeric(start, "start")
    assert_numeric(end, "end")
    assert_numeric_interval(start, 0, 1)
    assert_numeric_interval(end, 0, 1)
    if (start > end) {
        cli::cli_abort("{.arg start} must be less than or equal to {.arg end}.")
    }
  
    # 1. Build ST_Buffer parameters string
    other_args <- glue::glue("{start}, {end}")
  
    # 2. Pass to template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_LineSubstring",
        other_args = other_args
    )

}





#' Merge line geometries into a single line
#'
#' Merges a collection of line geometries that share endpoints into a single
#' \code{LINESTRING}, or \code{MULTILINESTRING} if endpoints are not shared
#'
#' @template x
#' @param preserve a logical value. If \code{TRUE} (default), line direction
#' is preserved
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
#' ## load package
#' library(duckspatial)
#' library(dplyr)
#'
#' ## read data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson",
#'   package = "duckspatial")
#' )
#' 
#' ## first, union by river name
#' rivers_union <- ddbs_union_agg(rivers_ddbs, by = "RIVER_NAME")
#'
#' ## merge lines, preserving direction
#' rivers_merged <- ddbs_line_merge(rivers_union)
#' 
#' ## check Rio Eume (union doesn't guarantee the merging)
#' rivers_union |> filter(RIVER_NAME == "Rio Eume")
#' rivers_merged |> filter(RIVER_NAME == "Rio Eume")
#' 
#' }
ddbs_line_merge <- function(
    x,
    preserve = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
  
    # 0. Handle function-specific errors
    assert_logic(preserve, "preserve")
  
    # 1. Build ST_Buffer parameters string
    other_args <- glue::glue("{preserve}")
  
    # 2. Pass to template
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_LineMerge",
        other_args = other_args
    )

}




#' Create a polygon from a single closed linestring
#'
#' Converts a single closed linestring geometry into a polygon. The linestring 
#' must be closed (first and last points identical). Does not work with 
#' MULTILINESTRING inputs - use [ddbs_polygonize()] or [ddbs_build_area()] instead.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @family polygon construction
#' @seealso [ddbs_polygonize()], [ddbs_build_area()]
#' @export
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
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    # 0. Handle function-specific error
    assert_geom_type(x = x, conn = conn, geom = "LINESTRING", multi = FALSE)
    
    template_unary_ops(
        x = x,
        conn = conn,
        name = name,
        mode = mode,
        overwrite = overwrite,
        quiet = quiet,
        fun = "ST_MakePolygon",
        other_args = NULL
    )
}








#' Assemble polygons from multiple linestrings
#'
#' Takes a collection of linestrings or polygons and assembles them into polygons by 
#' finding all closed rings formed by the network. Returns a GEOMETRYCOLLECTION containing 
#' the resulting polygons.
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @family polygon construction
#' @seealso [ddbs_make_polygon()], [ddbs_build_area()]
#' @export
ddbs_polygonize <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Validate inputs
    assert_xy(x, "x")
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

  
    # 1. Prepare inputs
  
    ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names)
    x <- normalize_spatial_input(x, conn)

    ## 1.2. Pre-extract attributes
    crs_x    <- ddbs_crs(x, conn)
    sf_col_x <- attr(x, "sf_column")
    mode     <- get_mode(mode, name)

    ## 1.3. Resolve spatial connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 1.4. Get list with query names for the input data
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 2. Prepare the query

    ## 2.1. Get the geometry column name (try to extract from attributes, if not 
    ## available get it from the database)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)
  
    ## 2.2.  Build the base query (depends on the output type - sf, duckspatial_df, table)
    st_function <- glue::glue("ST_Polygonize(LIST({x_geom}))")
    base.query <- glue::glue("
      SELECT 
        {build_geom_query(st_function, name, crs_x, mode)} as {x_geom}
      FROM 
        {x_list$query_name};
    ")


    # 3. Table creation if name is provided, or 
    # create duckspatial_df or sf object if name is NULL
    if (!is.null(name)) {
        create_duckdb_table(
        conn      = target_conn,
        name      = name,
        query     = base.query,
        overwrite = overwrite,
        quiet     = quiet
        )
    } else {
        ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = crs_x,
        x_geom     = x_geom
        )
    }
    
}







#' Build polygon areas from multiple linestrings
#'
#' Constructs polygon or multipolygon geometries from a collection of linestrings, 
#' handling intersections and creating unified areas. Returns POLYGON or MULTIPOLYGON 
#' (not wrapped in a geometry collection). Requires MULTILINESTRING input - for 
#' single linestrings, use [ddbs_make_polygon()].
#'
#' @template x
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @family polygon construction
#' @seealso [ddbs_make_polygon()], [ddbs_polygonize()]
#' @export
ddbs_build_area <- function(
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
    fun = "ST_BuildArea",
    other_args = NULL
  )

}




#' Locate a point along a linestring
#'
#' Returns a value between 0 and 1 representing the position of the closest
#' point on the line to the reference point, as a fraction of the line's
#' total length. 0 corresponds to the start of the line and 1 to the end.
#'
#' @template x
#' @param y The reference point. One of:
#'   \itemize{
#'     \item An \code{sf} object with exactly 1 point feature.
#'     \item A \code{duckspatial_df} with exactly 1 point feature.
#'     \item A character string naming a DuckDB table that contains exactly 1
#'       point feature (requires \code{conn}).
#'   }
#' @template new_column
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
#' ## load package
#' library(duckspatial)
#'
#' ## read river data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson", package = "duckspatial")
#' )
#'
#' ## define a single reference point (sf with 1 row)
#' ref_pt <- sf::st_sf(
#'   geometry = sf::st_sfc(sf::st_point(c(-8, 43)), crs = 4326)
#' )
#'
#' ## locate the fraction along each river closest to the reference point
#' ddbs_line_locate_point(rivers_ddbs, y = ref_pt)
#' }
ddbs_line_locate_point <- function(
    x,
    y,
    new_column = "line_fraction",
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Validate inputs

    ## common input checks
    assert_xy(x, "x")
    assert_name(name)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_character_scalar(new_column, "new_column")

    ## y type check
    if (!inherits(y, c("sf", "duckspatial_df", "tbl_duckdb_connection")) && !is.character(y)) {
        cli::cli_abort(
            "{.arg y} must be an {.cls sf} object, a {.cls duckspatial_df}, or a character table name.",
            call = NULL
        )
    }

    ## early row-count check for sf (no DB needed)
    if (inherits(y, "sf") && nrow(y) != 1L) {
        cli::cli_abort(
            "{.arg y} must have exactly 1 feature when passed as an {.cls sf} object, not {nrow(y)}.",
            call = NULL
        )
    }

    ## connection requirements
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x, y)


    # 1. Prepare inputs

    ## 1.1. Resolve conn defaults for character inputs (mirrors binary ops pattern)
    conn_x <- if (is.character(x)) conn else NULL
    conn_y <- if (is.character(y)) conn else NULL

    ## 1.2. Normalize x
    x        <- normalize_spatial_input(x, conn_x)
    crs_x    <- ddbs_crs(x, conn_x)
    sf_col_x <- attr(x, "sf_column")
    mode     <- get_mode(mode, name)


    # 2. Resolve connections and build y_sql based on y type

    if (inherits(y, "sf")) {
        ## y is sf: use an inline WKT literal — no connection resolution for y
        y_wkt <- sf::st_as_text(sf::st_geometry(y)[[1]])

        resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
        target_conn  <- resolve_conn$conn
        x            <- resolve_conn$x
        on.exit(resolve_conn$cleanup(), add = TRUE)

        x_list <- get_query_list(x, target_conn)
        on.exit(x_list$cleanup(), add = TRUE)

        x_geom <- (sf_col_x %||% get_geom_name(target_conn, x_list$query_name))[1]
        assert_geometry_column(x_geom, x_list)

        y_sql <- glue::glue("ST_GeomFromText('{y_wkt}')")

    } else {
        ## y is duckspatial_df or character table: normalize and resolve both
        y        <- normalize_spatial_input(y, conn_y)
        sf_col_y <- attr(y, "sf_column")

        resolve_res <- resolve_spatial_connections(x, y, conn, conn_x, conn_y, quiet = quiet)
        target_conn <- resolve_res$conn
        x           <- resolve_res$x
        y           <- resolve_res$y
        on.exit(resolve_res$cleanup(), add = TRUE)

        x_list <- get_query_list(x, target_conn)
        on.exit(x_list$cleanup(), add = TRUE)

        y_list <- get_query_list(y, target_conn)
        on.exit(y_list$cleanup(), add = TRUE)

        x_geom <- (sf_col_x %||% get_geom_name(target_conn, x_list$query_name))[1]
        y_geom <- (sf_col_y %||% get_geom_name(target_conn, y_list$query_name))[1]
        assert_geometry_column(x_geom, x_list)

        ## validate row count of y (after resolution so the table exists in target_conn)
        y_n <- DBI::dbGetQuery(
            target_conn,
            glue::glue("SELECT COUNT(*) AS n FROM {y_list$query_name}")
        )$n
        if (y_n != 1L) {
            cli::cli_abort(
                "{.arg y} must contain exactly 1 feature, but has {y_n} row{?s}.",
                call = NULL
            )
        }

        y_sql <- glue::glue("(SELECT {y_geom}::GEOMETRY FROM {y_list$query_name})")
    }


    # 3. Build the base query
    if (mode == "sf") {
        base.query <- glue::glue("
            SELECT ST_LineLocatePoint({x_geom}, {y_sql}) AS {new_column},
            FROM {x_list$query_name};
        ")
    } else {
        base.query <- glue::glue("
            SELECT
                * EXCLUDE {x_geom},
                ST_LineLocatePoint({x_geom}, {y_sql}) AS {new_column},
                {build_geom_query(x_geom, name, crs_x, mode)} AS {x_geom}
            FROM
                {x_list$query_name};
        ")
    }


    # 4. Table creation if name is provided, or
    # create duckspatial_df or sf object if name is NULL
    if (!is.null(name)) {
        create_duckdb_table(
            conn      = target_conn,
            name      = name,
            query     = base.query,
            overwrite = overwrite,
            quiet     = quiet
        )
    } else {
        ddbs_handle_query(
            query     = base.query,
            conn      = target_conn,
            mode      = mode,
            crs       = crs_x,
            x_geom    = x_geom,
            fun_group = 2,
            units     = NULL
        )
    }

}


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
#' # create points data
#' n <- 5
#' points_sf <- data.frame(
#'     id = 1,
#'     x = runif(n, min = -180, max = 180),
#'     y = runif(n, min = -90, max = 90)
#'     ) |>
#'     sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
#'     st_geometry() |>
#'     st_combine() |>
#'     st_cast("MULTIPOINT") |>
#'     st_as_sf()
#'
#' # option 1: passing sf objects
#' output1 <- duckspatial::ddbs_concave_hull(x = points_sf)
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
#' ddbs_write_vector(conn, points_sf, "points_tbl")
#'
#' # spatial join
#' output2 <- duckspatial::ddbs_concave_hull(
#'     conn = conn,
#'     x = "points_tbl"
#'     )
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
    overwrite = FALSE,
    quiet = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_numeric_interval(ratio, 0, 1, "ratio")
    assert_logic(allow_holes, "allow_holes")
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
            SELECT ST_ConcaveHull({x_geom}, {ratio}, {allow_holes}) as {x_geom} FROM {x_list$query_name};
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_ConcaveHull({x_geom}, {ratio}, {allow_holes}) as {x_geom} FROM {x_list$query_name};
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
            SELECT ST_AsText(ST_ConcaveHull({x_geom}, {ratio}, {allow_holes})) as {x_geom} FROM {x_list$query_name};
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_AsText(ST_ConcaveHull({x_geom}, {ratio}, {allow_holes})) as {x_geom} FROM {x_list$query_name};
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

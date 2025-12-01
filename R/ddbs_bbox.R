

#' Returns the minimal bounding box enclosing the input geometry
#'
#' Returns the minimal bounding box enclosing the input geometry from a `sf` object
#' or a DuckDB table. Returns the result as an \code{sf} object or creates a new
#' table in the database.
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
                      conn = NULL,
                      name = NULL,
                      crs = NULL,
                      crs_column = "crs_duckspatial",
                      overwrite = FALSE,
                      quiet = FALSE) {

    # 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
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

        ## create query
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Extent({x_geom})
            FROM {x_list$query_name}
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Extent({x_geom})
            FROM {x_list$query_name}
        ")
        }
        ## execute area query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))

        if (isFALSE(quiet)) {
            cli::cli_alert_success("Query successful")
        }

        return(invisible(TRUE))
    }

    # 4. Get data frame
    ## 4.1. create query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_Extent({x_geom})
            FROM {x_list$query_name}
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0(x_rest, collapse = ', ')}, ST_Extent({x_geom})
            FROM {x_list$query_name}
        ")
    }
    ## 4.2. retrieve results of the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    bbox <- data_tbl$`st_extent(geometry)`
    # class(bbox) <- "bbox"

    if (isFALSE(quiet)) cli::cli_alert_success("Query successful")
    return(bbox)
}




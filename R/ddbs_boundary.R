#' Returns the boundary of geometries
#'
#' Returns the boundary of geometries from a DuckDB table using the spatial extension.
#' Returns the result as an \code{sf} object or creates a new table in the database.
#'
#' @template conn
#' @param x a table with a geometry column within the DuckDB database
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
#' ## database setup
#' conn <- dbConnect(duckdb())
#' ddbs_install(conn)
#' ddbs_load(conn)
#'
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## boundary
#' b <- ddbs_boundary(conn, "argentina")
#' }
ddbs_boundary <- function(conn,
                          x,
                          name = NULL,
                          crs = NULL,
                          crs_column = "crs_duckspatial",
                          overwrite = FALSE,
                          quiet = FALSE) {

    ## 1. check conn
    dbConnCheck(conn)

    ## 2. get name of geometry column
    ## get convient names
    x_list <- get_query_name(x)
    ## get name
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    if (length(x_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{x_list$query_name}>.")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        if (overwrite) {
            DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name_list$query_name};"))
            cli::cli_alert_info("Table <{name_list$query_name}> dropped")
        }

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
        cli::cli_alert_success("Query successful")
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

    if (isFALSE(quiet)) cli::cli_alert_success("Query successful")
    return(data_sf)
}

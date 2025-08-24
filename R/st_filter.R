


#' Spatial Filter
#'
#' Filters data spatially based on a spatial predicate
#'
#' @param conn a connection object to a DuckDB database
#' @param x a table with geometry column within the DuckDB database. Data is returned
#' from this object
#' @param y a table with geometry column within the DuckDB database
#' @param name a character string of length one specifying the name of the table,
#' or a character string of length two specifying the schema and table names. If it's
#' NULL (the default), it will return the result as an \code{sf} object
#' @param predicate geometry predicate to use for filtering the data
#' @param crs the coordinates reference system of the data. Specify if the data
#' doesn't have crs_column, and you know the crs
#' @param crs_column a character string of length one specifying the column
#' storing the CRS (created automatically by \code{\link{ddbs_write_vector}}). Set
#' to NULL if absent
#' @param overwrite whether to overwrite the existing table if it exists. Ignored
#' when name is NULL
#' @template quiet
#'
#' @returns an sf object or TRUE (invisibly) for table creation
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
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ## filter countries touching argentina
#' ddbs_filter(conn, "countries", "argentina", predicate = "touches")
#' }
ddbs_filter <- function(conn,
                        x,
                        y,
                        name = NULL,
                        predicate = "intersection",
                        crs = NULL,
                        crs_column = "crs_duckspatial",
                        overwrite = FALSE,
                        quiet = FALSE) {

    ## 1. select predicate
    sel_pred <- switch(predicate,
           "intersection" = "ST_Intersects",
           "touches"      = "ST_Touches",
           "contains"     = "ST_Contains",
           "within"       = "ST_Within",  ## TODO -> add distance argument
           "disjoint"     = "ST_Disjoint",
           "equals"       = "ST_Equals",
           "overlaps"     = "ST_Overlaps",
           "crosses"      = "ST_Crosses",
           "intersects_extent" = "ST_Intersects_Extent",
           cli::cli_abort(
               "Predicate should be one of <intersection>, <touches>, <contains>,
               <within>, <disjoin>, <equals>, <overlaps>, <crosses>, or <intersects_extent>")
    )

    ## 2. get name of geometry column
    ## get convient names for x and y
    x_list <- get_query_name(x)
    y_list <- get_query_name(y)
    ## get name
    x_geom <- get_geom_name(conn, x_list$query_name)
    x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE)
    y_geom <- get_geom_name(conn, y_list$query_name)
    if (length(x_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{x_list$query_name}>.")
    if (length(y_geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{y_list$query_name}>.")

    ## error if crs_column not found
    if (!is.null(crs_column))
        if (!crs_column %in% x_rest)
            cli::cli_abort("CRS column <{crs_column}> do not found in the table. If the data do not have CRS column, set the argument `crs_column = NULL`")

    ## 3. if name is not NULL (i.e. no SF returned)
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

        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, v1.{x_geom} AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
            WHERE {sel_pred}(v2.{y_geom}, v1.{x_geom})
        ")
        ## execute filter query
        DBI::dbExecute(conn, tmp.query)

        if (isFALSE(quiet)) {
            cli::cli_alert_success("Query successful")
        }

        return(invisible(TRUE))
    }

    ## 4. get data frame
    ## send the query
    data_tbl <- DBI::dbGetQuery(
        conn, glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_AsText(v1.{x_geom}) AS {x_geom}
            FROM {x_list$query_name} v1, {y_list$query_name} v2
            WHERE {sel_pred}(v2.{y_geom}, v1.{x_geom})
        ")
    )

    ## 5. convert to SF
    if (is.null(crs)) {
        if (is.null(crs_column)) {
            data_sf <- data_tbl |>
                sf::st_as_sf(wkt = x_geom)
        } else {
            data_sf <- data_tbl |>
                sf::st_as_sf(wkt = x_geom, crs = data_tbl[1, crs_column])
            data_sf <- data_sf[, -which(names(data_sf) == crs_column)]
        }
    } else {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = x_geom, crs = crs)
    }

    if (isFALSE(quiet)) {
        cli::cli_alert_success("Query successful")
    }

    return(data_sf)
}





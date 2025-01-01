
#' Calculates the intersection of two geometries
#'
#' Calculates the intersection of two geometries, and return a \code{sf} object
#'
#' @param conn a connection object to a DuckDB database
#' @param x a table with geometry column within the DuckDB database. Data is returned
#' from this object
#' @param y a table with geometry column within the DuckDB database
#' @param crs the coordinates reference system of the data
#'
#' @returns an sf object
#' @export
#'
#' @examples
#' 1 + 1 ## TODO
#'
ddbs_intersection <- function(conn, x, y, crs = NULL) {
    ## 2. get name of geometry column
    x_geom <- get_geom_name(conn, x)
    x_rest <- get_geom_name(conn, x, rest = TRUE)
    y_geom <- get_geom_name(conn, y)

    ## 3. get data frame
    ## send the query
    res <- DBI::dbSendQuery(
        conn, glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_AsText(ST_Intersection(v1.{x_geom}, v2.{y_geom})) AS {x_geom}
            FROM {x} v1, {y} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
    )
    ## fetch the query
    data_tbl <- DBI::dbFetch(res)

    ## 4. convert to SF
    if (is.null(crs)) {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = "geom")
    } else {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = "geom", crs = crs)
    }
    cli::cli_alert_success("Query successful")
    return(data_sf)
}


#' Spatial Filter
#'
#' Filters data spatially based on a spatial predicate
#'
#' @param conn a connection object to a DuckDB database
#' @param x a table with geometry column within the DuckDB database. Data is returned
#' from this object
#' @param y a table with geometry column within the DuckDB database
#' @param predicate geometry predicate to use for filtering the data
#' @param crs the coordinates reference system of the data
#'
#' @returns an sf object
#' @export
#'
#' @examples
#' 1 + 1 ## TODO
#'
ddbs_filter <- function(conn, x, y, predicate = "intersection", crs = NULL) {
    ## 1. select predicate
    sel_pred <- switch(predicate,
           "intersection" = "ST_Intersects",
           "touches"      = "ST_Touches",
           "contains"     = "ST_Contains",
           "within"       = "ST_Within",
           "disjoint"     = "ST_Disjoint",
           "equals"       = "ST_Equals",
           "overlaps"     = "ST_Overlaps",
           "crosses"      = "ST_Crosses",
           cli::cli_abort("Predicate should be one of <intersection>, <touches>, <contains>, <within>, <disjoin>, <equals>, <overlaps>, or <crosses>")
    )
    ## 2. get name of geometry column
    x_geom <- get_geom_name(conn, x)
    x_rest <- get_geom_name(conn, x, rest = TRUE)
    y_geom <- get_geom_name(conn, y)

    ## 3. get data frame
    ## send the query
    res <- DBI::dbSendQuery(
        conn, glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_AsText(v1.{x_geom}) AS {x_geom}
            FROM {x} v1, {y} v2
            WHERE {sel_pred}(v2.{y_geom}, v1.{x_geom})
        ")
    )
    ## fetch the query
    data_tbl <- DBI::dbFetch(res)

    ## 4. convert to SF
    if (is.null(crs)) {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = "geom")
    } else {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = "geom", crs = crs)
    }
    cli::cli_alert_success("Query successful")
    return(data_sf)
}





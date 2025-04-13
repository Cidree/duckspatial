
#' Calculates the intersection of two geometries
#'
#' Calculates the intersection of two geometries, and return a \code{sf} object
#'
#' @param conn a connection object to a DuckDB database
#' @param x a table with geometry column within the DuckDB database. Data is returned
#' from this object
#' @param y a table with geometry column within the DuckDB database
#' @param name a character string of length one specifying the name of the table,
#' or a character string of length two specifying the schema and table names. If it's
#' NULL (the default), it will return the result as an \code{sf} object
#' @param crs the coordinates reference system of the data. Specify if the data
#' doesn't have crs_column, and you know the crs
#' @param crs_column a character string of length one specifying the column
#' storing the CRS (created automatically by \code{\link{ddbs_write_vector}}). Set
#' to NULL if absent
#' @param overwrite whether to overwrite the existing table if it exists. Ignored
#' when name is NULL
#'
#' @returns an sf object or TRUE (invisibly) for table creation
#' @export
#'
#' @examples
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
#' ## intersection
#' ddbs_intersection(conn, "countries", "argentina")
#'
ddbs_intersection <- function(conn,
                              x,
                              y,
                              name = NULL,
                              crs = NULL,
                              crs_column = "crs_duckspatial",
                              overwrite = NULL) {

    ## 1. check conn
    dbConnCheck(conn)

    ## 2. get name of geometry column
    x_geom <- get_geom_name(conn, x)
    x_rest <- get_geom_name(conn, x, rest = TRUE)
    y_geom <- get_geom_name(conn, y)


    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {
        ## handle overwrite
        if (overwrite) {
            DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name};"))
            cli::cli_alert_info("Table <{name}> dropped")
        }
        ## convenient names of table and/or schema.table
        if (length(name) == 2) {
            table_name <- name[2]
            schema_name <- name[1]
            query_name <- paste0(name, collapse = ".")
        } else {
            table_name   <- name
            schema_name <- "main"
            query_name <- name
        }
        ## create query (no st_as_text)
        if (length(x_rest) == 0) {
            tmp.query <- glue::glue("
            SELECT ST_Intersection(v1.{x_geom}, v2.{y_geom}) AS {x_geom}
            FROM {x} v1, {y} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
        } else {
            tmp.query <- glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_Intersection(v1.{x_geom}, v2.{y_geom}) AS {x_geom}
            FROM {x} v1, {y} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
        }
        ## execute intersection query
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {table_name} AS {tmp.query}"))
        cli::cli_alert_success("Query successful")
        return(invisible(TRUE))
    }

    ## 4. create the base query
    if (length(x_rest) == 0) {
        tmp.query <- glue::glue("
            SELECT ST_AsText(ST_Intersection(v1.{x_geom}, v2.{y_geom})) AS {x_geom}
            FROM {x} v1, {y} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
    } else {
        tmp.query <- glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_AsText(ST_Intersection(v1.{x_geom}, v2.{y_geom})) AS {x_geom}
            FROM {x} v1, {y} v2
            WHERE ST_Intersects(v2.{y_geom}, v1.{x_geom})
        ")
    }
    ## send the query
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

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
#'
#' @returns an sf object or TRUE (invisibly) for table creation
#' @export
#'
#' @examples
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
#'
ddbs_filter <- function(conn,
                        x,
                        y,
                        name = NULL,
                        predicate = "intersection",
                        crs = NULL,
                        crs_column = "crs_duckspatial",
                        overwrite = FALSE) {

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
    ## error if crs_column not found
    if (!is.null(crs_column))
        if (!crs_column %in% x_rest)
            cli::cli_abort("CRS column <{crs_column}> do not found in the table. If the data do not have CRS column, set the argument `crs_column = NULL`")

    ## 3. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {
        ## handle overwrite
        if (overwrite) {
            DBI::dbExecute(conn, glue::glue("DROP TABLE IF EXISTS {name};"))
            cli::cli_alert_info("Table <{name}> dropped")
        }
        ## convenient names of table and/or schema.table
        if (length(name) == 2) {
            table_name  <- name[2]
            schema_name <- name[1]
            query_name  <- paste0(name, collapse = ".")
        } else {
            table_name  <- name
            schema_name <- "main"
            query_name  <- name
        }
        tmp.query <- glue::glue("
            CREATE TABLE {table_name} AS
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, v1.{x_geom} AS {x_geom}
            FROM {x} v1, {y} v2
            WHERE {sel_pred}(v2.{y_geom}, v1.{x_geom})
        ")
        ## execute filter query
        DBI::dbExecute(conn, tmp.query)
        cli::cli_alert_success("Query successful")
        return(invisible(TRUE))
    }

    ## 4. get data frame
    ## send the query
    data_tbl <- DBI::dbGetQuery(
        conn, glue::glue("
            SELECT {paste0('v1.', x_rest, collapse = ', ')}, ST_AsText(v1.{x_geom}) AS {x_geom}
            FROM {x} v1, {y} v2
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
    cli::cli_alert_success("Query successful")
    return(data_sf)
}





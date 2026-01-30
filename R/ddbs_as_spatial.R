
#' Convert a table with coordinates to spatial data
#'
#' Converts a data frame or DuckDB table with coordinate columns into spatial 
#' points. This function replicates the functionality \code{sf::st_as_sf()} for 
#' DuckDB spatial operations.
#'
#' @template x
#' @param coords Character vector of length 2 specifying the names of the 
#'        longitude and latitude columns (or X and Y coordinates). Defaults to 
#'        \code{c("lon", "lat")}.
#' @param crs Character or numeric. The Coordinate Reference System (CRS) of the 
#'        input coordinates. Can be specified as an EPSG code (e.g., \code{"EPSG:4326"} 
#'        or \code{4326}) or a WKT string. Defaults to \code{"EPSG:4326"} (WGS84 
#'        longitude/latitude).
#' @template conn_null
#' @template name
#' @param crs_column \link{Deprecated} a character string of length one specifying the column
#'        storing the CRS (created automatically by \code{\link{ddbs_write_vector}}).
#'        Set to `NULL` if absent.
#' @template output
#' @template overwrite
#' @template quiet
#'
#' @template returns_output
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#'
#' ## create sample data with coordinates
#' cities_df <- data.frame(
#'   city = c("Buenos Aires", "Córdoba", "Rosario"),
#'   lon = c(-58.3816, -64.1811, -60.6393),
#'   lat = c(-34.6037, -31.4201, -32.9468),
#'   population = c(3075000, 1391000, 1193605)
#' )
#'
#' # option 1: convert data frame to sf object
#' cities_ddbs <- ddbs_as_spatial(cities_df)
#'
#' # specify custom coordinate column names
#' cities_df2 <- data.frame(
#'   city = c("Mendoza", "Tucumán"),
#'   longitude = c(-68.8272, -65.2226),
#'   latitude = c(-32.8895, -26.8241)
#' )
#' 
#' ddbs_as_spatial(cities_df2, coords = c("longitude", "latitude"))
#'
#'
#' ## option 2: convert table in duckdb to spatial table
#'
#' # create a duckdb connection and write data
#' conn <- duckspatial::ddbs_create_conn()
#' DBI::dbWriteTable(conn, "cities_tbl", cities_df, overwrite = TRUE)
#'
#' # convert to spatial table in database
#' ddbs_as_spatial(
#'     x = "cities_tbl",
#'     conn = conn,
#'     name = "cities_spatial",
#'     overwrite = TRUE
#' )
#'
#' # read the spatial table
#' ddbs_read_vector(conn, "cities_spatial")
#' }
ddbs_as_spatial <- function(
    x,
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    conn = NULL,
    name = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
  

    ## 0. Handle errors
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(output, "output")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
  

    # 1. Manage connection to DB
    
    ## 1.1. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
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

    ## 3.1. Get column names
    all_cols <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)

    ## 3.2. Coords as character
    coords_str <- paste0(coords,  collapse = ", ")


    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {
      
        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query (no st_as_text)
        tmp.query <-glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT {all_cols}
            '{crs}' AS '{crs_column}',
            ST_Point({coords_str}) as geometry
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
        SELECT {all_cols}
        ST_AsWKB(ST_Point({coords_str})) as geometry
        FROM {x_list$query_name};
    ")

    ## 5.2. retrieve results from the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)


    # 6. convert to SF and return result
    data_sf <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = crs,
        crs_column = NULL,
        x_geom     = "geometry"
    )

    feedback_query(quiet)
    return(data_sf)
}

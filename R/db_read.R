
#' Load vectorial data from DuckDB into R
#'
#' Retrieves the data from a DuckDB table with a geometry column, and convert
#' it to an R \code{sf} object.
#'
#' @param conn a connection object to a DuckDB database
#' @param name a character string of length one specifying the name of the table,
#' or a character string of length two specifying the schema and table names.
#' @param crs the coordinates reference system of the data
#'
#' @returns an sf object
#' @export
#'
#' @examples
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' ## connect to in memory database
#' conn <- dbConnect(duckdb::duckdb())
#'
#' ## install the spatial exntesion
#' ddbs_install(conn)
#' ddbs_load(conn)
#'
#' ## create random points
#' random_points <- data.frame(
#'   id = 1:5,
#'   x = runif(5, min = -180, max = 180),  # Random longitude values
#'   y = runif(5, min = -90, max = 90)     # Random latitude values
#' )
#'
#' ## convert to sf
#' sf_points <- st_as_sf(random_points, coords = c("x", "y"), crs = 4326)
#'
#' ## insert data into the database
#' ddbs_write_vector(conn, sf_points, "points")
#'
#' ## read data back into R
#' ddbs_read_vector(conn, "points", crs = 4326)
#'
#' ## disconnect from db
#' dbDisconnect(conn)
#'
ddbs_read_vector <- function(conn, name, crs = NULL) {

  # 1. Checks
  ## Check if connection is correct
  dbConnCheck(conn)
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
  ## Check if table name exists
  if (!table_name %in% DBI::dbListTables(conn))
      cli::cli_abort("The provided name is not present in the database.")
  ## check if geometry column is present
  info_tbl  <- DBI::dbGetQuery(conn, glue::glue("PRAGMA table_info('{query_name}');"))
  geom_name <- info_tbl[info_tbl$type == "GEOMETRY", "name"]
  if (length(geom_name) == 0) {
      cli::cli_abort("Geometry column wasn't found in table <{name}>.")
  }

  # 2. Retrieve data
  ## Get column names except geometry col
  no_geom_cols <- setdiff(
      DBI::dbListFields(conn,  DBI::Id(schema = schema_name, table = table_name)),
      geom_name
  ) |> paste(collapse = ", ")
  ## Retrieve data as data frame
  data_tbl <- DBI::dbGetQuery(conn, glue::glue(
          "SELECT
          {no_geom_cols},
          ST_AsText({geom_name}) AS {geom_name}
          FROM {query_name}"
  ))
  ## Convert to sf
  if (is.null(crs)) {
      data_sf <- data_tbl |>
          sf::st_as_sf(wkt = geom_name)
  } else {
      data_sf <- data_tbl |>
          sf::st_as_sf(wkt = geom_name, crs = crs)
  }
  cli::cli_alert_success("Table {name} successfully imported. Note that SRID is not currently stored in the database.")
  return(data_sf)

}
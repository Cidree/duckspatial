
#' Load vectorial data from DuckDB into R
#'
#' Retrieves the data from a DuckDB table with a geometry column, and convert
#' it to an R \code{sf} object.
#'
#' @template conn
#' @param name A character string of length one specifying the name of the table,
#'        or a character string of length two specifying the schema and table
#'        names.
#' @template crs
#' @param clauses character, additional SQL code to modify the query from the
#' table (e.g. "WHERE ...", "ORDER BY...")
#' @template quiet
#'
#' @returns an sf object
#' @export
#'
#' @examplesIf interactive()
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
#'   x = runif(5, min = -180, max = 180),
#'   y = runif(5, min = -90, max = 90)
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
ddbs_read_vector <- function(conn,
                             name,
                             crs = NULL,
                             crs_column = "crs_duckspatial",
                             clauses = NULL,
                             quiet = FALSE) {

  # 1. Checks
  ## Check if connection is correct
  dbConnCheck(conn)
  ## convenient names of table and/or schema.table
  name_list <- get_query_name(name)
  ## Check if table name exists
  if (!name_list$table_name %in% DBI::dbListTables(conn))
      cli::cli_abort("The provided name is not present in the database.")
  ## get column names
  geom_name    <- get_geom_name(conn, name_list$query_name)
  no_geom_cols <- get_geom_name(conn, name_list$query_name, rest = TRUE) |> paste(collapse = ", ")
  if (length(geom_name) == 0) cli::cli_abort("Geometry column wasn't found in table <{name_list$query_name}>.")

  # 2. Retrieve data
  ## Retrieve data as data frame
  tmp.query <- glue::glue(
          "SELECT
          {no_geom_cols},
          ST_AsText({geom_name}) AS {geom_name}
          FROM {name_list$query_name}"
  )
  tmp.query <- paste(tmp.query, clauses)
  data_tbl <- DBI::dbGetQuery(conn, tmp.query)
  ## Convert to sf
  if (is.null(crs)) {
    if (is.null(crs_column)) {
      data_sf <- data_tbl |>
        sf::st_as_sf(wkt = geom_name)
    } else {
      data_sf <- data_tbl |>
        sf::st_as_sf(wkt = geom_name, crs = data_tbl[1, crs_column])
      data_sf <- data_sf[, -which(names(data_sf) == crs_column)]
    }

  } else {
      data_sf <- data_tbl |>
          sf::st_as_sf(wkt = geom_name, crs = crs)
  }


  if (isFALSE(quiet)) {
      cli::cli_alert_success("Table {name} successfully imported.")
  }

  return(data_sf)

}

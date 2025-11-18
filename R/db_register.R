#' Register an SF Object as an Arrow Table in DuckDB
#'
#' This function registers a Simple Features (SF) object as a temporary Arrow-backed
#' view in a DuckDB database. This is a zero-copy operation and is significantly
#' faster than `ddbs_write_vector` for workflows that do not require data to be
#' permanently materialized in the database.
#'
#' @template conn
#' @param data A `sf` object to register in the DuckDB database.
#' @param name The name of the temporary view to create in the database.
#' @returns TRUE (invisibly) on successful registration.
#' @export
#' @examples
#' \dontrun{
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' conn <- ddbs_create_conn()
#'
#' nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
#'
#' ddbs_register_vector(conn, nc, "nc_arrow_view")
#'
#' dbGetQuery(conn, "SELECT COUNT(*) FROM nc_arrow_view;")
#'
#' dbDisconnect(conn, shutdown = TRUE)
#'}
ddbs_register_vector <- function(conn, data, name) {
  # 1. Checks
  ## Check if connection is correct
  dbConnCheck(conn)
  if (!inherits(data, "sf")) {
    cli::cli_abort("{.arg data} must be an {.cls sf} object.")
  }

  # Try to register geoarrow extensions when available
  try(
    DBI::dbExecute(conn, "CALL register_geoarrow_extensions();"),
    silent = TRUE
  )

  # 2. Register table
  df <- sf::st_drop_geometry(data)
  wkb <- wk::as_wkb(sf::st_geometry(data))

  # Use geoarrow to create a geoarrow vector from WKB
  df$geometry <- geoarrow::as_geoarrow_vctr(
    wkb,
    schema = geoarrow::geoarrow_wkb()
  )

  arrow_table <- arrow::Table$create(df)

  if (duckdb::dbExistsTable(conn, name)) {
    duckdb::duckdb_unregister_arrow(conn, name)
  }
  duckdb::duckdb_register_arrow(conn, name, arrow_table)

  invisible(TRUE)
}

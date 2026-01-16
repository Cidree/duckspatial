
test_that("duckspatial_df uses efficient SQL render fallback when source_table is missing", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  # Setup: Create a table and a duckspatial_df from a query
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  # Create a query-based duckspatial_df (no direct source table)
  # This typically results in source_table=NULL in as_duckspatial_df
  # providing we do it in a way that generates a query.
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test") |> dplyr::filter(AREA > 0)
  
  # Ensure it is a duckspatial_df
  ds <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  # Manually ensure source_table is NULL to force the fallback path logic
  attr(ds, "source_table") <- NULL
  
  # Call get_query_list (internal function)
  # We need to access it via triple colon or having it exported (it's not exported)
  # Since we are in the package tests, we can access internal functions if using devtools::test() 
  # but distinct test files run in separate environments. 
  # Usually testthat runs with access to internal functions.
  
  x_list <- get_query_list(ds, conn)
  
  # Verify we got a temp view name
  temp_view_name <- x_list$query_name
  expect_true(grepl("temp_view_", temp_view_name))
  
  # CLEANUP check: Register cleanup to occur on exit of this test block
  on.exit(x_list$cleanup(), add = TRUE)
  
  # REGRESSION CHECK:
  # The efficient method creates a VIEW using SQL.
  # The inefficient method used duckdb_register_arrow (via ddbs_write_vector(temp_view=TRUE)),
  # which creates a replacement scan view (typically no SQL definition in duckdb_views or different).
  
  # Let's inspect the view definition
  view_info <- DBI::dbGetQuery(conn, glue::glue(
    "SELECT sql FROM duckdb_views() WHERE view_name = '{temp_view_name}'"
  ))
  
  expect_equal(nrow(view_info), 1)
  
  view_sql <- view_info$sql
  
  # The new efficient method should show the CREATE VIEW AS SELECT ... statement
  expect_false(is.null(view_sql))
  expect_false(is.na(view_sql))
  # It should contain the original query logic (e.g. "AREA" > 0)
  expect_true(grepl("AREA", view_sql) || grepl("area", view_sql, ignore.case = TRUE))
  expect_true(grepl("SELECT", view_sql, ignore.case = TRUE))
  
  # If it were the old method (Arrow registration), the SQL for the view is usually internal
  # or at least wouldn't reflect the SQL query "AREA > 0", because it would be serving
  # materialized data (points/polygons) from the Arrow stream.
})

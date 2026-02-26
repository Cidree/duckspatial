
test_that("get_query_list handles sf objects", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  
  # Action
  res <- get_query_list(nc_sf, conn)
  
  # Verify
  expect_true(grepl("temp_view_", res$query_name))
  expect_true(DBI::dbExistsTable(conn, res$query_name))
  
  # Verify cleanup
  res$cleanup()
  expect_false(DBI::dbExistsTable(conn, res$query_name))
})

test_that("get_query_list handles duckspatial_df with source_table", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_source", quiet = TRUE)
  
  ds <- as_duckspatial_df("nc_source", conn = conn, crs = sf::st_crs(nc_sf))
  
  # Action
  res <- get_query_list(ds, conn)
  
  # Verify - should return source name directly
  expect_equal(res$query_name, "nc_source")
  
  # Cleanup should be no-op (source table remains)
  res$cleanup()
  expect_true(DBI::dbExistsTable(conn, "nc_source"))
})

test_that("get_query_list handles duckspatial_df WITHOUT source_table (efficient fallback)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  # Create query-based duckspatial_df
  lazy_tbl <- dplyr::tbl(conn, "nc_test") |> dplyr::filter(AREA > 0)
  ds <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  # Force source_table to NULL to trigger fallback
  attr(ds, "source_table") <- NULL
  
  # Action
  res <- get_query_list(ds, conn)
  
  # Verify
  expect_true(grepl("temp_view_", res$query_name))
  
  # Check it's a view with SQL definition (proving sql_render was used)
  view_def <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{res$query_name}'"))
  expect_true(nrow(view_def) == 1)
  expect_true(grepl("SELECT", view_def$sql, ignore.case = TRUE))
  
  # Verify cleanup
  res$cleanup()
  expect_false(DBI::dbExistsTable(conn, res$query_name))
})

test_that("get_query_list handles tbl_lazy objects", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  DBI::dbWriteTable(conn, "mtcars_test", mtcars)
  
  tbl_lazy <- dplyr::tbl(conn, "mtcars_test") |> dplyr::filter(mpg > 20)
  
  # Action
  res <- get_query_list(tbl_lazy, conn)
  
  # Verify
  expect_true(grepl("temp_view_", res$query_name))
  
  # Check view
  view_def <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{res$query_name}'"))
  expect_true(nrow(view_def) == 1)
  expect_true(grepl("mpg > 20", view_def$sql) || grepl('"mpg" > 20.0', view_def$sql))
  
  # Verify cleanup
  res$cleanup()
  expect_false(DBI::dbExistsTable(conn, res$query_name))
})

test_that("get_query_list handles character inputs", {
  conn <- ddbs_temp_conn()
  DBI::dbWriteTable(conn, "plain_table", data.frame(id = 1))
  
  # Action
  res <- get_query_list("plain_table", conn)
  
  # Verify
  expect_equal(res$query_name, "plain_table")
  expect_equal(res$table_name, "plain_table")
  
  # Cleanup is no-op
  res$cleanup()
  expect_true(DBI::dbExistsTable(conn, "plain_table"))
})

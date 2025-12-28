# skip tests on CRAN
skip_on_cran()
skip_if_not_installed("duckdb")
skip_if_not_installed("sf")

# Setup: connection and data
conn_test <- duckspatial::ddbs_create_conn()

# Write some data to test against
ddbs_write_vector(conn_test, points_sf, "test_points_read", overwrite = TRUE)
ddbs_write_vector(conn_test, countries_sf, "test_countries_read", overwrite = TRUE)
ddbs_register_vector(conn_test, points_sf, "test_points_view_read", overwrite = TRUE)

# 1. Basic reading from tables and views ----
test_that("ddbs_read_vector reads a table into an sf object", {
  # Read from a table created with ddbs_write_vector
  result <- ddbs_read_vector(conn_test, "test_points_read", crs = 4326)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(points_sf))
  expect_equal(sf::st_crs(result), sf::st_crs(points_sf))
  expect_true(all(c("id", "geometry") %in% names(result)))
})

test_that("ddbs_read_vector reads an Arrow view into an sf object", {
  # Read from a view created with ddbs_register_vector
  result <- ddbs_read_vector(conn_test, "test_points_view_read", crs = 4326)

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(points_sf))
  expect_equal(sf::st_crs(result), sf::st_crs(points_sf))
})

test_that("ddbs_read_vector reads a standard SQL view into an sf object", {
  # Create a standard SQL view from an existing table
  # First ensure the base table exists
  ddbs_write_vector(conn_test, points_sf, "base_table_for_view", overwrite = TRUE)
  
  # Create SQL view
  DBI::dbExecute(conn_test, "CREATE OR REPLACE VIEW test_sql_view AS SELECT * FROM base_table_for_view")
  
  # Read from the SQL view
  result <- ddbs_read_vector(conn_test, "test_sql_view", crs = 4326)
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(points_sf))
  expect_equal(sf::st_crs(result), sf::st_crs(points_sf))
  
  # Cleanup
  DBI::dbExecute(conn_test, "DROP VIEW IF EXISTS test_sql_view")
})

# 2. Parameters and clauses ----
test_that("ddbs_read_vector's 'clauses' argument works", {
  # Use a WHERE clause to filter the data
  result <- ddbs_read_vector(conn_test, "test_points_read", crs = 4326, clauses = "WHERE id <= 3")

  expect_equal(nrow(result), 3)
  expect_true(all(result$id <= 3))
})

test_that("ddbs_read_vector works with different geometry types", {
  # Test with polygons
  result <- ddbs_read_vector(conn_test, "test_countries_read", crs = 4326)
  expect_s3_class(result, "sf")
  expect_true(any(grepl("POLYGON", sf::st_geometry_type(result))))
})

# 3. CRS handling ----
test_that("ddbs_read_vector infers CRS from crs_duckspatial column", {
  # The 'test_points_read' table has crs_duckspatial = '4326'
  result <- ddbs_read_vector(conn_test, "test_points_read")
  expect_equal(sf::st_crs(result), sf::st_crs(4326))
})

test_that("ddbs_read_vector 'crs' parameter overrides stored CRS", {
  # Store with 4326, read with a different one (e.g., 3857)
  result <- ddbs_read_vector(conn_test, "test_points_read", crs = 3857)
  expect_equal(sf::st_crs(result), sf::st_crs(3857))
})

test_that("ddbs_read_vector handles tables with no CRS info", {
  # Create a table with no CRS
  points_no_crs <- points_sf
  sf::st_crs(points_no_crs) <- NA
  ddbs_write_vector(conn = conn_test, data = points_no_crs, name = "test_no_crs_read", overwrite = TRUE)

  # Read without specifying CRS (should result in NA CRS)
  expect_message(
    result <- ddbs_read_vector(conn_test, "test_no_crs_read"),
    "No CRS found"
  )
  expect_true(is.na(sf::st_crs(result)))

  # Read while specifying CRS
  result_with_crs <- ddbs_read_vector(conn_test, "test_no_crs_read", crs = 4326)
  expect_equal(sf::st_crs(result_with_crs), sf::st_crs(4326))
})

# 4. Errors ----
test_that("ddbs_read_vector throws an error for non-existent tables/views", {
  expect_error(
    ddbs_read_vector(conn_test, "this_table_does_not_exist"),
    "not present in the database"
  )
})

# Disconnect
duckdb::dbDisconnect(conn_test, shutdown = TRUE)


test_that("ddbs_default_conn works as expected", {
  skip_if_not_installed("duckdb")
  
  # Clear any existing default connection to start fresh
  opts <- options(duckspatial_conn = NULL)
  on.exit(options(opts))
  
  # 1. Should create new connection
  conn1 <- duckspatial:::ddbs_default_conn(create = TRUE)
  expect_true(DBI::dbIsValid(conn1))
  expect_true(inherits(conn1, "duckdb_connection"))
  
  # 2. Should reuse the same connection
  conn2 <- duckspatial:::ddbs_default_conn(create = TRUE)
  expect_identical(conn1, conn2) # Should be same object reference
  
  # 3. Method for checking without creating
  # Clearing option first
  options(duckspatial_conn = NULL)
  conn3 <- duckspatial:::ddbs_default_conn(create = FALSE)
  expect_null(conn3)
})

test_that("get_file_crs extracts CRS correctly", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("sf")
  
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  # Test with NC shapefile (EPSG:4267)
  path <- system.file("shape/nc.shp", package = "sf")
  
  crs <- duckspatial:::get_file_crs(path, conn)
  expect_s3_class(crs, "crs")
  expect_equal(crs$epsg, 4267)
})

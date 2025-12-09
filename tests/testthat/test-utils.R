# testthat::skip_if(Sys.getenv("TEST_ONE") != "")
# testthat::skip_on_cran()

test_that("crs_to_sql handles various inputs correctly", {
  # Numeric input (EPSG code)
  expect_equal(crs_to_sql(4326), "'EPSG:4326'")
  expect_equal(crs_to_sql(3857.9), "'EPSG:3857'") # casts to integer

  # Character input (WKT or PROJ string)
  expect_equal(crs_to_sql("EPSG:4326"), "'EPSG:4326'")
  
  # Character input with single quotes (SQL escaping)
  wkt_quote <- "PROJCS['Test', ...]"
  expected_quote <- "'PROJCS[''Test'', ...]'"
  expect_equal(crs_to_sql(wkt_quote), expected_quote)

  # sf crs objects
  # We construct a mock CRS object to test logic independently of PROJ/GDAL versions
  
  # Case 1: CRS with EPSG code (Standard SF object structure)
  crs_epsg <- structure(list(epsg = 4326, wkt = "GEOGCS[...]"), class = "crs")
  expect_equal(crs_to_sql(crs_epsg), "'EPSG:4326'")

  # Case 2: CRS without EPSG code (WKT only)
  # Mimic an sf object where epsg is NA
  wkt_raw <- "GEOGCS['TestCRS', ...]"
  crs_wkt <- structure(list(epsg = NA, wkt = wkt_raw), class = "crs")
  
  sql_out <- crs_to_sql(crs_wkt)
  # Should return quoted string with escaped internal quotes
  expected_wkt_sql <- "'GEOGCS[''TestCRS'', ...]'"
  expect_equal(sql_out, expected_wkt_sql)

  # NULL/NA inputs
  expect_equal(crs_to_sql(NULL), "NULL")
  expect_equal(crs_to_sql(NA), "NULL")
})

test_that("assert_col_exists validates database columns", {
  skip_if_not_installed("duckdb")
  
  # Setup dummy connection and table
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn), add = TRUE)
  
  DBI::dbExecute(conn, "CREATE TABLE test_utils (id INTEGER, val_a DOUBLE, val_b DOUBLE)")

  # Success cases
  expect_no_error(assert_col_exists(conn, "test_utils", "id", "my_func"))
  expect_no_error(assert_col_exists(conn, "test_utils", c("id", "val_a"), "my_func"))

  # Failure cases
  # 1. Single missing column
  expect_error(
    assert_col_exists(conn, "test_utils", "missing_col", "test_ref"),
    "missing_col"
  )
  
  # 2. Partial missing columns
  expect_error(
    assert_col_exists(conn, "test_utils", c("id", "missing_one"), "test_ref"),
    "missing_one"
  )
})

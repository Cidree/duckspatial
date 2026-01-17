skip_if_not_installed("duckdb")

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
  
  conn <- ddbs_temp_conn()
  
  # Test with NC shapefile (EPSG:4267)
  path <- system.file("shape/nc.shp", package = "sf")
  
  crs <- duckspatial:::get_file_crs(path, conn)
  expect_s3_class(crs, "crs")
  expect_equal(crs$epsg, 4267)
})


# detect_crs --------------------------------------------------------------
# 
# This helper is called BEFORE normalize_spatial_input() in ddbs_join/filter/area
# to pre-extract CRS from the original input. It handles:
# 1. sf objects → uses st_crs() (sf stores CRS differently than attr)
# 2. duckspatial_df → extracts from "crs" attribute (set by new_duckspatial_df)
# 3. tbl_duckdb_connection → tries ddbs_crs() fallback (often returns NULL)
# 4. character/other → returns NULL (no CRS attribute)

test_that("detect_crs(): extracts CRS from sf via st_crs()", {
  # sf objects store CRS via st_crs(), not attr(x, "crs")
  # detect_crs should use sf::st_crs() for sf input
  
  crs_out <- detect_crs(nc_sf)
  expect_s3_class(crs_out, "crs")
  expect_equal(crs_out, sf::st_crs(nc_sf))
})

test_that("detect_crs(): extracts CRS from duckspatial_df attribute", {
  # duckspatial_df stores CRS in attr(x, "crs"), set by new_duckspatial_df()
  
  crs_out <- detect_crs(nc_ddbs)
  expect_s3_class(crs_out, "crs")
  # Compare EPSG codes (WKT representations may differ between sf and DuckDB)
  expect_equal(crs_out$epsg, sf::st_crs(nc_sf)$epsg)
})

test_that("detect_crs(): attempts ddbs_crs() for tbl_duckdb_connection", {
  conn <- ddbs_temp_conn()
  
  # For raw tbl_duckdb_connection (not duckspatial_df), detect_crs tries

  # ddbs_crs() which attempts to extract CRS from view SQL containing ST_Read().
  # This often fails (returns NULL) when the view doesn't use ST_Read.
  
  # Case 1: Plain table without geometry - should return NULL
  DBI::dbExecute(conn, "CREATE OR REPLACE TABLE plain_table AS SELECT 1 as id")
  plain_tbl <- dplyr::tbl(conn, "plain_table")
  expect_null(detect_crs(plain_tbl))
  
  # Case 2: Table created via ddbs_write_vector - CRS is in metadata column,
  # but ddbs_crs.tbl_duckdb_connection cannot access it (returns NULL)
  ddbs_write_vector(conn, nc_sf, "nc_table", overwrite = TRUE)
  nc_tbl <- dplyr::tbl(conn, "nc_table")
  # Returns NULL because ddbs_crs can't detect CRS from regular tables
  expect_null(detect_crs(nc_tbl))
})

test_that("detect_crs(): returns NULL for character and other inputs", {
  # Character table names have no CRS attribute
  expect_null(detect_crs("some_table_name"))
  
  # Plain data frames have no CRS
  expect_null(detect_crs(data.frame(x = 1)))
  
  # NULL input
  expect_null(detect_crs(NULL))
})

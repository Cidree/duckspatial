skip_if_not_installed("duckdb")
testthat::skip_on_cran()

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
  
  conn <- duckspatial:::ddbs_temp_conn()
  
  # Test with NC shapefile (EPSG:4267)
  path <- system.file("shape/nc.shp", package = "sf")
  
  crs <- duckspatial:::get_file_crs(path, conn)
  expect_s3_class(crs, "crs")
  expect_equal(crs$epsg, 4267)
})


test_that("reframe_predicate_data keeps pairs at row 100000 (factor() on doubles gives '1e+05')", {
  skip_if_not_installed("duckdb")

  conn <- duckspatial::ddbs_create_conn()
  on.exit(duckspatial::ddbs_stop_conn(conn))
  DBI::dbExecute(conn, "CREATE TEMP TABLE rx AS SELECT range AS i FROM range(100000)")
  DBI::dbExecute(conn, "CREATE TEMP TABLE ry AS SELECT 1 AS j")

  ## the (i, j) pairs come from DuckDB as doubles (BIGINT)
  res <- duckspatial:::reframe_predicate_data(
    conn   = conn,
    data   = data.frame(i = c(1, 1e5), j = c(1, 1)),
    x_list = list(query_name = "rx"),
    y_list = list(query_name = "ry"),
    id_x   = NULL,
    id_y   = NULL,
    sparse = TRUE
  )
  expect_length(res, 100000)
  expect_equal(res[[1]], 1L)
  expect_equal(res[[100000]], 1L)
  expect_null(names(res))

  mat <- duckspatial:::reframe_predicate_data(
    conn, data.frame(i = c(1, 1e5), j = c(1, 1)),
    list(query_name = "rx"), list(query_name = "ry"), NULL, NULL, sparse = FALSE
  )
  expect_equal(dim(mat), c(100000L, 1L))
  expect_equal(sum(mat), 2L)
  expect_true(mat[100000, 1])
})

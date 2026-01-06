# =============================================================================
# Tests for duckspatial_df core functionality
# Tests: new_duckspatial_df, as_duckspatial_df.*, is_duckspatial_df
# Note: nc_sf is loaded from setup.R
# =============================================================================

test_that("new_duckspatial_df creates valid duckspatial_df objects", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  
  result <- new_duckspatial_df(
    lazy_tbl, 
    crs = sf::st_crs(nc_sf), 
    geom_col = "geometry",
    source_table = "nc_test"
  )
  
  expect_s3_class(result, "duckspatial_df")
  expect_s3_class(result, "tbl_lazy")
  expect_equal(attr(result, "sf_column"), "geometry")
  expect_equal(attr(result, "crs"), sf::st_crs(nc_sf))
  expect_equal(attr(result, "source_table"), "nc_test")
})

test_that("new_duckspatial_df avoids double wrapping", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  result1 <- new_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  result2 <- new_duckspatial_df(result1, crs = sf::st_crs(nc_sf))
  
  expect_identical(result1, result2)
})

test_that("as_duckspatial_df.duckspatial_df can update metadata", {
  conn <- ddbs_temp_conn()
  ds <- as_duckspatial_df(nc_sf, conn = conn)
  
  ds_new <- as_duckspatial_df(ds, crs = "EPSG:3857")
  expect_equal(sf::st_crs(ds_new), sf::st_crs("EPSG:3857"))
  
  ds_new2 <- as_duckspatial_df(ds, geom_col = "new_geom")
  expect_equal(attr(ds_new2, "sf_column"), "new_geom")
  
  expect_identical(ds, as_duckspatial_df(ds))
})

test_that("as_duckspatial_df.sf works correctly", {
  conn <- ddbs_temp_conn()
  
  result <- as_duckspatial_df(nc_sf, conn = conn)
  
  expect_s3_class(result, "duckspatial_df")
  expect_s3_class(result, "tbl_lazy")
  expect_equal(attr(result, "crs"), sf::st_crs(nc_sf))
  expect_equal(attr(result, "sf_column"), attr(nc_sf, "sf_column"))
})

test_that("as_duckspatial_df.tbl_duckdb_connection works correctly", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  result <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), sf::st_crs(nc_sf))
})

test_that("as_duckspatial_df.tbl_lazy works correctly", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test") |> dplyr::filter(AREA > 0)
  result <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), sf::st_crs(nc_sf))
})

test_that("as_duckspatial_df.character works correctly", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  result <- as_duckspatial_df("nc_test", conn = conn, crs = sf::st_crs(nc_sf))
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "source_table"), "nc_test")
})

test_that("as_duckspatial_df.character requires connection", {
  expect_error(
    as_duckspatial_df("some_table"),
    "conn|Table.*does not exist"
  )
})

test_that("as_duckspatial_df.data.frame works with sfc columns", {
  conn <- ddbs_temp_conn()
  
  df <- data.frame(id = 1, val = "a")
  df$geom <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  
  result <- as_duckspatial_df(df, conn = conn)
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(sf::st_crs(result), sf::st_crs(4326))
  expect_equal(attr(result, "sf_column"), "geom")
  
  result2 <- as_duckspatial_df(df, conn = conn, geom_col = "geom")
  expect_equal(attr(result2, "sf_column"), "geom")
  
  df_no_geom <- data.frame(id = 1)
  expect_error(as_duckspatial_df(df_no_geom, conn = conn), "sfc")
  
  df_wrong_geom_multi <- data.frame(id = 1, not_geom = 2)
  df_wrong_geom_multi$real_geom <- sf::st_sfc(sf::st_point(c(0, 0)))
  expect_error(
    as_duckspatial_df(df_wrong_geom_multi, conn = conn, geom_col = "not_geom"), 
    "sfc"
  )
})

test_that("is_duckspatial_df works correctly", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  result <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  expect_true(is_duckspatial_df(result))
  expect_false(is_duckspatial_df(lazy_tbl))
  expect_false(is_duckspatial_df(nc_sf))
  expect_false(is_duckspatial_df(NULL))
})

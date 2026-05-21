# =============================================================================
# Tests for duckspatial_df core functionality
# Tests: new_duckspatial_df, as_duckspatial_df.*, is_duckspatial_df
# Note: nc_sf is loaded from setup.R
# =============================================================================
testthat::skip_on_cran()

test_that("new_duckspatial_df creates valid duckspatial_df objects", {
  conn <- duckspatial:::ddbs_temp_conn()
  ddbs_write_table(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  
  result <- duckspatial:::new_duckspatial_df(
    lazy_tbl, 
    crs = sf::st_crs(nc_sf), 
    geom_col = "geometry",
    source_table = "nc_test"
  )
  
  expect_s3_class(result, "duckspatial_df")
  expect_s3_class(result, "tbl_lazy")
  expect_equal(attr(result, "sf_column"), "geometry")
  expect_true(duckspatial:::crs_equal(attr(result, "crs"), sf::st_crs(nc_sf)))
  expect_equal(attr(result, "source_table"), "nc_test")
})

test_that("new_duckspatial_df avoids double wrapping", {
  conn <- duckspatial:::ddbs_temp_conn()
  ddbs_write_table(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  result1 <- duckspatial:::new_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  result2 <- duckspatial:::new_duckspatial_df(result1, crs = sf::st_crs(nc_sf))
  
  expect_identical(result1, result2)
})

test_that("as_duckspatial_df.duckspatial_df can update metadata", {
  conn <- duckspatial:::ddbs_temp_conn()
  ds <- as_duckspatial_df(nc_sf, conn = conn)
  
  ds_new <- as_duckspatial_df(ds, crs = "EPSG:3857")
  expect_equal(sf::st_crs(ds_new), sf::st_crs("EPSG:3857"))
  
  ds_new2 <- as_duckspatial_df(ds, geom_col = "new_geom")
  expect_equal(attr(ds_new2, "sf_column"), "new_geom")
  
  expect_identical(ds, as_duckspatial_df(ds))
})

test_that("as_duckspatial_df.sf works correctly", {
  conn <- duckspatial:::ddbs_temp_conn()
  
  result <- as_duckspatial_df(nc_sf, conn = conn)
  
  expect_s3_class(result, "duckspatial_df")
  expect_s3_class(result, "tbl_lazy")
  expect_true(duckspatial:::crs_equal(attr(result, "crs"), sf::st_crs(nc_sf)))
  expect_equal(attr(result, "sf_column"), attr(nc_sf, "sf_column"))
})

test_that("as_duckspatial_df.tbl_duckdb_connection works correctly", {
  conn <- duckspatial:::ddbs_temp_conn()
  ddbs_write_table(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  # Auto-detection should work now!
  result <- as_duckspatial_df(lazy_tbl)
  
  expect_s3_class(result, "duckspatial_df")
  expect_true(duckspatial:::crs_equal(attr(result, "crs"), sf::st_crs(nc_sf)))
})

test_that("as_duckspatial_df.tbl_lazy works correctly", {
  conn <- duckspatial:::ddbs_temp_conn()
  ddbs_write_table(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test") |> dplyr::filter(AREA > 0)
  result <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))  
  
  expect_s3_class(result, "duckspatial_df")
  expect_true(duckspatial:::crs_equal(attr(result, "crs"), sf::st_crs(nc_sf)))
})

test_that("as_duckspatial_df.character works correctly", {
  conn <- duckspatial:::ddbs_temp_conn()
  ddbs_write_table(conn, nc_sf, "nc_test", quiet = TRUE)
  
  result <- as_duckspatial_df("nc_test", conn = conn, crs = sf::st_crs(nc_sf))
  
  expect_s3_class(result, "duckspatial_df")
  # expect_equal(attr(result, "source_table"), "nc_test")
})

test_that("as_duckspatial_df.character requires connection", {
  expect_error(
    as_duckspatial_df("some_table"),
    "conn|table|Table.*does not exist"
  )
})

test_that("as_duckspatial_df.data.frame works with sfc columns", {
  conn <- duckspatial:::ddbs_temp_conn()
  
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

test_that("as_duckspatial_df handles coords ingestion across inputs", {
  conn <- duckspatial:::ddbs_temp_conn()
  df <- data.frame(id = 1:3, lon = 1:3, lat = 4:6)
  
  # 1. Data frame
  ds_df <- as_duckspatial_df(df, coords = c("lon", "lat"), conn = conn, crs = 4326)
  expect_s3_class(ds_df, "duckspatial_df")
  expect_equal(sf::st_crs(ds_df), sf::st_crs(4326))
  expect_false("lon" %in% colnames(ds_df))
  
  # 2. Character (table name)
  DBI::dbWriteTable(conn, "points_tbl", df, overwrite = TRUE)
  ds_char <- as_duckspatial_df("points_tbl", coords = c("lon", "lat"), conn = conn, crs = 3857)
  expect_s3_class(ds_char, "duckspatial_df")
  expect_equal(sf::st_crs(ds_char), sf::st_crs(3857))
  
  # 3. Lazy table
  lazy_tbl <- dplyr::tbl(conn, "points_tbl")
  ds_lazy <- as_duckspatial_df(lazy_tbl, coords = c("lon", "lat"), crs = 4326)
  expect_s3_class(ds_lazy, "duckspatial_df")
  
  # na.fail = TRUE (default)
  df_na <- df
  df_na$lon[1] <- NA
  expect_error(as_duckspatial_df(df_na, coords = c("lon", "lat"), conn = conn, crs = 4326))
  
  # remove = FALSE
  ds_keep <- as_duckspatial_df(df, coords = c("lon", "lat"), remove = FALSE, conn = conn, crs = 4326)
  expect_true("lon" %in% colnames(ds_keep))
})

test_that("as_duckspatial_df handles WKT ingestion across inputs", {
  conn <- duckspatial:::ddbs_temp_conn()
  df <- data.frame(id = 1:3, wkt_col = c("POINT(0 0)", "POINT(1 1)", "POINT(2 2)"))
  
  # 1. Data frame
  ds_df <- as_duckspatial_df(df, wkt = "wkt_col", conn = conn, crs = 4326)
  expect_s3_class(ds_df, "duckspatial_df")
  expect_equal(sf::st_crs(ds_df), sf::st_crs(4326))
  expect_false("wkt_col" %in% colnames(ds_df))
  
  # 2. Character (table name)
  DBI::dbWriteTable(conn, "wkt_tbl", df, overwrite = TRUE)
  ds_char <- as_duckspatial_df("wkt_tbl", wkt = "wkt_col", conn = conn, crs = 3857)
  expect_s3_class(ds_char, "duckspatial_df")
  
  # 3. Lazy table
  lazy_tbl <- dplyr::tbl(conn, "wkt_tbl")
  ds_lazy <- as_duckspatial_df(lazy_tbl, wkt = "wkt_col", crs = 4326)
  expect_s3_class(ds_lazy, "duckspatial_df")
  
  # na.fail = TRUE (default)
  df_na <- df
  df_na$wkt_col[1] <- NA
  expect_error(as_duckspatial_df(df_na, wkt = "wkt_col", conn = conn, crs = 4326))
  
  # remove = FALSE
  ds_keep <- as_duckspatial_df(df, wkt = "wkt_col", remove = FALSE, conn = conn, crs = 4326)
  expect_true("wkt_col" %in% colnames(ds_keep))
})

test_that("as_duckspatial_df ingestion respects geom_col", {
  conn <- duckspatial:::ddbs_temp_conn()
  df <- data.frame(id = 1, x = 0, y = 0)
  
  ds <- as_duckspatial_df(df, coords = c("x", "y"), geom_col = "my_geom", conn = conn, crs = 4326)
  expect_equal(attr(ds, "sf_column"), "my_geom")
  
  # Collect and check column names
  res_sf <- ddbs_collect(ds)
  expect_true("my_geom" %in% colnames(res_sf))
})

test_that("is_duckspatial_df works correctly", {
  conn <- duckspatial:::ddbs_temp_conn()
  ddbs_write_table(conn, nc_sf, "nc_test", quiet = TRUE)
  
  # lazy_tbl <- dplyr::tbl(conn, "nc_test")
  # result <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  result <- as_duckspatial_df("nc_test", conn, crs = sf::st_crs(nc_sf))
  
  expect_true(is_duckspatial_df(result))
  # expect_false(is_duckspatial_df(lazy_tbl))
  expect_false(is_duckspatial_df(nc_sf))
  expect_false(is_duckspatial_df(NULL))
})


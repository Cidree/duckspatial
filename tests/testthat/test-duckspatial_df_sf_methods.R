# =============================================================================
# Tests for duckspatial_df sf methods
# Tests: st_crs, st_bbox, st_geometry, st_as_sf, print, ddbs_geom_col
# Note: nc_sf is loaded from setup.R
# =============================================================================

test_that("st_crs.duckspatial_df returns correct CRS", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  expect_equal(sf::st_crs(nc_lazy), sf::st_crs(nc_sf))
})

test_that("st_bbox.duckspatial_df works correctly", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  bbox <- sf::st_bbox(nc_lazy)
  expect_s3_class(bbox, "bbox")
  
  bbox_orig <- sf::st_bbox(nc_sf)
  expect_equal(as.numeric(bbox), as.numeric(bbox_orig))
})

test_that("st_geometry.duckspatial_df works correctly", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  geom <- sf::st_geometry(nc_lazy)
  
  expect_s3_class(geom, "sfc")
  expect_equal(length(geom), nrow(nc_sf))
  expect_equal(sf::st_crs(geom), sf::st_crs(nc_sf))
})

test_that("st_as_sf.duckspatial_df works correctly", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  result_sf <- sf::st_as_sf(nc_lazy)
  
  expect_s3_class(result_sf, "sf")
  expect_s3_class(result_sf, "data.frame")
  expect_equal(nrow(result_sf), nrow(nc_sf))
})

test_that("print.duckspatial_df shows informative output", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  output <- capture.output(print(nc_lazy))
  
  expect_true(any(grepl("duckspatial lazy spatial table", output)))
  expect_true(any(grepl("CRS:", output)))
  expect_true(any(grepl("Geometry column:", output)))
})

test_that("ddbs_geom_col returns correct geometry column name", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  expect_equal(ddbs_geom_col(nc_lazy), "geometry")
  expect_equal(ddbs_geom_col(nc_sf), attr(nc_sf, "sf_column"))
})

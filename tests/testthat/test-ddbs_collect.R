
test_that("ddbs_collect supports all output formats", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  # Setup data
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:5,]
  ddbs_write_vector(conn, nc_sf, "nc_data")
  
  nc_lazy <- dplyr::tbl(conn, "nc_data") |> 
    as_duckspatial_df(geom_col = "geometry", crs = sf::st_crs(nc_sf))
    
  # 1. Default (sf)
  res_sf <- ddbs_collect(nc_lazy)
  expect_s3_class(res_sf, "sf")
  expect_s3_class(res_sf$geometry, "sfc")
  expect_equal(nrow(res_sf), 5)
  
  # 2. Tibble (no geometry vs geometry dropped?)
  # The implementation of collect.duckspatial_df(as="tibble") drops geometry if it exists in lazy table
  res_tbl <- ddbs_collect(nc_lazy, as = "tibble")
  expect_s3_class(res_tbl, "tbl_df")
  expect_false(inherits(res_tbl, "sf"))
  expect_false("geometry" %in% names(res_tbl)) # Should be dropped by logic
  
  # 3. Raw (WKB)
  res_raw <- ddbs_collect(nc_lazy, as = "raw")
  expect_s3_class(res_raw, "tbl_df")
  expect_true("geometry" %in% names(res_raw))
  # Check content is raw list or blob
  expect_true(is.list(res_raw$geometry))
  expect_true(is.raw(res_raw$geometry[[1]]) || inherits(res_raw$geometry[[1]], "blob"))
  
  # 4. GeoArrow
  # Only if geoarrow package is available (optional dependency)
  if (requireNamespace("geoarrow", quietly = TRUE)) {
    res_ga <- ddbs_collect(nc_lazy, as = "geoarrow")
    expect_s3_class(res_ga, "tbl_df")
    expect_true("geometry" %in% names(res_ga))
    expect_true(inherits(res_ga$geometry, "geoarrow_vctr"))
  }
})

test_that("st_as_sf.duckspatial_df delegates to collect(as='sf')", {
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:5,]
  ddbs_write_vector(conn, nc_sf, "nc_as_sf")
  nc_lazy <- dplyr::tbl(conn, "nc_as_sf") |> as_duckspatial_df(geom_col="geometry", crs=sf::st_crs(nc_sf))
  
  res <- sf::st_as_sf(nc_lazy)
  expect_s3_class(res, "sf")
})

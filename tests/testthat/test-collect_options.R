
test_that("collect.duckspatial_df respects global options", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  # Setup data
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:5,]
  ddbs_write_vector(conn, nc_sf, "nc_test_opts")
  
  # Create duckspatial_df
  x <- dplyr::tbl(conn, "nc_test_opts") |> 
     as_duckspatial_df(geom_col = "geometry", crs = sf::st_crs(nc_sf))
  
  # Save original option
  op_orig <- options()
  on.exit(options(op_orig), add = TRUE)
  
  # 1. Default (sf) behavior when option is "duckspatial_df" (default)
  ddbs_options(output_type = "duckspatial_df")
  res <- dplyr::collect(x)
  expect_s3_class(res, "sf")
  
  # 2. Option = "tibble"
  ddbs_options(output_type = "tibble")
  res_tbl <- dplyr::collect(x)
  expect_false(inherits(res_tbl, "sf"))
  expect_s3_class(res_tbl, "tbl_df")
  expect_false("geometry" %in% names(res_tbl))
  
  # 3. Option = "raw"
  ddbs_options(output_type = "raw")
  res_raw <- dplyr::collect(x)
  expect_true("geometry" %in% names(res_raw))
  expect_true(is.list(res_raw$geometry))
  
  # 4. Explicit override wins
  ddbs_options(output_type = "tibble")
  res_sf <- dplyr::collect(x, as = "sf")
  expect_s3_class(res_sf, "sf")
})

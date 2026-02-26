
test_that("DuckDB auto-detects spatial formats", {
  skip_if_not_installed("duckdb")
  
  # Setup connection
  conn <- tryCatch(ddbs_default_conn(), error = function(e) DBI::dbConnect(duckdb::duckdb()))
  ddbs_install(conn, quiet = TRUE)
  ddbs_load(conn, quiet = TRUE)
  
  # Use existing GeoJSON
  gj_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  expect_true(file.exists(gj_path))
  
  # 1. Test basic auto-detection with extension
  res_gj <- duckspatial::ddbs_open_dataset(gj_path, conn = conn)
  expect_true(inherits(res_gj, "duckspatial_df"))
  expect_gt(as.numeric(dplyr::count(res_gj) |> dplyr::collect()), 0)
  
  # 2. Test auto-detection with GPKG
  # Create a small GPKG from scratch to avoid schema issues
  tmp_gpkg <- tempfile(fileext = ".gpkg")
  
  # Simple polygon
  sf_obj <- sf::st_as_sf(data.frame(id = 1, geom = sf::st_sfc(sf::st_point(c(0,0)))), crs = 4326)
  sf::st_write(sf_obj, tmp_gpkg, quiet = TRUE)
  
  res_gpkg <- duckspatial::ddbs_open_dataset(tmp_gpkg, conn = conn)
  expect_true(inherits(res_gpkg, "duckspatial_df"))
  
  # 3. Test auto-detection WITHOUT extension (rename file)
  tmp_no_ext <- tempfile() # no extension
  file.copy(tmp_gpkg, tmp_no_ext)
  
  # This is the critical test: can GDAL/DuckDB detect format without extension?
  res_no_ext <- duckspatial::ddbs_open_dataset(tmp_no_ext, conn = conn)
  expect_true(inherits(res_no_ext, "duckspatial_df"))
  
  # 4. Test Parquet auto-detection without extension (the new path)
  tmp_pq <- tempfile(fileext = ".parquet")
  # Simple dataframe - ddbs_open_dataset just needs to be able to open it
  # We add a dummy geometry column to satisfy potential column checks if any (though lazy)
  df_pq <- data.frame(a = 1L)
  # Binary column simulating geometryWKB (just raw bytes)
  df_pq$geometry <- I(list(as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00))))
  arrow::write_parquet(df_pq, tmp_pq)
  
  tmp_pq_no_ext <- tempfile()
  file.copy(tmp_pq, tmp_pq_no_ext)
  
  res_pq_no_ext <- duckspatial::ddbs_open_dataset(tmp_pq_no_ext, conn = conn)
  expect_true(inherits(res_pq_no_ext, "duckspatial_df"))
  
})

test_that("ddbs_open_dataset gives clear error for unsupported file types", {
  skip_if_not_installed("duckdb")
  
  conn <- tryCatch(ddbs_default_conn(), error = function(e) DBI::dbConnect(duckdb::duckdb()))
  ddbs_install(conn, quiet = TRUE)
  ddbs_load(conn, quiet = TRUE)
  
  # Create an unsupported file (plain text)
  tmp_garbage <- tempfile(fileext = ".txt")
  writeLines("This is not spatial data", tmp_garbage)
  
  # Expect an informative error and NO CRS warnings
  expect_warning(
    expect_error(
      duckspatial::ddbs_open_dataset(tmp_garbage, conn = conn),
      regexp = "not recognized|not supported|failed",
      ignore.case = TRUE
    ),
    NA # NA means "no warnings"
  )
  
  # Test with no extension garbage file
  tmp_garbage_no_ext <- tempfile()
  writeLines("Random garbage content", tmp_garbage_no_ext)
  
  expect_warning(
    expect_error(
      duckspatial::ddbs_open_dataset(tmp_garbage_no_ext, conn = conn),
      regexp = "not recognized|not supported|failed|format",
      ignore.case = TRUE
    ),
    NA
  )
})

test_that("ddbs_open_dataset warns when file has no CRS", {
  skip_if_not_installed("duckdb")
  
  conn <- tryCatch(ddbs_default_conn(), error = function(e) DBI::dbConnect(duckdb::duckdb()))
  ddbs_install(conn, quiet = TRUE)
  ddbs_load(conn, quiet = TRUE)

  # Create a shapefile without CRS
  tmp_dir <- tempdir()
  tmp_shp <- file.path(tmp_dir, "no_crs_autodetect_test.shp")
  
  # Create points without CRS
  pts <- sf::st_as_sf(
    data.frame(x = 1:3, y = 1:3, id = 1:3),
    coords = c("x", "y"),
    crs = NA
  )
  suppressWarnings(sf::st_write(pts, tmp_shp, quiet = TRUE, delete_dsn = TRUE))
  on.exit(unlink(list.files(tmp_dir, pattern = "no_crs_autodetect_test", full.names = TRUE)), add = TRUE)

  # Should warn about missing CRS
  expect_warning(
    ds <- duckspatial::ddbs_open_dataset(tmp_shp, conn = conn),
    regexp = "Could not auto-detect CRS",
    ignore.case = TRUE
  )

  # CRS should be NA
  expect_true(is.na(sf::st_crs(attr(ds, "crs"))$epsg))
})

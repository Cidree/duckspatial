# Comprehensive validation tests for ddbs_write_dataset

test_that("warns when extension and driver mismatch", {
  conn <- ddbs_temp_conn()
  ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"), conn = conn)
  
  tmp_file <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp_file), add = TRUE)
  
  # .geojson with ESRI Shapefile driver should warn
  expect_warning(
    ddbs_write_dataset(ds, tmp_file, gdal_driver = "ESRI Shapefile", quiet = TRUE),
    "Extension/driver mismatch"
  )
  
  # Should still create the file
  expect_true(file.exists(tmp_file))
})

test_that("warns with specific extension and driver details in mismatch", {
  conn <- ddbs_temp_conn()
  ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"), conn = conn)
  
  tmp_file <- tempfile(fileext = ".shp")
  on.exit(unlink(tmp_file), add = TRUE)
  
  # .shp maps to "ESRI Shapefile", but we specify "GeoJSON"
  expect_warning(
    ddbs_write_dataset(ds, tmp_file, gdal_driver = "GeoJSON", quiet = TRUE),
    "File extension.*\\.shp.*ESRI Shapefile"
  )
  
  # Verify mentions GeoJSON and .geojson
  tmp_file2 <- tempfile(fileext = ".shp")
  on.exit(unlink(tmp_file2), add = TRUE)
  
  expect_warning(
    ddbs_write_dataset(ds, tmp_file2, gdal_driver = "GeoJSON", quiet = TRUE),
    "You specified driver.*GeoJSON.*\\.geojson"
  )
})

test_that("errors on unknown extension without gdal_driver", {
  conn <- ddbs_temp_conn()
  ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"), conn = conn)
  
  expect_error(
    ddbs_write_dataset(ds, "output.xyz"),
    "Cannot determine GDAL driver"
  )
  
  # Error should suggest using gdal_driver
  expect_error(
    ddbs_write_dataset(ds, "output.xyz"),
    "gdal_driver"
  )
  
  # Error should suggest ddbs_drivers()
  expect_error(
    ddbs_write_dataset(ds, "output.xyz"),
    "ddbs_drivers"
  )
})

test_that("works with unknown extension when gdal_driver provided", {
  conn <- ddbs_temp_conn()
  ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"), conn = conn)
  
  tmp_file <- tempfile(fileext = ".xyz")
  on.exit(unlink(tmp_file), add = TRUE)
  
  expect_no_error(
    ddbs_write_dataset(ds, tmp_file, gdal_driver = "GeoJSON", quiet = TRUE)
  )
  expect_true(file.exists(tmp_file))
  
  # Verify it's actually GeoJSON by reading it back
  ds_back <- ddbs_open_dataset(tmp_file, conn = conn)
  expect_equal(dplyr::count(ds_back) |> dplyr::pull(n), 257)
})

test_that("errors with helpful message for invalid driver", {
  conn <- ddbs_temp_conn()
  ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"), conn = conn)
  
  tmp_file <- tempfile(fileext = ".geojson")
  
  expect_error(
    ddbs_write_dataset(ds, tmp_file, gdal_driver = "NonExistentDriver", quiet = TRUE),
    "is not available on this system"
  )
  
  # Should suggest available drivers
  expect_error(
    ddbs_write_dataset(ds, tmp_file, gdal_driver = "NonExistentDriver", quiet = TRUE),
    "Available writable drivers"
  )
  
  # Should mention ddbs_drivers()
  expect_error(
    ddbs_write_dataset(ds, tmp_file, gdal_driver = "NonExistentDriver", quiet = TRUE),
    "ddbs_drivers"
  )
})

test_that("auto-detects from standard extensions", {
  conn <- ddbs_temp_conn()
  ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"), conn = conn)
  
  # Test various extensions
  extensions <- c("geojson", "shp", "fgb")
  
  for (ext in extensions) {
    tmp_file <- tempfile(fileext = paste0(".", ext))
    on.exit(unlink(tmp_file), add = TRUE)
    
    # Should work without specifying gdal_driver
    expect_no_error(
      ddbs_write_dataset(ds, tmp_file, quiet = TRUE)
    )
    expect_true(file.exists(tmp_file))
  }
})

test_that("handles native formats without GDAL validation", {
  conn <- ddbs_temp_conn()
  ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"), conn = conn)
  
  # Parquet
  tmp_parquet <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp_parquet), add = TRUE)
  expect_no_error(ddbs_write_dataset(ds, tmp_parquet, quiet = TRUE))
  expect_true(file.exists(tmp_parquet))
  
  # CSV
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  expect_no_error(ddbs_write_dataset(ds, tmp_csv, quiet = TRUE))
  expect_true(file.exists(tmp_csv))
})

test_that("no warning when extension and driver match", {
  conn <- ddbs_temp_conn()
  ds <- ddbs_open_dataset(system.file("spatial/countries.geojson", package = "duckspatial"), conn = conn)
  
  tmp_file <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp_file), add = TRUE)
  
  # .geojson with GeoJSON driver should not warn
  expect_no_warning(
    ddbs_write_dataset(ds, tmp_file, gdal_driver = "GeoJSON", quiet = TRUE)
  )
  expect_true(file.exists(tmp_file))
})

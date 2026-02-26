test_that("ddbs_create_temp_spatial_file creates file and cleans up automatically", {
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)
  
  # Track the temp directory
  temp_path <- NULL
  temp_file_path <- NULL
  
  # Use a local scope to trigger cleanup
  local({
    temp_file_path <<- ddbs_create_temp_spatial_file(ds, ext = "geojson", conn = conn)
    temp_path <<- dirname(temp_file_path)
    
    # File should exist within the function scope
    expect_true(file.exists(temp_file_path))
    expect_true(dir.exists(temp_path))
    
    # Verify it's actually geojson by reading it back
    ds_back <- ddbs_open_dataset(temp_file_path, conn = conn)
    expect_equal(dplyr::count(ds_back) |> dplyr::pull(n), 257)
  })
  
  # After exiting scope, directory should be cleaned up
  expect_false(dir.exists(temp_path))
})

test_that("ddbs_create_temp_spatial_file works with shapefile format", {
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)
  
  temp_path <- NULL
  
  local({
    # Shapefiles create multiple files, so directory cleanup is crucial
    temp_file <- ddbs_create_temp_spatial_file(ds, ext = "shp", conn = conn)
    temp_path <<- dirname(temp_file)
    
    expect_true(file.exists(temp_file))
    
    # Shapefiles create .shp, .shx, .dbf, etc.
    # Check that multiple files exist in the directory
    all_files <- list.files(temp_path, recursive = TRUE)
    expect_gt(length(all_files), 1) # Should have multiple associated files
  })
  
  # Cleanup should remove all shapefile components
  expect_false(dir.exists(temp_path))
})

test_that("ddbs_create_temp_spatial_file passes through additional arguments", {
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)
  
  local({
    # Test passing crs argument through
    temp_file <- ddbs_create_temp_spatial_file(
      ds, 
      ext = "geojson", 
      conn = conn,
      crs = "EPSG:3857"
    )
    
    expect_true(file.exists(temp_file))
    
    # Verify CRS was set
    sf_obj <- sf::st_read(temp_file, quiet = TRUE)
    expect_equal(sf::st_crs(sf_obj)$epsg, 3857)
  })
})


test_that("GeoPackage output automatically renames 'FID' to 'FID_original'", {
  conn <- ddbs_temp_conn()
  
  # Create a simple sf object with an 'FID' column
  df <- data.frame(
    FID = 1:5,
    name = letters[1:5],
    lat = 0,
    lon = 0
  )
  sf_obj <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # Test 1: Writing local sf object
  # Should trigger warning and rename
  tmp_local <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp_local), add = TRUE)
  
  expect_message(
    ddbs_write_dataset(sf_obj, tmp_local, quiet = TRUE),
    "Column 'FID' renamed to 'FID_original'"
  )
  
  # Verify local write result has 'FID_original'
  ds_local <- ddbs_open_dataset(tmp_local, conn = conn)
  cols_local <- glue::glue("DESCRIBE SELECT * FROM {attr(ds_local, 'source_table')}") |> 
    DBI::dbGetQuery(conn, statement = _)
  
  # Print names for debugging if needed
  # print(cols_local$column_name)
  
  expect_true("FID_original" %in% cols_local$column_name)
  # GDAL primary key might be 'fid', 'geom', etc. We mainly care that our data is preserved.
  
  # Test 2: Writing remote (lazy) object
  # First create a table in DuckDB with FID
  DBI::dbWriteTable(conn, "test_fid_source", df, overwrite = TRUE)
  ddbs_load(conn) # Ensure spatial
  DBI::dbExecute(conn, "CREATE OR REPLACE TABLE test_fid_spatial AS SELECT *, ST_Point(lon, lat) as geom FROM test_fid_source")
  
  remote_tbl <- dplyr::tbl(conn, "test_fid_spatial")
  
  tmp_remote <- tempfile(fileext = ".gpkg")
  on.exit(unlink(tmp_remote), add = TRUE)
  
  # Provide CRS explicitly to avoid warnings and ensure correct metadata
  expect_message(
    ddbs_write_dataset(remote_tbl, tmp_remote, crs = "EPSG:4326", quiet = TRUE),
    "Column 'FID' renamed to 'FID_original'"
  )
  
  # Verify remote write result
  ds_remote <- ddbs_open_dataset(tmp_remote, conn = conn)
  cols_remote <- glue::glue("DESCRIBE SELECT * FROM {attr(ds_remote, 'source_table')}") |> 
    DBI::dbGetQuery(conn, statement = _)
    
  expect_true("FID_original" %in% cols_remote$column_name)
})

test_that("No warning for non-GeoPackage formats with FID column", {
  conn <- ddbs_temp_conn()
  
  df <- data.frame(FID = 1:5, lat = 0, lon = 0)
  sf_obj <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # GeoJSON shouldn't care about FID column
  tmp_json <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp_json), add = TRUE)
  
  expect_no_message(
    ddbs_write_dataset(sf_obj, tmp_json, quiet = TRUE)
  )
  expect_true(file.exists(tmp_json))
})

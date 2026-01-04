test_that("ddbs_write_dataset works for Parquet", {
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)
  
  tmp_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp_file), add = TRUE)
  expect_no_error(ddbs_write_dataset(ds, tmp_file, quiet = TRUE))
  expect_true(file.exists(tmp_file))
  
  # Verify reading back
  ds_back <- ddbs_open_dataset(tmp_file, conn = conn)
  expect_equal(dplyr::count(ds_back) |> dplyr::pull(n), 257) # Expect same count
})

test_that("ddbs_write_dataset works for GeoJSON (GDAL)", {
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)
  
  tmp_file <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp_file), add = TRUE)
  expect_no_error(ddbs_write_dataset(ds, tmp_file, quiet = TRUE))
  expect_true(file.exists(tmp_file))
  
  # Verify reading back
  ds_back <- ddbs_open_dataset(tmp_file, conn = conn)
  expect_equal(dplyr::count(ds_back) |> dplyr::pull(n), 257)
})


test_that("ddbs_write_dataset overwrite behavior", {
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)
  
  tmp_file <- tempfile(fileext = ".parquet")
  file.create(tmp_file) # Create empty file
  on.exit(unlink(tmp_file), add = TRUE)
  
  # Ensure file exists to genuinely test overwrite check
  expect_true(file.exists(tmp_file))
  
  # Should fail by default
  expect_error(ddbs_write_dataset(ds, tmp_file), "already exists")
  
  # Should succeed with overwrite=TRUE
  expect_no_error(ddbs_write_dataset(ds, tmp_file, overwrite = TRUE, quiet = TRUE))
  expect_gt(file.size(tmp_file), 0)
})

test_that("ddbs_write_dataset partitioning (Parquet)", {
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)
  
  # Add a dummy partition column
  ds_mod <- ds |> dplyr::mutate(part_col = dplyr::if_else(CNTR_NAME == 'Argentina', "AR", "Others"))
  
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  tmp_file <- file.path(tmp_dir, "partitioned_countries.parquet")
  
  expect_no_error(
    ddbs_write_dataset(ds_mod, tmp_file, partitioning = "part_col", quiet = TRUE)
  )
  
  # Check directory structure
  # print(list.files(tmp_dir, recursive = TRUE))
  # print(list.dirs(tmp_dir))
  
  # Check if directory structure exists (Hive style)
  # Directories are created INSIDE the target path
  expect_true(dir.exists(file.path(tmp_file, "part_col=AR")))
  expect_true(dir.exists(file.path(tmp_file, "part_col=Others")))
})

test_that("ddbs_write_dataset auto-detects partitioning from grouped data", {
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  conn <- ddbs_temp_conn()
  
  ds <- ddbs_open_dataset(path, conn = conn)
  
  # Group the lazy table
  ds_grouped <- ds |> dplyr::group_by(CNTR_ID)
  
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  tmp_file <- file.path(tmp_dir, "auto_partitioned.parquet")
  
  # Should use CNTR_ID for partitioning automatically
  expect_no_error(
    ddbs_write_dataset(ds_grouped, tmp_file, quiet = TRUE)
  )
  
  # Verify some partitions exist (CNTR_ID are 2-char codes)
  # Argentina is AR
  expect_true(dir.exists(file.path(tmp_file, "CNTR_ID=AR")))
})

test_that("ddbs_write_dataset validates non-spatial data for spatial format", {
  conn <- ddbs_temp_conn()
  df <- data.frame(a = 1:5, b = letters[1:5])
  
  tmp_file <- tempfile(fileext = ".geojson")
  
  expect_error(
    ddbs_write_dataset(df, tmp_file, format = "geojson", quiet = TRUE),
    "Input local data must be an 'sf' object"
  )
})

test_that("ddbs_write_dataset fails with plain local data.frame", {
  conn <- ddbs_temp_conn()
  
  df <- mtcars
  df$geometry <- "POLYGON((0 0, 1 1, 1 0, 0 0))" # Fake geometry column
  tmp_file <- tempfile(fileext = ".parquet")
  
  # Should fail because it's not an sf object
  expect_error(
    ddbs_write_dataset(df, tmp_file, quiet = TRUE),
    "Input local data must be an 'sf' object"
  )
})

test_that("ddbs_write_dataset fails with fake spatial DuckDB table (wrong type)", {
  conn <- ddbs_temp_conn()
  
  # Create a table with a column named 'geometry' but it's VARCHAR
  DBI::dbExecute(conn, "CREATE OR REPLACE TABLE fake_spatial AS SELECT 'POINT(0 0)'::VARCHAR AS geometry")
  data <- dplyr::tbl(conn, "fake_spatial")
  
  tmp_file <- tempfile(fileext = ".parquet")
  
  # Should fail because type is VARCHAR, not GEOMETRY
  expect_error(
    ddbs_write_dataset(data, tmp_file, quiet = TRUE),
    "does not contain a spatial column of type 'GEOMETRY'"
  )
})

test_that("ddbs_write_dataset CRS override works", {
  skip_if_not_installed("sf")
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)
  
  tmp_file <- tempfile(fileext = ".geojson")
  
  # Original is likely 4326. Let's try to override metadata to something else (e.g. 3857)
  # Note: SRS option in writes sets metadata, doesn't transform geometry usually (per GDAL driver)
  expect_no_error(
    ddbs_write_dataset(ds, tmp_file, crs = "EPSG:3857", quiet = TRUE)
  )
  
  # Use sf to verify CRS metadata
  sf_obj <- sf::st_read(tmp_file, quiet = TRUE)
  expect_equal(sf::st_crs(sf_obj)$epsg, 3857)
})

test_that("ddbs_write_dataset works with local sf object", {
  skip_if_not_installed("sf")
  conn <- ddbs_temp_conn()
  
  # Create simple SF
  poly <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE))))
  sf_obj <- sf::st_sf(id = 1, geometry = poly, crs = 4326)
  
  tmp_file <- tempfile(fileext = ".parquet")
  
  expect_no_error(ddbs_write_dataset(sf_obj, tmp_file, conn = conn, quiet = TRUE))
  
  # Read back
  ds_back <- ddbs_open_dataset(tmp_file, conn = conn)
  expect_equal(dplyr::count(ds_back) |> dplyr::pull(n), 1)
})

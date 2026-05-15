
testthat::skip_on_cran()

parquet_geo_crs_metadata <- function(conn, path) {
  path_sql <- as.character(DBI::dbQuoteString(conn, path))
  query <- paste0(
    "SELECT decode(value) AS geo ",
    "FROM parquet_kv_metadata(", path_sql, ") ",
    "WHERE decode(key) = 'geo' ",
    "LIMIT 1"
  )
  res <- DBI::dbGetQuery(conn, query)
  if (nrow(res) == 0) {
    return(NA_character_)
  }

  res$geo[[1]]
}

sf_read_parquet_or_null <- function(path) {
  tryCatch(
    sf::st_read(path, quiet = TRUE),
    error = function(e) NULL
  )
}

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
    ddbs_write_dataset(df, tmp_file, gdal_driver = "GeoJSON", quiet = TRUE),
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

  # Use helper with CRS override - helper passes through kwargs
  tmp_file <- ddbs_create_temp_spatial_file(ds, ext = "geojson", conn = conn, crs = "EPSG:3857")

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
  on.exit(unlink(tmp_file), add = TRUE)

  expect_no_error(ddbs_write_dataset(sf_obj, tmp_file, conn = conn, quiet = TRUE))

  # Read back
  ds_back <- ddbs_open_dataset(tmp_file, conn = conn)
  expect_equal(dplyr::count(ds_back) |> dplyr::pull(n), 1)
  expect_equal(ddbs_crs(ds_back)$epsg, 4326)

  sf_back <- sf_read_parquet_or_null(tmp_file)
  if (!is.null(sf_back)) {
    expect_equal(sf::st_crs(sf_back)$epsg, 4326)
  }
})

test_that("ddbs_write_dataset preserves local sf EPSG CRS in GeoParquet metadata", {
  skip_if_not_installed("sf")
  conn <- ddbs_temp_conn()

  coords <- matrix(
    c(0, 0, 1000000, 0, 1000000, 1000000, 0, 1000000, 0, 0),
    ncol = 2,
    byrow = TRUE
  )
  poly <- sf::st_sfc(sf::st_polygon(list(coords)), crs = 3857)
  sf_obj <- sf::st_sf(id = 1, geometry = poly)

  tmp_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp_file), add = TRUE)

  expect_no_error(ddbs_write_dataset(sf_obj, tmp_file, conn = conn, quiet = TRUE))

  crs_meta <- parquet_geo_crs_metadata(conn, tmp_file)
  expect_match(crs_meta, '"authority"\\s*:\\s*"EPSG"')
  expect_match(crs_meta, '"code"\\s*:\\s*"?3857"?')

  sf_back <- sf_read_parquet_or_null(tmp_file)
  if (!is.null(sf_back)) {
    expect_equal(sf::st_crs(sf_back)$epsg, 3857)
    expect_gt(unname(sf::st_bbox(sf_back)[["xmax"]]), 100000)
  }

  ds_back <- ddbs_open_dataset(tmp_file, conn = conn)
  expect_equal(ddbs_crs(ds_back)$epsg, 3857)
})

test_that("ddbs_write_dataset does not invent EPSG CRS metadata for local sf without CRS", {
  skip_if_not_installed("sf")
  conn <- ddbs_temp_conn()

  poly <- sf::st_sfc(sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE))))
  sf_obj <- sf::st_sf(id = 1, geometry = poly)

  tmp_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp_file), add = TRUE)

  expect_no_error(ddbs_write_dataset(sf_obj, tmp_file, conn = conn, quiet = TRUE))

  crs_meta <- parquet_geo_crs_metadata(conn, tmp_file)
  crs_text <- if (is.na(crs_meta)) "" else crs_meta
  expect_false(grepl("EPSG", crs_text, fixed = TRUE))
})

test_that("ddbs_write_dataset preserves local sf non-EPSG CRS in GeoParquet metadata", {
  skip_if_not_installed("sf")
  conn <- ddbs_temp_conn()

  crs <- sf::st_crs("+proj=laea +lat_0=52 +lon_0=10 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  expect_true(is.na(crs$epsg))

  poly <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol=2, byrow=TRUE))),
    crs = crs
  )
  sf_obj <- sf::st_sf(id = 1, geometry = poly)

  tmp_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp_file), add = TRUE)

  expect_no_warning(ddbs_write_dataset(sf_obj, tmp_file, conn = conn, quiet = TRUE))
  expect_true(file.exists(tmp_file))

  crs_meta <- parquet_geo_crs_metadata(conn, tmp_file)
  expect_false(is.na(crs_meta))
  expect_match(crs_meta, '"crs"\\s*:\\s*\\{')
  expect_match(crs_meta, '"type"\\s*:\\s*"ProjectedCRS"')

  path_sql <- as.character(DBI::dbQuoteString(conn, tmp_file))
  desc <- DBI::dbGetQuery(conn, paste0("DESCRIBE SELECT * FROM read_parquet(", path_sql, ")"))
  geom_type <- desc$column_type[desc$column_name == attr(sf_obj, "sf_column")]
  expect_true(grepl("^GEOMETRY\\('", geom_type))
  expect_true(grepl("ProjectedCRS", geom_type, fixed = TRUE))

  ds_back <- ddbs_open_dataset(tmp_file, conn = conn)
  ds_crs <- ddbs_crs(ds_back)
  expect_false(is.na(ds_crs))
  expect_true(is.na(ds_crs$epsg))

  sf_back <- sf_read_parquet_or_null(tmp_file)
  if (!is.null(sf_back)) {
    sf_crs <- sf::st_crs(sf_back)
    expect_false(is.na(sf_crs))
    expect_true(is.na(sf_crs$epsg))
  }
})

# Driver validation tests

test_that("ddbs_write_dataset validates invalid driver", {
  conn <- ddbs_temp_conn()
  path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(path, conn = conn)

  tmp_file <- tempfile(fileext = ".fake")

  # Should error when specifying non-existent driver
  expect_error(
    ddbs_write_dataset(ds, tmp_file, gdal_driver = "NonExistentDriver", quiet = TRUE),
    "is not available on this system"
  )
})

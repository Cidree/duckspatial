# Tests for ddbs_open_dataset following the ssf_read testing pattern
# Uses internal package datasets (countries, argentina, rivers) from setup.R

# =============================================================================
# Format-specific tests
# =============================================================================

test_that("ddbs_open_dataset works with GeoJSON", {
  conn <- ddbs_temp_conn()

  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(countries_path, conn = conn)

  expect_s3_class(ds, "duckspatial_df")

  # Verify row count via SQL (countries.geojson has 257 countries)
  view_name <- attr(ds, "source_table")
  res <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) FROM %s", view_name))
  expect_equal(as.numeric(res[[1]]), 257)

  # Verify CRS detection (countries is WGS84 EPSG:4326)
  detected_crs <- attr(ds, "crs")
  expect_equal(sf::st_crs(detected_crs)$epsg, 4326)

  # Verify geometry is valid
  res_geom <- DBI::dbGetQuery(
    conn,
    sprintf("SELECT ST_AsText(geom) FROM %s LIMIT 1", view_name)
  )
  expect_true(nrow(res_geom) == 1)
  expect_true(grepl("POLYGON", res_geom[[1]]))
})

test_that("ddbs_open_dataset works with GeoPackage", {
  conn <- ddbs_temp_conn()

  # Create temp GeoPackage
  tmp_gpkg <- ddbs_create_temp_spatial_file(
    countries_sf,
    ext = "gpkg",
    conn = conn
  )

  ds <- ddbs_open_dataset(tmp_gpkg, conn = conn)

  expect_s3_class(ds, "duckspatial_df")

  # Verify row count
  view_name <- attr(ds, "source_table")
  res <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) FROM %s", view_name))
  expect_equal(as.numeric(res[[1]]), nrow(countries_sf))

  # Verify CRS detection (countries is WGS84 EPSG:4326)
  detected_crs <- attr(ds, "crs")
  expect_equal(sf::st_crs(detected_crs)$epsg, 4326)
})

test_that("ddbs_open_dataset works with Parquet (GeoArrow)", {
  skip_if_not_installed("arrow")

  conn <- ddbs_temp_conn()

  # Create temp Parquet from internal data using helper
  tmp_parquet <- ddbs_create_temp_spatial_file(countries_sf, ext = "parquet", conn = conn)

  ds <- ddbs_open_dataset(tmp_parquet, conn = conn)

  expect_s3_class(ds, "duckspatial_df")

  # Verify row count
  view_name <- attr(ds, "source_table")
  res <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) FROM %s", view_name))
  expect_equal(as.numeric(res[[1]]), nrow(countries_sf))

  # Check geometry column detection
  expect_equal(attr(ds, "sf_column"), "geometry")
})

test_that("ddbs_open_dataset works with Shapefile", {
  conn <- ddbs_temp_conn()

  # Create temp shapefile from internal data using helper
  tmp_shp <- ddbs_create_temp_spatial_file(rivers_sf, ext = "shp", conn = conn)
  expect_true(file.exists(tmp_shp))

  ds <- ddbs_open_dataset(tmp_shp, conn = conn)

  expect_s3_class(ds, "duckspatial_df")

  # Verify row count (rivers_sf has 100 features)
  view_name <- attr(ds, "source_table")
  res <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) FROM %s", view_name))
  expect_equal(as.numeric(res[[1]]), nrow(rivers_sf))

  # Verify CRS detection (rivers is EPSG:3035)
  detected_crs <- attr(ds, "crs")
  expect_equal(sf::st_crs(detected_crs)$epsg, 3035)

  # Verify geometry is valid (rivers are linestrings)
  res_geom <- DBI::dbGetQuery(
    conn,
    sprintf("SELECT ST_AsText(geom) FROM %s LIMIT 1", view_name)
  )
  expect_true(nrow(res_geom) == 1)
  expect_true(grepl("LINESTRING", res_geom[[1]]))
})

# =============================================================================
# Dedicated reader dispatch tests
# =============================================================================

test_that("ddbs_open_dataset dispatches to ST_ReadSHP vs GDAL correctly", {
  conn <- ddbs_temp_conn()

  # Create temp shapefile using helper
  tmp_shp <- ddbs_create_temp_spatial_file(argentina_sf, ext = "shp", conn = conn)
  expect_true(file.exists(tmp_shp))

  # Default mode: ST_ReadSHP
  ds_shp <- ddbs_open_dataset(tmp_shp, conn = conn)
  expect_s3_class(ds_shp, "duckspatial_df")

  view_sql_shp <- DBI::dbGetQuery(
    conn,
    glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_shp, 'source_table')}'")
  )$sql
  expect_true(grepl("st_readshp", view_sql_shp, ignore.case = TRUE))

  # Explicit GDAL mode
  ds_gdal <- ddbs_open_dataset(tmp_shp, conn = conn, read_shp_mode = "GDAL")
  expect_s3_class(ds_gdal, "duckspatial_df")

  view_sql_gdal <- DBI::dbGetQuery(
    conn,
    glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_gdal, 'source_table')}'")
  )$sql
  expect_true(grepl("st_read", view_sql_gdal, ignore.case = TRUE))
  expect_false(grepl("st_readshp", view_sql_gdal, ignore.case = TRUE))

  # Data integrity: counts should match
  expect_equal(
    ds_shp |> dplyr::count() |> dplyr::collect() |> dplyr::pull(n),
    ds_gdal |> dplyr::count() |> dplyr::collect() |> dplyr::pull(n)
  )
})

test_that("ddbs_open_dataset handles shp_encoding argument", {
  conn <- ddbs_temp_conn()

  # Create temp shapefile using helper
  tmp_shp <- ddbs_create_temp_spatial_file(argentina_sf, ext = "shp", conn = conn)
  expect_true(file.exists(tmp_shp))

  ds_enc <- ddbs_open_dataset(tmp_shp, conn = conn, shp_encoding = "UTF-8")

  expect_s3_class(ds_enc, "duckspatial_df")

  view_sql_enc <- DBI::dbGetQuery(
    conn,
    glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_enc, 'source_table')}'")
  )$sql
  expect_true(grepl("encoding.*UTF-8", view_sql_enc, ignore.case = TRUE))
})

test_that("ddbs_open_dataset OSM mode dispatch", {
  conn <- ddbs_temp_conn()

  # GDAL mode (default) - uses dummy file that doesn't exist.
  # Since our logic now correctly dispatches to ST_Read (GDAL), and we have strict error handling,
  # this should error "Unable to open file" because ST_Read validates existence.
  expect_error(
    ddbs_open_dataset("dummy.osm.pbf", conn = conn, read_osm_mode = "GDAL"),
    "Unable to open file"
  )

  # ST_ReadOSM mode
  # This path is lazy/permissive and might not error on open, allowing us to inspect SQL.
  ds_osm_read <- ddbs_open_dataset("dummy.osm.pbf", conn = conn, read_osm_mode = "ST_ReadOSM")
  view_sql_osm_read <- DBI::dbGetQuery(
    conn,
    glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_osm_read, 'source_table')}'")
  )$sql
  expect_true(grepl("st_readosm", view_sql_osm_read, ignore.case = TRUE))
})

# =============================================================================
# Argument validation and warnings
# =============================================================================

test_that("ddbs_open_dataset warns when ST_Read args are passed to Parquet", {
  skip_if_not_installed("arrow")

  conn <- ddbs_temp_conn()

  tmp_parquet <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(x = 1, y = 2), tmp_parquet)
  on.exit(unlink(tmp_parquet), add = TRUE)

  expect_warning(
    ddbs_open_dataset(tmp_parquet, conn = conn, layer = "foo"),
    "Arguments specific to ST_Read .* are ignored"
  )
})

test_that("ddbs_open_dataset detects format correctly with GDAL options", {
  conn <- ddbs_temp_conn()

  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")

  ds <- ddbs_open_dataset(
    countries_path,
    conn = conn,
    gdal_open_options = c("FLATTEN_NESTED_ATTRIBUTES=YES")
  )

  expect_s3_class(ds, "duckspatial_df")
  res <- collect(ds)
  expect_true(nrow(res) > 0)
})


# =============================================================================
# CRS and geometry handling
# =============================================================================

test_that("ddbs_open_dataset handles explicit CRS override", {
  conn <- ddbs_temp_conn()

  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")

  # countries.geojson is EPSG:4326, override to 3857
  ds <- ddbs_open_dataset(countries_path, crs = 3857, conn = conn)

  expect_s3_class(ds, "duckspatial_df")

  # Verify CRS is the overridden value (3857), not the original (4326)
  detected_crs <- attr(ds, "crs")
  expect_equal(sf::st_crs(detected_crs)$epsg, 3857)
})

test_that("ddbs_open_dataset with custom geom_col", {
  conn <- ddbs_temp_conn()

  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")

  ds <- ddbs_open_dataset(countries_path, geom_col = "geom", conn = conn)

  expect_s3_class(ds, "duckspatial_df")
  expect_equal(attr(ds, "sf_column"), "geom")
})

# =============================================================================
# Error handling
# =============================================================================

test_that("ddbs_open_dataset handles missing file gracefully", {
  conn <- ddbs_temp_conn()

  expect_error(
    ddbs_open_dataset("/path/to/nonexistent/file.geojson", conn = conn),
    regexp = "Unable to open file",
    ignore.case = TRUE
  )
})

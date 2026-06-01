# Tests for ddbs_open_dataset following the ssf_read testing pattern
# Uses internal package datasets (countries, argentina, rivers) from setup.R

# =============================================================================
# Format-specific tests
# =============================================================================
testthat::skip_on_cran()

expect_duckdb_st_crs <- function(ds, conn) {
  view_name <- attr(ds, "source_table")
  geom_col <- attr(ds, "sf_column")
  q_geom <- as.character(DBI::dbQuoteIdentifier(conn, geom_col))

  res <- DBI::dbGetQuery(
    conn,
    glue::glue(
      "SELECT ST_CRS({q_geom}) AS crs ",
      "FROM {view_name} ",
      "WHERE {q_geom} IS NOT NULL ",
      "LIMIT 1"
    )
  )

  expect_equal(nrow(res), 1)
  expect_false(is.na(res$crs[[1]]))
  expect_false(identical(res$crs[[1]], ""))
  res$crs[[1]]
}

test_that("ddbs_open_dataset works with GeoJSON", {
  conn <- ddbs_temp_conn()

  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds <- ddbs_open_dataset(countries_path, conn = conn)

  expect_s3_class(ds, "duckspatial_df")

  # Verify row count via SQL (countries.geojson has 257 countries)
  view_name <- attr(ds, "source_table")
  res <- DBI::dbGetQuery(conn, sprintf("SELECT count(*) FROM %s", view_name))
  expect_equal(as.numeric(res[[1]]), 257)

  # Verify CRS detection delegates to DuckDB when available
  duckdb_crs <- expect_duckdb_st_crs(ds, conn)
  expect_equal(attr(ds, "crs"), sf::st_crs(duckdb_crs))

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

  # Verify CRS detection delegates to DuckDB when available
  duckdb_crs <- expect_duckdb_st_crs(ds, conn)
  expect_equal(attr(ds, "crs"), sf::st_crs(duckdb_crs))
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

  # Verify CRS detection delegates to DuckDB when available
  duckdb_crs <- expect_duckdb_st_crs(ds, conn)
  expect_equal(attr(ds, "crs"), sf::st_crs(duckdb_crs))
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

test_that("ddbs_open_dataset detects CRS across generated and bundled formats", {
  skip_if_not_installed("arrow")

  conn <- ddbs_temp_conn()

  generated_geojson <- ddbs_create_temp_spatial_file(countries_sf, ext = "geojson", conn = conn)
  generated_geoparquet <- ddbs_create_temp_spatial_file(countries_sf, ext = "parquet", conn = conn)
  bundled_gpkg <- system.file("spatial/points.gpkg", package = "duckspatial")
  bundled_shp <- system.file("shape/nc.shp", package = "sf")

  generated_geojson_ds <- ddbs_open_dataset(generated_geojson, conn = conn)
  expect_equal(attr(generated_geojson_ds, "crs"), sf::st_crs(expect_duckdb_st_crs(generated_geojson_ds, conn)))

  generated_geoparquet_ds <- ddbs_open_dataset(generated_geoparquet, conn = conn)
  expect_equal(attr(generated_geoparquet_ds, "crs"), sf::st_crs(expect_duckdb_st_crs(generated_geoparquet_ds, conn)))
  expect_false(is.na(attr(generated_geoparquet_ds, "crs")))

  bundled_gpkg_ds <- ddbs_open_dataset(bundled_gpkg, conn = conn)
  expect_equal(attr(bundled_gpkg_ds, "crs"), sf::st_crs(expect_duckdb_st_crs(bundled_gpkg_ds, conn)))

  bundled_shp_ds <- ddbs_open_dataset(bundled_shp, conn = conn)
  expect_equal(sf::st_crs(attr(bundled_shp_ds, "crs"))$epsg, 4267)
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

  # view_sql_enc <- DBI::dbGetQuery(
  #   conn,
  #   glue::glue("SELECT sql FROM duckdb_tables() WHERE table_name = '{attr(ds_enc, 'source_table')}'")
  # )$sql
  # expect_true(grepl("encoding.*UTF-8", view_sql_enc, ignore.case = TRUE))
})

test_that("ddbs_open_dataset OSM mode dispatch", {

  conn <- ddbs_temp_conn()

  # GDAL mode (default) - uses dummy file that doesn't exist.
  # Since our logic now correctly dispatches to ST_Read (GDAL), and we have strict error handling,
  # this should error "Unable to open file" because ST_Read validates existence.
  expect_error(
    ddbs_open_dataset("dummy.osm.pbf", conn = conn, read_osm_mode = "GDAL"),
    # "Could not open GDAL"
    "File format not recognized"
  )
  
  # ST_ReadOSM mode
  # This path is lazy/permissive and might not error on open, allowing us to inspect SQL.
  # ST_ReadOSM mode
  # Note: ST_ReadOSM does not produce a spatial column DuckDB understands,
  # so it returns a regular table, not a duckspatial_df.
  ds_osm_read <- ddbs_open_dataset("dummy.osm.pbf", conn = conn, read_osm_mode = "ST_ReadOSM")
  expect_false(inherits(ds_osm_read, "duckspatial_df"))

  # The view name is stored in the remote_name for dbplyr objects
  view_name_osm <- as.character(dbplyr::remote_name(ds_osm_read))

  view_sql_osm_read <- DBI::dbGetQuery(
    conn,
    glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{view_name_osm}'")
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

test_that("ddbs_open_dataset fails gracefully on non-compliant GeoArrow structs", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("geoarrow")
  
  # Create a file with Arrow native encoding (NOT WKB) using direct write_parquet
  # This triggers the GeoArrow native struct encoding
  tmp_bad <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp_bad))
  arrow::write_parquet(countries_sf, tmp_bad)
  
  expect_error(ddbs_open_dataset(tmp_bad), "uses a native Arrow/GeoArrow struct encoding")
})

# =============================================================================
# DuckDB Native Format Support
# =============================================================================

test_that("ddbs_open_dataset validates DuckDB files properly", {
  expect_error(
    ddbs_open_dataset("/nonexistent_path.duckdb"),
    "does not exist"
  )

  tmp_empty_duck <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp_empty_duck), add = TRUE)
  file.create(tmp_empty_duck)
  expect_error(
    ddbs_open_dataset(tmp_empty_duck),
    "not a valid DuckDB database"
  )

  tmp_bad_duck <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp_bad_duck), add = TRUE)
  writeLines("this is just text", tmp_bad_duck)
  expect_error(
    ddbs_open_dataset(tmp_bad_duck),
    "not a valid DuckDB database"
  )

  # Use a unique file for the valid DuckDB test to avoid any interference
  # We use the established ddbs_temp_conn helper for clean connection management
  tmp_good_duck <- tempfile(fileext = ".duckdb")
  on.exit(unlink(tmp_good_duck), add = TRUE)

  local({
    conn <- ddbs_temp_conn(file = tmp_good_duck, cleanup = FALSE)
    ddbs_write_table(conn, countries_sf, "countries", quiet = TRUE)
  })

  expect_error(
    ddbs_open_dataset(tmp_good_duck),
    "layer"
  )

  expect_error(
    ddbs_open_dataset(tmp_good_duck, layer = "missing_table"),
    "not present"
  )

  ds <- ddbs_open_dataset(tmp_good_duck, layer = "countries", crs = 4326)
  expect_s3_class(ds, "duckspatial_df")
  expect_equal(as.character(dbplyr::remote_name(ds)), "countries")
  expect_equal(nrow(dplyr::collect(ds)), nrow(countries_sf))
})

test_that("ddbs_open_dataset opens supported DuckDB file extensions", {
  # Helper to create a fresh DB file for each extension to avoid lock issues
  create_duckdb_test_file <- function(ext) {
    db_path <- tempfile(fileext = ext)
    local({
      conn <- ddbs_temp_conn(file = db_path, cleanup = FALSE)
      ddbs_write_table(conn, countries_sf, "countries", quiet = TRUE)
    })
    db_path
  }

  expect_duckdb_open <- function(db_path) {
    on.exit(unlink(db_path), add = TRUE)
    ds <- ddbs_open_dataset(db_path, layer = "countries", crs = 4326)

    expect_s3_class(ds, "duckspatial_df")
    expect_equal(as.character(dbplyr::remote_name(ds)), "countries")
    expect_equal(nrow(dplyr::collect(ds)), nrow(countries_sf))
  }

  for (ext in c(".duckdb", ".db", ".ddb")) {
    expect_duckdb_open(create_duckdb_test_file(ext))
  }
})

test_that("ddbs_open_dataset does not open DuckDB files with unsupported extensions natively", {
  tmp_txt <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp_txt), add = TRUE)

  local({
    conn <- ddbs_temp_conn(file = tmp_txt, cleanup = FALSE)
    DBI::dbExecute(conn, "CREATE TABLE countries AS SELECT 1 AS value")
  })

  expect_error(
    ddbs_open_dataset(tmp_txt, layer = "countries"),
    regexp = "Unable to open file",
    ignore.case = TRUE
  )
})

test_that("ddbs_open_dataset lets non-DuckDB .db files fall through to ST_Read", {
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db), add = TRUE)
  writeLines("not a duckdb database", tmp_db)

  expect_error(
    ddbs_open_dataset(tmp_db),
    regexp = "Unable to open file",
    ignore.case = TRUE
  )
})

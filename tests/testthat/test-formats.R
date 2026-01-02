
# Auto-generated tests for ddbs_open_dataset extended formats

test_that("ddbs_open_dataset works with Parquet files (GeoParquet)", {
  skip_if_not_installed("arrow")
  # Need valid geoparquet file. 
  # We can create one using sf -> arrow/parquet if available, or skip if not easily creative.
  # For robustness, let's write a simple sf to parquet using arrow if installed.
  
  if (!requireNamespace("arrow", quietly = TRUE)) {
     skip("arrow package required for writing test parquet files")
  }
  
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  # Create sample data
  nc_path <- system.file("gpkg/nc.gpkg", package = "sf")
  # Need sf, arrow, and tibble
  
  # Use user-provided method to write geoparquet with metadata
  nc <- sf::read_sf(nc_path)
  
  # We need to simulate the user's environment or just write standard parquet
  # The user used: nc |> tibble::as_tibble() |> arrow::write_parquet(tf)
  # But specifically loaded 'geoarrow' library. Does that hook into write_parquet?
  # Or does 'tibble::as_tibble' on sf object keep geometry as list of WKB?
  # sf objects are data.frames. as_tibble keeps geometry column as class 'sfc'.
  # arrow::write_parquet handles sfc columns by converting to binary (WKB) and adding metadata if arrow >= ?
  
  # Let's try to match the user snippet exactly
  tf <- tempfile(fileext = ".parquet")
  
  # Ensure we have the libraries used
  if (!requireNamespace("arrow", quietly = TRUE)) skip("arrow not installed")
  # We don't necessarily have 'geoarrow' package installed in CI environment? 
  # Check if installed. User said "library(geoarrow)".
  # If not, we fall back to manual binary write which we know causes issues if we don't handle metadata/types right.
  
  # Just write as binary explicitly if geoarrow not present, or try write_parquet directly.
  # arrow write_parquet defaults: writes WKB binary.
  
  # User snippet:
  # nc |> tibble::as_tibble() |> write_parquet(tf)
  
  # We recreate this:
  arrow::write_parquet(tibble::as_tibble(nc), tf)
  
  # Test reading it back
  ds <- ddbs_open_dataset(tf, conn = conn)
  
  expect_s3_class(ds, "duckspatial_df")
  
  # Check if geometry column is correct
  expect_equal(attr(ds, "sf_column"), "geometry")
  
  # Collect and verify
  res <- collect(ds)
  
  # Since DuckDB returns STRUCT/LIST for this parquet file (GeoArrow native), 
  # and we don't convert it to WKB/Geometry automatically yet, 
  # collect returns a tibble with list-column for geometry. 
  # It is NOT an sf object because sf requires standard geometry types.
  
  expect_s3_class(res, "tbl_df")
  expect_true("geom" %in% names(res))
  expect_true(is.list(res$geom)) # Structs come back as lists
  
  # Cleanup
  unlink(tf)
})

test_that("ddbs_open_dataset detects format correctly and uses ST_Read for known extensions", {
   conn <- ddbs_create_conn()
   on.exit(ddbs_stop_conn(conn))
   
   # GeoJSON test (ST_Read path)
   countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")
   
   # Use an argument specific to ST_Read (e.g. layer or open_options?)
   # GeoJSON driver supports FLATTEN_NESTED_ATTRIBUTES
   ds <- ddbs_open_dataset(
      countries_path, 
      conn = conn, 
      gdal_open_options = c("FLATTEN_NESTED_ATTRIBUTES=YES")
   )
   
   expect_s3_class(ds, "duckspatial_df")
   res <- collect(ds)
   expect_true(nrow(res) > 0)
})

test_that("ddbs_open_dataset warns when ST_Read args are passed to Parquet", {
  skip_if_not_installed("arrow")
  
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  # Create dummy parquet
  tmp_parquet <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data.frame(x=1, y=2), tmp_parquet)
  
  expect_warning(
    ddbs_open_dataset(tmp_parquet, conn = conn, layer = "foo"),
    "Arguments specific to ST_Read .* are ignored"
  )
  
  unlink(tmp_parquet)
})

test_that("validate_driver_availability works", {
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  # Valid driver (Shapefile)
  expect_no_error(
    duckspatial:::validate_driver_availability("test.shp", conn)
  )
  
  # Unknown extension (should pass)
  expect_no_error(
    duckspatial:::validate_driver_availability("test.xyz", conn)
  )
})

test_that("dedicated readers dispatch works", {
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  # 1. SHAPEFILE
  nc_path <- system.file("shape/nc.shp", package = "sf")
  
  # Default mode: ST_ReadSHP
  ds_shp <- ddbs_open_dataset(nc_path, conn = conn)
  expect_s3_class(ds_shp, "duckspatial_df")
  
  # Verify it's using ST_ReadSHP via view SQL
  view_sql_shp <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_shp, 'source_table')}'"))$sql
  expect_true(grepl("st_readshp", view_sql_shp, ignore.case = TRUE))

  # Explicit GDAL mode
  ds_gdal <- ddbs_open_dataset(nc_path, conn = conn, read_shp_mode = "GDAL")
  expect_s3_class(ds_gdal, "duckspatial_df")
  
  # Verify it's using ST_Read via view SQL
  view_sql_gdal <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_gdal, 'source_table')}'"))$sql
  expect_true(grepl("st_read", view_sql_gdal, ignore.case = TRUE))
  expect_false(grepl("st_readshp", view_sql_gdal, ignore.case = TRUE))

  # Data Integrity: Compare counts
  expect_equal(
    ds_shp |> dplyr::count() |> dplyr::collect() |> dplyr::pull(n),
    ds_gdal |> dplyr::count() |> dplyr::collect() |> dplyr::pull(n)
  )

  # Encoding argument
  ds_enc <- ddbs_open_dataset(nc_path, conn = conn, shp_encoding = "UTF-8")
  expect_s3_class(ds_enc, "duckspatial_df")
  view_sql_enc <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_enc, 'source_table')}'"))$sql
  expect_true(grepl('encoding.*UTF-8', view_sql_enc, ignore.case = TRUE))

  # 2. OSM
  # GDAL mode (default)
  ds_osm_gdal <- ddbs_open_dataset("dummy.osm.pbf", conn = conn, read_osm_mode = "GDAL")
  view_sql_osm_gdal <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_osm_gdal, 'source_table')}'"))$sql
  expect_true(grepl("st_read", view_sql_osm_gdal, ignore.case = TRUE))
  
  # ST_ReadOSM mode
  ds_osm_read <- ddbs_open_dataset("dummy.osm.pbf", conn = conn, read_osm_mode = "ST_ReadOSM")
  view_sql_osm_read <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{attr(ds_osm_read, 'source_table')}'"))$sql
  expect_true(grepl("st_readosm", view_sql_osm_read, ignore.case = TRUE))
})

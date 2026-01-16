# Tests for duckdbfs interoperability
# All tests involving duckdbfs::open_dataset integration with duckspatial

# =============================================================================
# normalize_spatial_input with duckdbfs
# =============================================================================

test_that("normalize_spatial_input works for duckdbfs::open_dataset inputs", {
  skip_if_not_installed("duckdbfs")

  # Create a temporary parquet file

  conn <- ddbs_temp_conn()
  sf_obj <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))), a = 1)
  ddbs_write_vector(conn, sf_obj, "test_table")

  tmp_file <- tempfile(fileext = ".parquet")
  DBI::dbExecute(conn, glue::glue("COPY test_table TO '{tmp_file}' (FORMAT PARQUET)"))

  # Open with duckdbfs
  ds <- duckdbfs::open_dataset(tmp_file)

  # Should be converted to duckspatial_df
  result <- normalize_spatial_input(ds)
  expect_s3_class(result, "duckspatial_df")
  expect_s3_class(result, "tbl_duckdb_connection")

  # Clean up
  unlink(tmp_file)
})

# =============================================================================
# ddbs_join with duckdbfs inputs
# =============================================================================

test_that("ddbs_join works with raw duckdbfs tbl_duckdb_connection", {
  skip_if_not_installed("duckdbfs")

  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")

  # Both from duckdbfs - different connections but same CRS
  # Using open_dataset directly (no head) preserves source table for CRS detection
  conn1 <- ddbs_temp_conn()
  conn2 <- ddbs_temp_conn()

  countries1 <- duckdbfs::open_dataset(
    countries_path, format = "sf", conn = conn1
  )
  countries2 <- duckdbfs::open_dataset(
    countries_path, format = "sf", conn = conn2
  )

  # Cross-connection join should work with warning about different connections
  testthat::expect_warning(
    result <- ddbs_join(countries1, countries2, join = "intersects"),
    "connection are different"
  )

  testthat::expect_true(
    inherits(result, "duckspatial_df") || inherits(result, "sf")
  )
})

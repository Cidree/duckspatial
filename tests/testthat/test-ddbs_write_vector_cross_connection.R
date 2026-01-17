# Tests for cross-connection import in ddbs_write_vector
# These test the efficient import path when source is from a different connection

test_that("ddbs_write_vector imports from external duckspatial_df without collecting", {
  skip_if_not_installed("duckdbfs")
  
  # Create a "source" connection (simulating duckdbfs pattern)
  source_conn <- ddbs_create_conn(dbdir = "memory")
  ddbs_write_vector(source_conn, countries_sf, "source_countries", quiet = TRUE)
  
  # Create duckspatial_df from source
  source_tbl <- ddbs_read_vector(source_conn, "source_countries")
  
  # Create our target connection
  target_conn <- ddbs_create_conn(dbdir = "memory")
  on.exit({
    DBI::dbDisconnect(target_conn, shutdown = TRUE)
    DBI::dbDisconnect(source_conn, shutdown = TRUE)
  }, add = TRUE)
  
  # Import from source to target - should use efficient cross-connection path
  expect_no_error(
    ddbs_write_vector(target_conn, source_tbl, "imported_countries", quiet = TRUE)
  )
  
  # Verify table was created
  tables <- ddbs_list_tables(target_conn)
  expect_true("imported_countries" %in% tables$table_name)
  
  # Verify geometry column exists and is GEOMETRY type
  desc <- DBI::dbGetQuery(target_conn, "DESCRIBE imported_countries")
  geom_types <- desc$column_type[desc$column_type == "GEOMETRY"]
  expect_true(length(geom_types) >= 1)
  
  # Verify CRS was preserved
  imported <- ddbs_read_vector(target_conn, "imported_countries")
  expect_equal(ddbs_crs(imported)$input, ddbs_crs(source_tbl)$input)
  
  # Verify data was imported correctly
  imported_sf <- st_as_sf(imported)
  expect_equal(nrow(imported_sf), nrow(countries_sf))
})

test_that("ddbs_write_vector respects overwrite for cross-connection import", {
  skip_if_not_installed("duckdbfs")
  
  # Setup source
  source_conn <- ddbs_create_conn(dbdir = "memory")
  ddbs_write_vector(source_conn, countries_sf, "source_countries", quiet = TRUE)
  source_tbl <- ddbs_read_vector(source_conn, "source_countries")
  
  # Setup target with existing table
  target_conn <- ddbs_create_conn(dbdir = "memory")
  ddbs_write_vector(target_conn, argentina_sf, "imported", quiet = TRUE)
  on.exit({
    DBI::dbDisconnect(target_conn, shutdown = TRUE)
    DBI::dbDisconnect(source_conn, shutdown = TRUE)
  }, add = TRUE)
  
  # Should fail without overwrite
  expect_error(
    ddbs_write_vector(target_conn, source_tbl, "imported", quiet = TRUE),
    "already present"
  )
  
  # Should succeed with overwrite
  expect_no_error(
    ddbs_write_vector(target_conn, source_tbl, "imported", overwrite = TRUE, quiet = TRUE)
  )
  
  # Verify it's the countries data now, not argentina
  result <- DBI::dbGetQuery(target_conn, "SELECT COUNT(*) as n FROM imported")
  expect_equal(result$n, nrow(countries_sf))
})

test_that("ddbs_write_vector same-connection falls back to collect", {
  # When source and target are the SAME connection, should use collect path
  conn <- ddbs_create_conn(dbdir = "memory")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  
  # Write initial data
  ddbs_write_vector(conn, countries_sf, "original", quiet = TRUE)
  
  # Create duckspatial_df from same connection
  tbl <- ddbs_read_vector(conn, "original")
  
  # Write to new table - should work (same connection doesn't need import)
  expect_no_error(
    ddbs_write_vector(conn, tbl, "copy", quiet = TRUE)
  )
  
  # Verify both tables exist
  tables <- ddbs_list_tables(conn)
  expect_true(all(c("original", "copy") %in% tables$table_name))
})

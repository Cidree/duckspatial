# skip tests on CRAN
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")
skip_if_not_installed("sf")

# create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

# 1. Basic functionality with sf objects ----
test_that("ddbs_write_vector writes an sf object to a new table", {
  # Write the points_sf data to a new table
  table_name <- "test_points_write"
  expect_true(ddbs_write_vector(conn_test, points_sf, table_name, overwrite = TRUE))

  # Verify the table exists
  all_tables <- DBI::dbListTables(conn_test)
  expect_true(table_name %in% all_tables)

  # Verify the content
  result <- ddbs_read_vector(conn_test, table_name, crs = 4326)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(points_sf))
  expect_equal(sf::st_crs(result), sf::st_crs(points_sf))
})

test_that("ddbs_write_vector respects the overwrite argument", {
  table_name <- "test_overwrite"
  # Write initial data
  ddbs_write_vector(conn_test, points_sf, table_name, overwrite = TRUE)

  # Expect an error when trying to overwrite without permission
  expect_error(
    ddbs_write_vector(conn_test, points_sf, table_name, overwrite = FALSE),
    "already present"
  )

  # Overwrite with new data (countries_sf)
  expect_true(ddbs_write_vector(conn_test, countries_sf, table_name, overwrite = TRUE))

  # Verify the new data is in the table
  result <- ddbs_read_vector(conn_test, table_name, crs = 4326)
  expect_equal(nrow(result), nrow(countries_sf))
})

test_that("ddbs_write_vector handles different geometry types", {
  # Create some test data with different geometry types
  line <- sf::st_as_sfc("LINESTRING(0 0, 1 1)") |>
    sf::st_sf(id = 1, geom = _)
  polygon <- sf::st_as_sfc("POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))") |>
    sf::st_sf(id = 1, geom = _)

  # Write LINESTRING
  table_line <- "test_line"
  ddbs_write_vector(conn_test, line, table_line, overwrite = TRUE)
  result_line <- ddbs_read_vector(conn_test, table_line, crs = NULL)
  expect_s3_class(result_line, "sf")
  expect_equal(sf::st_geometry_type(result_line) |> as.character(), "LINESTRING")

  # Write POLYGON
  table_polygon <- "test_polygon"
  ddbs_write_vector(conn_test, polygon, table_polygon, overwrite = TRUE)
  result_polygon <- ddbs_read_vector(conn_test, table_polygon, crs = NULL)
  expect_s3_class(result_polygon, "sf")
  expect_equal(sf::st_geometry_type(result_polygon) |> as.character(), "POLYGON")
})

test_that("ddbs_write_vector stores CRS information correctly", {
  table_name <- "test_crs_storage"
  ddbs_write_vector(conn_test, points_sf, table_name, overwrite = TRUE)

  # Query the crs_duckspatial column
  crs_info <- DBI::dbGetQuery(conn_test, glue::glue("SELECT crs_duckspatial FROM {table_name} LIMIT 1"))
  expect_equal(crs_info$crs_duckspatial, "EPSG:4326")
})

# 2. Writing from file paths ----
test_that("ddbs_write_vector can write from a .geojson file path", {
  file_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  table_name <- "countries_from_file_write"

  expect_true(ddbs_write_vector(conn_test, file_path, table_name, overwrite = TRUE))

  # Verify table exists and has content
  all_tables <- DBI::dbListTables(conn_test)
  expect_true(table_name %in% all_tables)
  count <- DBI::dbGetQuery(conn_test, glue::glue("SELECT COUNT(*) FROM {table_name}"))
  expect_gt(count[[1]], 0)

  # Verify CRS is stored
  crs_info <- DBI::dbGetQuery(conn_test, glue::glue("SELECT crs_duckspatial FROM {table_name} LIMIT 1"))
  expect_true(!is.na(crs_info$crs_duckspatial) && nchar(crs_info$crs_duckspatial) > 0)
})

# 3. Edge cases and errors ----
test_that("ddbs_write_vector handles sf objects with no CRS", {
  points_no_crs <- points_sf
  sf::st_crs(points_no_crs) <- NA
  table_name <- "test_no_crs"

  ddbs_write_vector(conn_test, points_no_crs, table_name, overwrite = TRUE)

  # Check that the crs_duckspatial column does NOT exist
  # Because we skip creating it when CRS is missing
  fields <- DBI::dbListFields(conn_test, table_name)
  expect_false("crs_duckspatial" %in% fields)
})

test_that("ddbs_write_vector respects temp_view = TRUE", {
  table_name <- "test_temp_view_write"
  
  # Write with temp_view = TRUE
  expect_true(ddbs_write_vector(conn_test, points_sf, table_name, temp_view = TRUE, overwrite = TRUE))
  
  # Should NOT be in persistent tables
  all_tables <- DBI::dbListTables(conn_test)
  expect_false(table_name %in% all_tables)
  
  # Should be in Arrow views
  arrow_views <- duckdb::duckdb_list_arrow(conn_test)
  expect_true(table_name %in% arrow_views)
  
  # Should be readable
  result <- ddbs_read_vector(conn_test, table_name, crs = 4326)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(points_sf))
})

# Disconnect
duckdb::dbDisconnect(conn_test, shutdown = TRUE)

# 4. New functionality: duckspatial_df and existing CRS columns ----
test_that("ddbs_write_vector can write a duckspatial_df object", {
  conn_new <- ddbs_create_conn()
  
  # Create a duckspatial_df
  ddbs_write_vector(conn_new, points_sf, "points_source", overwrite = TRUE)
  df_lazy <- ddbs_read_vector(conn_new, "points_source", crs = 4326) |>
    as_duckspatial_df()
  
  table_name <- "points_from_lazy"
  
  # This should now work (previously failed with "Expected string vector of length 1")
  expect_true(ddbs_write_vector(conn_new, df_lazy, table_name, overwrite = TRUE))
  
  # Verify result
  result <- ddbs_read_vector(conn_new, table_name, crs = 4326)
  expect_equal(nrow(result), nrow(points_sf))
  expect_equal(sf::st_crs(result), sf::st_crs(points_sf))
  
  duckdb::dbDisconnect(conn_new, shutdown = TRUE)
})

test_that("ddbs_write_vector handles sf objects with existing crs_duckspatial column", {
  conn_new <- ddbs_create_conn()
  
  # Create an sf object that already has crs_duckspatial
  points_with_crs_col <- points_sf
  points_with_crs_col$crs_duckspatial <- "EPSG:4326"
  
  table_name <- "points_duplicate_col"
  
  # This should now work (previously failed with "Column with name crs_duckspatial already exists")
  expect_true(ddbs_write_vector(conn_new, points_with_crs_col, table_name, overwrite = TRUE))
  
  # Verify result
  result <- ddbs_read_vector(conn_new, table_name, crs = 4326)
  expect_equal(nrow(result), nrow(points_sf))
  
  # Check we didn't end up with two crs columns or errors
  fields <- DBI::dbListFields(conn_new, table_name)
  expect_equal(sum(fields == "crs_duckspatial"), 1)
  
  duckdb::dbDisconnect(conn_new, shutdown = TRUE)
})

test_that("ddbs_write_vector throws error for unsupported input", {
  conn_new <- ddbs_create_conn()
  
  expect_error(
    ddbs_write_vector(conn_new, list(a = 1), "bad_input"),
    "must be an .*sf.* object"
  )
  
  duckdb::dbDisconnect(conn_new, shutdown = TRUE)
})

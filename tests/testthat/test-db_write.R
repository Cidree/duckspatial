# skip tests on CRAN
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")
skip_if_not_installed("sf")

# 0. Test ddbs_temp_conn helper ----
test_that("ddbs_temp_conn creates valid auto-closing connection", {
  # Test that connection is valid
  conn <- ddbs_temp_conn()
  expect_true(DBI::dbIsValid(conn))
  
  # Test that connection works
  result <- DBI::dbGetQuery(conn, "SELECT 1 AS test")
  expect_equal(result$test, 1)
})

test_that("ddbs_temp_conn auto-closes on function exit", {
  # Create a helper function that uses ddbs_temp_conn
  get_conn_status <- function() {
    conn <- ddbs_temp_conn()
    # Return the connection object so we can check it after func exits
    conn
  }
  
  # Call the function - connection should be closed after it returns
  returned_conn <- get_conn_status()
  
  # Connection should be invalid (closed) after function returned
  expect_false(DBI::dbIsValid(returned_conn))
})

# create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

# 1. Basic functionality with sf objects ----
test_that("ddbs_write_table writes an sf object to a new table", {
  # Write the points_sf data to a new table
  table_name <- "test_points_write"
  expect_true(ddbs_write_table(conn_test, points_sf, table_name, overwrite = TRUE))

  # Verify the table exists
  all_tables <- DBI::dbListTables(conn_test)
  expect_true(table_name %in% all_tables)

  # Verify the content
  result <- ddbs_read_table(conn_test, table_name)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(points_sf))
  expect_equal(sf::st_crs(result), sf::st_crs(points_sf))
})

test_that("ddbs_write_table respects the overwrite argument", {
  table_name <- "test_overwrite"
  # Write initial data
  ddbs_write_table(conn_test, points_sf, table_name, overwrite = TRUE)

  # Expect an error when trying to overwrite without permission
  expect_error(
    ddbs_write_table(conn_test, points_sf, table_name, overwrite = FALSE),
    "already present"
  )

  # Overwrite with new data (countries_sf)
  expect_true(ddbs_write_table(conn_test, countries_sf, table_name, overwrite = TRUE))

  # Verify the new data is in the table
  result <- ddbs_read_table(conn_test, table_name)
  expect_equal(nrow(result), nrow(countries_sf))
})

test_that("ddbs_write_table handles different geometry types", {
  # Create some test data with different geometry types
  line <- sf::st_as_sfc("LINESTRING(0 0, 1 1)") |>
    sf::st_sf(id = 1, geom = _)
  polygon <- sf::st_as_sfc("POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))") |>
    sf::st_sf(id = 1, geom = _)

  # Write LINESTRING
  table_line <- "test_line"
  ddbs_write_table(conn_test, line, table_line, overwrite = TRUE)
  result_line <- ddbs_read_table(conn_test, table_line)
  expect_s3_class(result_line, "sf")
  expect_equal(sf::st_geometry_type(result_line) |> as.character(), "LINESTRING")

  # Write POLYGON
  table_polygon <- "test_polygon"
  ddbs_write_table(conn_test, polygon, table_polygon, overwrite = TRUE)
  result_polygon <- ddbs_read_table(conn_test, table_polygon)
  expect_s3_class(result_polygon, "sf")
  expect_equal(sf::st_geometry_type(result_polygon) |> as.character(), "POLYGON")
})

test_that("ddbs_write_table stores CRS information correctly", {
  table_name <- "test_crs_storage"
  ddbs_write_table(conn_test, points_sf, table_name, overwrite = TRUE)

  # Query the crs
  crs_info <- DBI::dbGetQuery(
    conn_test, 
    glue::glue("SELECT ST_CRS(geometry) FROM {table_name} LIMIT 1")
  ) |> as.character()
  expect_equal(crs_info, "EPSG:4326")
})

# 2. Writing from file paths ----
test_that("ddbs_write_table can write from a .geojson file path", {
  file_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  table_name <- "countries_from_file_write"

  expect_true(ddbs_write_table(conn_test, file_path, table_name, overwrite = TRUE))

  # Verify table exists and has content
  all_tables <- DBI::dbListTables(conn_test)
  expect_true(table_name %in% all_tables)
  count <- DBI::dbGetQuery(conn_test, glue::glue("SELECT COUNT(*) FROM {table_name}"))
  expect_gt(count[[1]], 0)

  # Verify CRS is stored
  crs_info <- DBI::dbGetQuery(
    conn_test, 
    glue::glue("SELECT ST_CRS(geom) FROM {table_name} LIMIT 1")
  ) |> as.character()
  expect_true(!is.na(crs_info) && nchar(crs_info) > 0)
})

# 3. Edge cases and errors ----
test_that("ddbs_write_table handles sf objects with no CRS", {
  points_no_crs <- points_sf
  sf::st_crs(points_no_crs) <- NA
  table_name <- "test_no_crs"

  ddbs_write_table(conn_test, points_no_crs, table_name, overwrite = TRUE)

  # Check that the crs_duckspatial column does NOT exist
  # Because we skip creating it when CRS is missing
  crs <- DBI::dbGetQuery(
    conn_test,
    "SELECT ST_CRS(geometry) as crs FROM test_no_crs LIMIT 1;"
  )$crs
  expect_true(is.na(crs))
})

test_that("ddbs_write_table respects temp_view = TRUE", {
  table_name <- "test_temp_view_write"
  
  # Write with temp_view = TRUE
  expect_true(ddbs_write_table(conn_test, points_sf, table_name, temp_view = TRUE, overwrite = TRUE))
  
  # Should NOT be in persistent tables (changed in duckdb 1.5)
  all_tables <- DBI::dbListTables(conn_test)
  # expect_false(table_name %in% all_tables)
  expect_true(table_name %in% all_tables)
  
  # Should be in Arrow views
  arrow_views <- duckdb::duckdb_list_arrow(conn_test)
  expect_true(paste0(table_name, "_raw") %in% arrow_views)
  
  # Should be readable
  result <- ddbs_read_table(conn_test, table_name)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(points_sf))
})

# Disconnect
duckdb::dbDisconnect(conn_test)

# 4. New functionality: duckspatial_df and existing CRS columns ----
test_that("ddbs_write_table can write a duckspatial_df object", {
  conn_new <- ddbs_temp_conn()
  
  # Create a duckspatial_df
  ddbs_write_table(conn_new, points_sf, "points_source", overwrite = TRUE)
  df_lazy <- ddbs_read_table(conn_new, "points_source") |>
    as_duckspatial_df()
  
  table_name <- "points_from_lazy"
  
  # This should now work (previously failed with "Expected string vector of length 1")
  expect_true(ddbs_write_table(conn_new, df_lazy, table_name, overwrite = TRUE))
  
  # Verify result
  result <- ddbs_read_table(conn_new, table_name)
  expect_equal(nrow(result), nrow(points_sf))
  expect_equal(sf::st_crs(result), sf::st_crs(points_sf))
})


test_that("ddbs_write_table throws error for unsupported input", {
  conn_new <- ddbs_temp_conn()
  
  expect_error(
    ddbs_write_table(conn_new, list(a = 1), "bad_input"),
    "must be an .*sf.* object"
  )
})

test_that("ddbs_write_table with temp_view=TRUE works for duckspatial_df", {
  conn_new <- ddbs_temp_conn()
  
  # Create a duckspatial_df first
  ddbs_write_table(conn_new, points_sf, "points_src", overwrite = TRUE)
  df_lazy <- ddbs_read_table(conn_new, "points_src") |>
    as_duckspatial_df()
  
  view_name <- "points_lazy_view"
  
  # This should work with temp_view = TRUE
  expect_true(ddbs_write_table(conn_new, df_lazy, view_name, temp_view = TRUE, overwrite = TRUE))
  
  # Verify view was created (should be in Arrow views, not tables)
  arrow_views <- duckdb::duckdb_list_arrow(conn_new)
  expect_true(paste0(view_name, "_raw") %in% arrow_views)
  
  # Should be queryable
  result <- ddbs_read_table(conn_new, view_name)
  expect_equal(nrow(result), nrow(points_sf))
})


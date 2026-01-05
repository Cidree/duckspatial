
test_that("normalize_spatial_input works for sf objects", {
  sf_obj <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))), a = 1)
  expect_identical(duckspatial:::normalize_spatial_input(sf_obj), sf_obj)
})

test_that("normalize_spatial_input works for duckspatial_df objects", {
  conn <- duckspatial:::ddbs_temp_conn()
  
  sf_obj <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))), a = 1)
  duckspatial::ddbs_write_vector(conn, sf_obj, "test_table")
  
  ds_df <- duckspatial::ddbs_read_vector(conn, "test_table")
  expect_identical(duckspatial:::normalize_spatial_input(ds_df), ds_df)
})

test_that("normalize_spatial_input works for tbl_duckdb_connection objects", {
  conn <- duckspatial:::ddbs_temp_conn()
  
  sf_obj <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))), a = 1)
  duckspatial::ddbs_write_vector(conn, sf_obj, "test_table")
  
  tbl_obj <- dplyr::tbl(conn, "test_table")
  result <- duckspatial:::normalize_spatial_input(tbl_obj)
  
  expect_s3_class(result, "duckspatial_df")
  expect_identical(attr(result, "source_table"), "test_table")
})

test_that("normalize_spatial_input works for character inputs", {
  conn <- duckspatial:::ddbs_temp_conn()
  
  DBI::dbExecute(conn, "CREATE TABLE test_table (a INTEGER)")
  
  # Valid existing table
  expect_identical(duckspatial:::normalize_spatial_input("test_table", conn), "test_table")
  
  # Missing connection
  expect_error(duckspatial:::normalize_spatial_input("test_table"), "required when using character table names")
  
  # Non-existent table
  expect_error(duckspatial:::normalize_spatial_input("non_existent", conn), "does not exist in connection")
})

test_that("normalize_spatial_input passes through unsupported types", {
  expect_identical(duckspatial:::normalize_spatial_input(123), 123)
  expect_identical(duckspatial:::normalize_spatial_input(NULL), NULL)
})

test_that("normalize_spatial_input works for duckspatial::ddbs_open_dataset inputs", {
  # Create a distinct temporary parquet file
  conn <- duckspatial:::ddbs_temp_conn()
  sf_obj <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(1, 1))), b = 2) # Different data
  duckspatial::ddbs_write_vector(conn, sf_obj, "test_table_2")
  
  tmp_file_ds <- tempfile(fileext = ".parquet")
  DBI::dbExecute(conn, glue::glue("COPY test_table_2 TO '{tmp_file_ds}' (FORMAT PARQUET)"))
  
  # Open with duckspatial
  ds_ddbs <- duckspatial::ddbs_open_dataset(tmp_file_ds)
  
  # Should be passed through (as it's already duckspatial_df)
  result <- duckspatial:::normalize_spatial_input(ds_ddbs)
  expect_s3_class(result, "duckspatial_df")
  
  # Clean up
  unlink(tmp_file_ds)
})

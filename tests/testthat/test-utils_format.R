test_that("get_parquet_crs handles column names with dots and special characters", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")

  # Create a simple sf dataframe with a geometry column containing a dot
  # Use a non-4326 CRS (like 3857) so GDAL explicitly writes the PROJJSON crs property
  df <- data.frame(id = 1L)
  df[["geom.1"]] <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  df_sf <- sf::st_sf(df, sf_column_name = "geom.1")
  
  tmp_pq <- tempfile(fileext = ".parquet")
  # Use layer_options to force GDAL to keep the geometry column name
  sf::st_write(df_sf, tmp_pq, driver = "Parquet", layer_options = "GEOMETRY_NAME=geom.1", quiet = TRUE)
  on.exit(unlink(tmp_pq), add = TRUE)
  
  conn <- tryCatch(ddbs_default_conn(), error = function(e) DBI::dbConnect(duckdb::duckdb()))
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  
  duckspatial::ddbs_install(conn, extension = "json", quiet = TRUE)
  duckspatial::ddbs_load(conn, extension = "json", quiet = TRUE)
  
  # Call the unexported function
  crs <- duckspatial:::get_parquet_crs(tmp_pq, conn)
  
  expect_false(is.null(crs))
  expect_equal(crs$epsg, 3857)
})

test_that("get_parquet_crs handles column names with spaces", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")

  # Create a simple sf dataframe with a geometry column containing a space
  df <- data.frame(id = 1L)
  df[["my geom"]] <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857)
  df_sf <- sf::st_sf(df, sf_column_name = "my geom")
  
  tmp_pq <- tempfile(fileext = ".parquet")
  sf::st_write(df_sf, tmp_pq, driver = "Parquet", layer_options = "GEOMETRY_NAME=my geom", quiet = TRUE)
  on.exit(unlink(tmp_pq), add = TRUE)
  
  conn <- tryCatch(ddbs_default_conn(), error = function(e) DBI::dbConnect(duckdb::duckdb()))
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  
  duckspatial::ddbs_install(conn, extension = "json", quiet = TRUE)
  duckspatial::ddbs_load(conn, extension = "json", quiet = TRUE)
  
  crs <- duckspatial:::get_parquet_crs(tmp_pq, conn)
  
  expect_false(is.null(crs))
  expect_equal(crs$epsg, 3857)
})

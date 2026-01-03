test_that("new_duckspatial_df creates valid duckspatial_df objects", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  # Create a simple sf object
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  
  # Write to DuckDB
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  # Create lazy table
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  
  # Test new_duckspatial_df
  result <- new_duckspatial_df(
    lazy_tbl, 
    crs = sf::st_crs(nc_sf), 
    geom_col = "geometry",
    source_table = "nc_test"
  )
  
  # Verify class structure
  expect_s3_class(result, "duckspatial_df")
  expect_s3_class(result, "tbl_lazy")
  
  # Verify attributes
  expect_equal(attr(result, "sf_column"), "geometry")
  expect_equal(attr(result, "crs"), sf::st_crs(nc_sf))
  expect_equal(attr(result, "source_table"), "nc_test")
})

test_that("new_duckspatial_df avoids double wrapping", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  result1 <- new_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  # Wrap again - should return same object
  result2 <- new_duckspatial_df(result1, crs = sf::st_crs(nc_sf))
  
  expect_identical(result1, result2)
})

test_that("as_duckspatial_df.duckspatial_df can update metadata", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ds <- as_duckspatial_df(nc_sf, conn = conn)
  
  # Update CRS
  ds_new <- as_duckspatial_df(ds, crs = "EPSG:3857")
  expect_equal(sf::st_crs(ds_new), sf::st_crs("EPSG:3857"))
  
  # Update geom_col
  ds_new2 <- as_duckspatial_df(ds, geom_col = "new_geom")
  expect_equal(attr(ds_new2, "sf_column"), "new_geom")
  
  # Return same if no updates
  expect_identical(ds, as_duckspatial_df(ds))
})

test_that("as_duckspatial_df.sf works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  
  # Convert sf to duckspatial_df
  result <- as_duckspatial_df(nc_sf, conn = conn)
  
  expect_s3_class(result, "duckspatial_df")
  expect_s3_class(result, "tbl_lazy")
  expect_equal(attr(result, "crs"), sf::st_crs(nc_sf))
  expect_equal(attr(result, "sf_column"), attr(nc_sf, "sf_column"))
})

test_that("as_duckspatial_df.tbl_duckdb_connection works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  
  # Convert to duckspatial_df
  result <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), sf::st_crs(nc_sf))
})

test_that("as_duckspatial_df.tbl_lazy works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  # Use dbplyr to create a lazy table (which is tbl_lazy)
  lazy_tbl <- dplyr::tbl(conn, "nc_test") |> dplyr::filter(AREA > 0)
  
  result <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), sf::st_crs(nc_sf))
})

test_that("as_duckspatial_df.character works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  # Convert using table name
  result <- as_duckspatial_df("nc_test", conn = conn, crs = sf::st_crs(nc_sf))
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "source_table"), "nc_test")
})

test_that("as_duckspatial_df.character requires connection", {
  # When no connection is provided and no default exists, should error
  # The error might come from trying to access the table or from the connection check
  expect_error(
    as_duckspatial_df("some_table"),
    "conn|Table.*does not exist"
  )
})

test_that("as_duckspatial_df.data.frame works with sfc columns", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  # Create a data frame with an sfc column (but not sf class yet)
  df <- data.frame(id = 1, val = "a")
  df$geom <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  
  result <- as_duckspatial_df(df, conn = conn)
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(sf::st_crs(result), sf::st_crs(4326))
  expect_equal(attr(result, "sf_column"), "geom")
  
  # Test with explicit geom_col
  result2 <- as_duckspatial_df(df, conn = conn, geom_col = "geom")
  expect_equal(attr(result2, "sf_column"), "geom")
  
  # Should error if no sfc column
  df_no_geom <- data.frame(id = 1)
  expect_error(as_duckspatial_df(df_no_geom, conn = conn), "sfc")
  
  # Should error if specified geom_col is not sfc (but there are other sfc columns)
  df_wrong_geom_multi <- data.frame(id = 1, not_geom = 2)
  df_wrong_geom_multi$real_geom <- sf::st_sfc(sf::st_point(c(0, 0)))
  expect_error(
    as_duckspatial_df(df_wrong_geom_multi, conn = conn, geom_col = "not_geom"), 
    "sfc"
  )
})

test_that("is_duckspatial_df works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  lazy_tbl <- dplyr::tbl(conn, "nc_test")
  result <- as_duckspatial_df(lazy_tbl, crs = sf::st_crs(nc_sf))
  
  expect_true(is_duckspatial_df(result))
  expect_false(is_duckspatial_df(lazy_tbl))
  expect_false(is_duckspatial_df(nc_sf))
  expect_false(is_duckspatial_df(NULL))
})

test_that("dplyr verbs preserve duckspatial_df class", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  # Test filter
  filtered <- nc_lazy |> dplyr::filter(AREA > 0.1)
  expect_s3_class(filtered, "duckspatial_df")
  expect_equal(attr(filtered, "crs"), attr(nc_lazy, "crs"))
  
  # Test mutate
  mutated <- nc_lazy |> dplyr::mutate(area_sq = AREA * AREA)
  expect_s3_class(mutated, "duckspatial_df")
  expect_equal(attr(mutated, "crs"), attr(nc_lazy, "crs"))
  
  # Test select
  selected <- nc_lazy |> dplyr::select(NAME, AREA, geometry)
  expect_s3_class(selected, "duckspatial_df")
  expect_equal(attr(selected, "crs"), attr(nc_lazy, "crs"))
  
  # Test arrange
  arranged <- nc_lazy |> dplyr::arrange(AREA)
  expect_s3_class(arranged, "duckspatial_df")
  expect_equal(attr(arranged, "crs"), attr(nc_lazy, "crs"))
})

test_that("ddbs_collect works with duckspatial_df", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(geom_col = "geometry", crs = sf::st_crs(nc_sf))
  
  # ddbs_collect should work (detailed tests in test-ddbs_collect.R)
  result <- ddbs_collect(nc_lazy)
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(nc_sf))
})



test_that("st_crs.duckspatial_df returns correct CRS", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  expect_equal(sf::st_crs(nc_lazy), sf::st_crs(nc_sf))
})



test_that("print.duckspatial_df shows informative output", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  # Capture print output
  output <- capture.output(print(nc_lazy))
  
  expect_true(any(grepl("duckspatial lazy spatial table", output)))
  expect_true(any(grepl("CRS:", output)))
  expect_true(any(grepl("Geometry column:", output)))
})

test_that("ddbs_geom_col returns correct geometry column name", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  expect_equal(ddbs_geom_col(nc_lazy), "geometry")
  expect_equal(ddbs_geom_col(nc_sf), attr(nc_sf, "sf_column"))
})

test_that("left_join.duckspatial_df preserves spatial attributes", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  # Create a simple data frame to join
  extra_data <- data.frame(
    NAME = nc_sf$NAME[1:5],
    extra_col = 1:5
  )
  DBI::dbWriteTable(conn, "extra_data", extra_data)
  extra_lazy <- dplyr::tbl(conn, "extra_data")
  
  # Perform left join
  result <- dplyr::left_join(nc_lazy, extra_lazy, by = "NAME")
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), attr(nc_lazy, "crs"))
  expect_equal(attr(result, "sf_column"), attr(nc_lazy, "sf_column"))
})

test_that("inner_join.duckspatial_df preserves spatial attributes", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  # Create a simple data frame to join
  extra_data <- data.frame(
    NAME = nc_sf$NAME[1:5],
    extra_col = 1:5
  )
  DBI::dbWriteTable(conn, "extra_data", extra_data)
  extra_lazy <- dplyr::tbl(conn, "extra_data")
  
  # Perform inner join
  result <- dplyr::inner_join(nc_lazy, extra_lazy, by = "NAME")
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), attr(nc_lazy, "crs"))
  expect_equal(attr(result, "sf_column"), attr(nc_lazy, "sf_column"))
})

test_that("st_geometry.duckspatial_df works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  # Access geometry
  geom <- sf::st_geometry(nc_lazy)
  
  expect_s3_class(geom, "sfc")
  expect_equal(length(geom), nrow(nc_sf))
  expect_equal(sf::st_crs(geom), sf::st_crs(nc_sf))
})

test_that("st_bbox.duckspatial_df works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  bbox <- sf::st_bbox(nc_lazy)
  expect_s3_class(bbox, "bbox")
  # Compare with original bbox
  bbox_orig <- sf::st_bbox(nc_sf)
  expect_equal(as.numeric(bbox), as.numeric(bbox_orig))
})

test_that("st_as_sf.duckspatial_df works correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  result_sf <- sf::st_as_sf(nc_lazy)
  
  expect_s3_class(result_sf, "sf")
  expect_s3_class(result_sf, "data.frame")
  expect_equal(nrow(result_sf), nrow(nc_sf))
})


# Tests for connection management

test_that("cross-connection filtering works with proper fallback strategies", {
  skip_if_not_installed("sf")

  # Setup: Two distinct connections
  conn1 <- ddbs_create_conn()
  conn2 <- ddbs_create_conn()
  on.exit({
    ddbs_stop_conn(conn1)
    ddbs_stop_conn(conn2)
  })

  # Load data using internal package data
  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  ds1 <- ddbs_open_dataset(countries_path, conn = conn1)
  ds2 <- ddbs_open_dataset(countries_path, conn = conn2)

  # CASE 1: Direct View Import (Strategy 1)
  # This should trigger warnings about cross-connection imports
  expect_warning(
    expect_warning(
      res1 <- ddbs_filter(ds1, ds2),
      "come from different DuckDB connections"
    ),
    "Importing.*to the target connection"
  )
  res_df1 <- collect(res1)
  expect_true(nrow(res_df1) > 0)

  # CASE 2: Transformed Query Import (Strategy 3 via Collect)
  # Filter ds2 so it becomes a query, not a direct view
  # This forces fallback to Strategy 3 because dbplyr queries can't be SQL-recreated across conns easily
  ds2_mod <- ds2 |> dplyr::filter(CNTR_ID == "AR")

  expect_warning(
    expect_warning(
      expect_warning(
        res2 <- ddbs_filter(ds1, ds2_mod),
        "come from different DuckDB connections"
      ),
      "Importing.*to the target connection"
    ),
    "Imported via collection"
  )

  res_df2 <- collect(res2)
  # Argentina + neighbors should be returned
  expect_true(nrow(res_df2) >= 1)
  expect_true(nrow(res_df2) < 257) # Should be a subset
})


test_that("ddbs_crs works on character tables without CRS column using view analysis", {
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  nc_path <- system.file("shape/nc.shp", package = "sf")
  
  # Create a view manually that mimics one without crs_duckspatial
  # (Standard ST_Read view)
  view_name <- "raw_view"
  DBI::dbExecute(conn, glue::glue("
    CREATE VIEW {view_name} AS SELECT * FROM ST_Read('{nc_path}')
  "))
  
  # Check if ddbs_crs can auto-detect it
  crs <- ddbs_crs(view_name, conn = conn)
  
  # Should find EPSG:4267
  expect_false(is.na(crs))
  expect_equal(crs$epsg, 4267)
})

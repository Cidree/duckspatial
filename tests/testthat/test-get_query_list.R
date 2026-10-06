testthat::skip_on_cran()

test_that("get_query_list handles sf objects", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  
  # Action
  res <- get_query_list(nc_sf, conn)
  
  # Verify
  expect_true(grepl("temp_view_", res$query_name))
  expect_true(DBI::dbExistsTable(conn, res$query_name))
  
  # Verify cleanup
  res$cleanup()
  expect_disjoint(
    res$query_name,
    ddbs_list_tables(conn)$table_name
  )
  expect_false(DBI::dbExistsTable(conn, res$query_name))
})

test_that("get_query_list handles duckspatial_df with source_table", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_table(conn, nc_sf, "nc_source", quiet = TRUE)
  
  ds <- as_duckspatial_df("nc_source", conn = conn, crs = sf::st_crs(nc_sf))
  
  # Action
  res <- get_query_list(ds, conn)
  
  # Verify - should return source name directly
  # expect_equal(res$query_name, "nc_source")
  expect_in(
    "nc_source",
    ddbs_list_tables(conn)$table_name
  )
  
  # Cleanup should be no-op (source table remains)
  res$cleanup()
  # expect_true(DBI::dbExistsTable(conn, "nc_source"))
  expect_in(
    "nc_source",
    ddbs_list_tables(conn)$table_name
  )
})

test_that("get_query_list handles duckspatial_df WITHOUT source_table (efficient fallback)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ddbs_write_table(conn, nc_sf, "nc_test", quiet = TRUE)
  
  # Create query-based duckspatial_df
  ds <- as_duckspatial_df("nc_test", conn) |> dplyr::filter(AREA > 0)
  
  # Force source_table to NULL to trigger fallback
  attr(ds, "source_table") <- NULL
  
  # Action
  res <- get_query_list(ds, conn)
  
  # Verify
  expect_true(grepl("temp_view_", res$query_name))
  
  # Check it's a view with SQL definition (proving sql_render was used)
  view_def <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{res$query_name}'"))
  expect_true(nrow(view_def) == 1)
  expect_true(grepl("SELECT", view_def$sql, ignore.case = TRUE))
  
  # Verify cleanup
  res$cleanup()
  expect_false(DBI::dbExistsTable(conn, res$query_name))
})

test_that("get_query_list handles tbl_lazy objects", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  conn <- ddbs_temp_conn()
  DBI::dbWriteTable(conn, "mtcars_test", mtcars)
  
  tbl_lazy <- dplyr::tbl(conn, "mtcars_test") |> dplyr::filter(mpg > 20)
  
  # Action
  res <- get_query_list(tbl_lazy, conn)
  
  # Verify
  expect_true(grepl("temp_view_", res$query_name))
  
  # Check view
  view_def <- DBI::dbGetQuery(conn, glue::glue("SELECT sql FROM duckdb_views() WHERE view_name = '{res$query_name}'"))
  expect_true(nrow(view_def) == 1)
  expect_true(grepl("mpg > 20", view_def$sql) || grepl('"mpg" > 20.0', view_def$sql))
  
  # Verify cleanup
  res$cleanup()
  expect_false(DBI::dbExistsTable(conn, res$query_name))
})

test_that("get_query_list handles character inputs", {
  conn <- ddbs_temp_conn()
  DBI::dbWriteTable(conn, "plain_table", data.frame(id = 1))
  
  # Action
  res <- get_query_list("plain_table", conn)
  
  # Verify: wrapped in a temp view, so any table name is safe to use in SQL (#168)
  expect_match(res$query_name, "^temp_view_")
  expect_equal(DBI::dbGetQuery(conn, glue::glue("SELECT id FROM {res$query_name}"))$id, 1)
  
  # Cleanup drops the view, never the table
  res$cleanup()
  expect_false(DBI::dbExistsTable(conn, res$query_name))
  expect_true(DBI::dbExistsTable(conn, "plain_table"))
})

test_that("get_query_list uses source_table directly for unmodified duckspatial_df", {
  ds <- as_duckspatial_df(nc_sf)

  res <- get_query_list(ds, dbplyr::remote_con(ds))
  on.exit(res$cleanup(), add = TRUE)

  expect_equal(res$query_name, attr(ds, "source_table"))
})

test_that("ddbs_* functions respect dplyr verbs without a duckspatial_df method (#159)", {
  ## 6 rows, 3 distinct
  ds <- as_duckspatial_df(rbind(nc_sf[1:3, ], nc_sf[1:3, ]))
  conn <- dbplyr::remote_con(ds)

  verbs <- list(
    distinct  = dplyr::distinct(ds),
    semi_join = dplyr::semi_join(ds, data.frame(NAME = "Ashe"), by = "NAME", copy = TRUE),
    anti_join = dplyr::anti_join(ds, data.frame(NAME = "Ashe"), by = "NAME", copy = TRUE),
    union_all = dplyr::union_all(ds, ds)
  )

  for (verb in names(verbs)) {
    v <- verbs[[verb]]
    expected <- nrow(dplyr::collect(v))

    res <- get_query_list(v, conn)
    n_query <- DBI::dbGetQuery(conn, glue::glue("SELECT COUNT(*) AS n FROM {res$query_name}"))$n
    res$cleanup()

    expect_equal(n_query, expected, info = verb)
    expect_equal(nrow(dplyr::collect(ddbs_area(v))), expected, info = verb)
    expect_equal(nrow(ddbs_centroid(v, mode = "sf")), expected, info = verb)
  }
})

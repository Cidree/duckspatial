testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb", minimum_version = "1.5.1")
testthat::skip_if_not_installed("sf")

test_that("duckdb geometry type preserves authority and custom CRS literals", {
  conn <- ddbs_temp_conn()

  expect_equal(
    duckspatial:::duckdb_geometry_type(conn, "EPSG:4326"),
    "GEOMETRY('EPSG:4326')"
  )
  expect_equal(duckspatial:::duckdb_geometry_type(conn, NA), "GEOMETRY")

  custom <- "+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=49 +lat_2=77 +datum=NAD83"
  literal <- duckspatial:::crs_to_duckdb_literal(custom)
  expect_equal(literal$kind, "wkt")
  expect_match(
    duckspatial:::duckdb_geometry_type(conn, custom),
    "^GEOMETRY\\('PROJCRS",
    perl = TRUE
  )
})

test_that("ddbs_create_conn persists native CRS metadata in v1.5 storage", {
  db_path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  conn <- ddbs_create_conn(db_path)
  expect_equal(attr(conn, "duckspatial_storage_mode"), "v1.5.0")
  expect_equal(attr(conn, "duckspatial_storage"), "v1.5.0+")
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(conn), silent = TRUE)),
    add = TRUE
  )
  ddbs_write_table(conn, points_sf[1:3, ], "points", quiet = TRUE)
  ddbs_stop_conn(conn)

  conn2 <- ddbs_create_conn(db_path)
  expect_equal(attr(conn2, "duckspatial_storage_mode"), "v1.5.0")
  expect_equal(attr(conn2, "duckspatial_storage"), "v1.5.0+")
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(conn2), silent = TRUE)),
    add = TRUE
  )
  ds <- as_duckspatial_df("points", conn = conn2)
  geom_col <- attr(ds, "sf_column")

  expect_equal(sf::st_crs(ds)$epsg, 4326)
  expect_false(is.na(duckspatial:::read_native_crs(conn2, "points", geom_col)))
  expect_true(is.na(duckspatial:::get_column_comment(
    conn2,
    "points",
    geom_col
  )))
})

test_that("v1.0.0 storage (Legacy Compatibility) writes and reads CRS column comments", {
  db_path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  conn <- ddbs_create_conn(db_path, duckdb_storage_version = "v1.0.0")
  expect_equal(attr(conn, "duckspatial_storage_mode"), "legacy")
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(conn), silent = TRUE)),
    add = TRUE
  )
  ddbs_write_table(conn, points_sf[1:3, ], "points", quiet = TRUE)
  geom_col <- get_geom_name(conn, "points")

  comment <- duckspatial:::get_column_comment(conn, "points", geom_col)
  expect_match(comment, "\"duckspatial\"", fixed = TRUE)
  expect_equal(sf::st_crs(ddbs_crs("points", conn = conn))$epsg, 4326)
  ddbs_stop_conn(conn)

  conn2 <- ddbs_create_conn(db_path, duckdb_storage_version = "v1.0.0")
  expect_equal(attr(conn2, "duckspatial_storage_mode"), "legacy")
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(conn2), silent = TRUE)),
    add = TRUE
  )
  expect_equal(sf::st_crs(ddbs_crs("points", conn = conn2))$epsg, 4326)
})

test_that("ddbs_write_dataset writes native DuckDB database files", {
  db_path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  expect_no_error(
    ddbs_write_dataset(
      points_sf[1:3, ],
      db_path,
      layer = "points",
      quiet = TRUE
    )
  )

  ds <- ddbs_open_dataset(db_path, layer = "points")
  on.exit(
    suppressWarnings(try(
      ddbs_stop_conn(attr(ds, "source_conn")),
      silent = TRUE
    )),
    add = TRUE
  )

  expect_s3_class(ds, "duckspatial_df")
  expect_equal(dplyr::count(ds) |> dplyr::pull(n), 3)
  expect_equal(sf::st_crs(ds)$epsg, 4326)
})

test_that("ddbs_write_dataset v1.0.0 DuckDB output (Legacy Compatibility) writes CRS column comments", {
  db_path <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  expect_no_error(
    ddbs_write_dataset(
      points_sf[1:3, ],
      db_path,
      layer = "points",
      duckdb_storage_version = "v1.0.0",
      quiet = TRUE
    )
  )

  conn <- ddbs_create_conn(db_path, duckdb_storage_version = "v1.0.0")
  expect_equal(attr(conn, "duckspatial_storage_mode"), "legacy")
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(conn), silent = TRUE)),
    add = TRUE
  )

  geom_col <- get_geom_name(conn, "points")
  comment <- duckspatial:::get_column_comment(conn, "points", geom_col)

  expect_match(comment, "\"duckspatial\"", fixed = TRUE)
  expect_equal(sf::st_crs(ddbs_crs("points", conn = conn))$epsg, 4326)
  ddbs_stop_conn(conn)

  expect_no_warning({
    ds <- ddbs_open_dataset(db_path, layer = "points")
  })
  on.exit(
    suppressWarnings(try(
      ddbs_stop_conn(attr(ds, "source_conn")),
      silent = TRUE
    )),
    add = TRUE
  )
  expect_equal(sf::st_crs(ds)$epsg, 4326)
})

test_that("custom proj4 CRS round-trips through native and comment metadata", {
  custom_crs <- paste(
    "+proj=lcc +lat_0=49 +lon_0=-95 +lat_1=49 +lat_2=77",
    "+datum=NAD83 +units=m +no_defs"
  )
  custom_points <- sf::st_sf(
    id = 1:2,
    geom = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1000, 1000)),
      crs = custom_crs
    )
  )

  native_db <- tempfile(fileext = ".duckdb")
  compat_db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(c(native_db, compat_db)), add = TRUE)

  native_conn <- ddbs_create_conn(native_db)
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(native_conn), silent = TRUE)),
    add = TRUE
  )
  ddbs_write_table(native_conn, custom_points, "pts", quiet = TRUE)
  ddbs_stop_conn(native_conn)

  native_conn2 <- ddbs_create_conn(native_db)
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(native_conn2), silent = TRUE)),
    add = TRUE
  )
  native_crs <- ddbs_crs("pts", conn = native_conn2)
  expect_true(duckspatial:::crs_equal(native_crs, sf::st_crs(custom_crs)))
  expect_false(is.na(duckspatial:::read_native_crs(native_conn2, "pts", "geom")))

  compat_conn <- ddbs_create_conn(compat_db, duckdb_storage_version = "v1.0.0")
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(compat_conn), silent = TRUE)),
    add = TRUE
  )
  ddbs_write_table(compat_conn, custom_points, "pts", quiet = TRUE)
  ddbs_stop_conn(compat_conn)

  compat_conn2 <- ddbs_create_conn(compat_db, duckdb_storage_version = "v1.0.0")
  on.exit(
    suppressWarnings(try(ddbs_stop_conn(compat_conn2), silent = TRUE)),
    add = TRUE
  )
  compat_crs <- ddbs_crs("pts", conn = compat_conn2)
  expect_true(duckspatial:::crs_equal(compat_crs, sf::st_crs(custom_crs)))
  expect_true(is.na(duckspatial:::read_native_crs(compat_conn2, "pts", "geom")))
  expect_match(
    duckspatial:::get_column_comment(compat_conn2, "pts", "geom"),
    "\"wkt2_2019\"",
    fixed = TRUE
  )
})

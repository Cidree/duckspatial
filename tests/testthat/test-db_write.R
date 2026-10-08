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
  
  # The requested name should be queryable through the typed temp view
  all_tables <- DBI::dbListTables(conn_test)
  expect_true(table_name %in% all_tables)

  # The raw Arrow view is hidden behind the typed SQL view
  arrow_views <- duckdb::duckdb_list_arrow(conn_test)
  expect_true(paste0("__raw_", table_name) %in% arrow_views)
  
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
  
  # Verify the typed view was created and backed by a hidden raw Arrow view
  all_tables <- DBI::dbListTables(conn_new)
  expect_true(view_name %in% all_tables)

  arrow_views <- duckdb::duckdb_list_arrow(conn_new)
  expect_true(paste0("__raw_", view_name) %in% arrow_views)
  
  # Should be queryable
  result <- ddbs_read_table(conn_new, view_name)
  expect_equal(nrow(result), nrow(points_sf))
})

test_that("ddbs_write_table on a parquet path errors without a CRS warning (#166)", {
  conn_new <- ddbs_temp_conn()
  parquet_path <- tempfile(fileext = ".parquet")
  ddbs_write_dataset(points_sf, parquet_path, quiet = TRUE)
  on.exit(unlink(parquet_path), add = TRUE)

  expect_no_warning(
    expect_error(ddbs_write_table(conn_new, parquet_path, "pq", quiet = TRUE), "parquet")
  )
})


# 5. Fast write paths (sf register + CTAS, same-connection CTAS) ----
describe("ddbs_write_table() sf fast path", {

  it("keeps column types, row order, CRS and geometry", {
    conn <- ddbs_temp_conn()
    x <- points_sf[1:50, ]
    x$chr <- rep(c("a", NA), 25)
    x$fac <- factor(rep(c("lo", "hi"), 25))
    x$dt  <- as.Date("2020-01-01") + 0:49
    x$ts  <- as.POSIXct("2021-05-01 12:00:00", tz = "UTC") + 0:49
    x$lg  <- rep(c(TRUE, FALSE), 25)
    x$ord <- 50:1

    ddbs_write_table(conn, x, "types_t", quiet = TRUE)

    cols <- DBI::dbGetQuery(conn, "DESCRIBE types_t")
    types <- stats::setNames(cols$column_type, cols$column_name)
    expect_equal(unname(types[c("chr", "fac", "dt", "ts", "lg", "ord")]),
                 c("VARCHAR", "ENUM('hi', 'lo')", "DATE", "TIMESTAMP", "BOOLEAN", "INTEGER"))
    expect_equal(unname(types["geometry"]), "GEOMETRY('EPSG:4326')")

    res <- DBI::dbGetQuery(conn, "SELECT ord, chr, ST_AsWKB(geometry) AS wkb FROM types_t")
    expect_equal(res$ord, x$ord)
    expect_equal(res$chr, x$chr)
    expect_equal(lapply(res$wkb, as.raw),
                 lapply(sf::st_as_binary(sf::st_geometry(x)), as.raw))
  })

  it("writes mixed XYZ / XY / EMPTY geometries", {
    conn <- ddbs_temp_conn()
    mixed <- sf::st_sf(
      id = 1:4,
      geometry = sf::st_sfc(
        sf::st_point(c(1, 2, 3)), sf::st_point(c(1, 2)), sf::st_point(),
        sf::st_linestring(matrix(c(0.5, 1.5, 0.5, 1.5), 2)),
        crs = 4326
      )
    )
    expect_true(ddbs_write_table(conn, mixed, "mixed", quiet = TRUE))
    wkt <- DBI::dbGetQuery(conn, "SELECT ST_AsText(geometry) AS t FROM mixed ORDER BY id")$t
    expect_equal(wkt, c("POINT Z (1 2 3)", "POINT (1 2)", "POINT EMPTY",
                        "LINESTRING (0.5 0.5, 1.5 1.5)"))
  })

  it("handles integer coordinates, missing CRS and zero rows", {
    conn <- ddbs_temp_conn()
    int_sf <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(sf::st_point(1:2), sf::st_point(3:4), crs = 4326))
    ddbs_write_table(conn, int_sf, "int_t", quiet = TRUE)
    expect_equal(DBI::dbGetQuery(conn, "SELECT ST_AsText(geometry) t FROM int_t ORDER BY id")$t,
                 c("POINT (1 2)", "POINT (3 4)"))

    no_crs <- sf::st_set_crs(points_sf[1:5, ], NA)
    expect_message(ddbs_write_table(conn, no_crs, "no_crs_t"), "No CRS")
    expect_true(is.na(sf::st_crs(ddbs_read_table(conn, "no_crs_t", quiet = TRUE))))

    ddbs_write_table(conn, points_sf[0, ], "zero_t", quiet = TRUE)
    expect_equal(DBI::dbGetQuery(conn, "SELECT count(*) n FROM zero_t")$n, 0)
    cols <- DBI::dbGetQuery(conn, "DESCRIBE zero_t")
    expect_equal(cols$column_type[cols$column_name == "geometry"], "GEOMETRY('EPSG:4326')")
  })

  it("writes to a schema-qualified name given as 'schema.table'", {
    conn <- ddbs_temp_conn()
    DBI::dbExecute(conn, "CREATE SCHEMA s1")
    expect_true(ddbs_write_table(conn, nc_sf, "s1.nc", quiet = TRUE))
    expect_equal(DBI::dbGetQuery(conn, "SELECT count(*) n FROM s1.nc")$n, nrow(nc_sf))
    expect_equal(DBI::dbGetQuery(conn, "SELECT ST_CRS(geometry) c FROM s1.nc LIMIT 1")$c, "EPSG:4267")
  })

  it("does not leave a partial table behind when the geometry cannot be written", {
    conn <- ddbs_temp_conn()
    curve <- sf::st_sf(id = 1, geometry = sf::st_as_sfc("CIRCULARSTRING(0 0, 1 1, 2 0)", crs = 4326))
    expect_error(ddbs_write_table(conn, curve, "curve_t", quiet = TRUE))
    expect_false("curve_t" %in% DBI::dbListTables(conn))
  })
})

describe("ddbs_write_table() same-connection lazy input", {

  it("materializes a filtered/mutated pipeline in-database with types and CRS", {
    conn <- ddbs_temp_conn()
    ddbs_write_table(conn, nc_sf, "nc_src", quiet = TRUE)
    lazy <- as_duckspatial_df("nc_src", conn = conn) |>
      dplyr::filter(AREA > 0.1) |>
      dplyr::mutate(area2 = AREA * 2)

    expect_message(ddbs_write_table(conn, lazy, "nc_out"), "successfully imported")

    out <- ddbs_read_table(conn, "nc_out", quiet = TRUE)
    expect_equal(nrow(out), sum(nc_sf$AREA > 0.1))
    expect_equal(out$area2, out$AREA * 2)
    expect_equal(sf::st_crs(out)$epsg, 4267L)
    cols <- DBI::dbGetQuery(conn, "DESCRIBE nc_out")
    expect_equal(cols$column_type[cols$column_name == "CRESS_ID"], "INTEGER")

    expect_error(ddbs_write_table(conn, lazy, "nc_out", quiet = TRUE), "already present")
  })

  it("can overwrite the table it reads from", {
    conn <- ddbs_temp_conn()
    ddbs_write_table(conn, nc_sf, "nc_self", quiet = TRUE)
    lazy <- as_duckspatial_df("nc_self", conn = conn) |> dplyr::filter(AREA > 0.1)

    expect_message(
      ddbs_write_table(conn, lazy, "nc_self", overwrite = TRUE),
      "dropped"
    )
    expect_equal(DBI::dbGetQuery(conn, "SELECT count(*) n FROM nc_self")$n, sum(nc_sf$AREA > 0.1))
    expect_equal(DBI::dbGetQuery(conn, "SELECT ST_CRS(geometry) c FROM nc_self LIMIT 1")$c, "EPSG:4267")
  })

  it("keeps a CRS that is only known from the R metadata", {
    conn <- ddbs_temp_conn()
    ddbs_write_table(conn, sf::st_set_crs(nc_sf, NA), "nc_nocrs", quiet = TRUE)
    lazy <- as_duckspatial_df(dplyr::tbl(conn, "nc_nocrs"), crs = sf::st_crs(nc_sf))

    ddbs_write_table(conn, lazy, "nc_crs", quiet = TRUE)
    expect_equal(DBI::dbGetQuery(conn, "SELECT ST_CRS(geometry) c FROM nc_crs LIMIT 1")$c, "EPSG:4267")
  })

  it("writes the legacy CRS comment on legacy-storage connections", {
    path <- tempfile(fileext = ".duckdb")
    conn <- suppressWarnings(ddbs_create_conn(path, duckdb_storage_version = "v1.0.0"))
    on.exit({ ddbs_stop_conn(conn); unlink(path) }, add = TRUE)
    ddbs_write_table(conn, nc_sf, "nc_legacy", quiet = TRUE)
    lazy <- as_duckspatial_df("nc_legacy", conn = conn) |> dplyr::filter(AREA > 0.1)

    ddbs_write_table(conn, lazy, "nc_legacy_out", quiet = TRUE)
    cmt <- DBI::dbGetQuery(conn, "SELECT comment FROM duckdb_columns() WHERE table_name = 'nc_legacy_out' AND column_name = 'geometry'")$comment
    expect_match(cmt, "duckspatial")
    expect_equal(sf::st_crs(ddbs_read_table(conn, "nc_legacy_out", quiet = TRUE))$epsg, 4267L)
  })
})

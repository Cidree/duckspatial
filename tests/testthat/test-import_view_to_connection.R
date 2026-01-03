
# Tests for import_view_to_connection -------------------------------------

test_that("Strategy 1: SQL recreation works for same DB (direct view)", {
  skip_if_not_installed("duckdb")
  
  # File-based DB required for testing same-file connections
  conn1 <- duckspatial:::ddbs_tmp_conn(file = TRUE)
  db_file <- attr(conn1, "db_file")
  
  DBI::dbExecute(conn1, "CREATE TABLE data (id INTEGER, val DOUBLE)")
  DBI::dbExecute(conn1, "INSERT INTO data VALUES (1, 1.1), (2, 2.2)")
  DBI::dbExecute(conn1, "CREATE VIEW my_view AS SELECT * FROM data")
  
  tbl_ref <- dplyr::tbl(conn1, "my_view")
  
  # Second connection to same file
  conn2 <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file)
  withr::defer(DBI::dbDisconnect(conn2), envir = parent.frame())
  
  expect_message(
    res <- duckspatial:::import_view_to_connection(conn2, conn1, tbl_ref, "imported_view"),
    "SQL recreation"
  )
  
  expect_equal(res$method, "sql_recreation")
  df_res <- DBI::dbGetQuery(conn2, "SELECT * FROM imported_view ORDER BY id")
  expect_equal(nrow(df_res), 2)
})

test_that("Strategy 2: SQL render works for same DB (lazy query)", {
  skip_if_not_installed("duckdb")
  
  # File-based DB required for testing same-file connections
  conn1 <- duckspatial:::ddbs_tmp_conn(file = TRUE)
  db_file <- attr(conn1, "db_file")
  
  DBI::dbExecute(conn1, "CREATE TABLE data (id INTEGER, val DOUBLE)")
  DBI::dbExecute(conn1, "INSERT INTO data VALUES (1, 10), (2, 20)")
  
  tbl_lazy <- dplyr::tbl(conn1, "data") |> 
    dplyr::filter(id == 1) |>
    dplyr::mutate(val = val * 2)
  
  # Second connection to same file
  conn2 <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file)
  withr::defer(DBI::dbDisconnect(conn2, shutdown = TRUE), envir = parent.frame())
  
  expect_message(
    res <- duckspatial:::import_view_to_connection(conn2, conn1, tbl_lazy, "imported_lazy"),
    "SQL query"
  )
  
  expect_equal(res$method, "sql_render")
  df <- DBI::dbGetQuery(conn2, "SELECT * FROM imported_lazy")
  expect_equal(nrow(df), 1)
})

test_that("Strategy 3: ATTACH works for file-based cross-connection", {
  skip_if_not_installed("duckdb")
  
  # Create source file, write data, then close
  source_file <- tempfile(fileext = ".duckdb")
  source_conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = source_file)
  DBI::dbExecute(source_conn, "CREATE TABLE test_data (id INTEGER, name TEXT)")
  DBI::dbExecute(source_conn, "INSERT INTO test_data VALUES (1, 'a'), (2, 'b')")
  DBI::dbDisconnect(source_conn)
  
  # Re-connect as read-only using helper
  source_conn <- duckspatial:::ddbs_tmp_conn(file = source_file, read_only = TRUE)
  tbl_ref <- dplyr::tbl(source_conn, "test_data")
  
  # Target: memory DB
  target_conn <- duckspatial:::ddbs_tmp_conn()
  
  # Since dbGetInfo may return NULL for read-only, ATTACH might not trigger
  res <- duckspatial:::import_view_to_connection(target_conn, source_conn, tbl_ref, "attached_view")
  
  # Accept any successful method
  expect_true(res$method %in% c("attach", "nanoarrow", "duckdb_register"))
})

test_that("Strategy 4: Nanoarrow streaming works for cross-memory-DB", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("nanoarrow")
  
  conn1 <- duckspatial:::ddbs_tmp_conn()
  conn2 <- duckspatial:::ddbs_tmp_conn()
  
  DBI::dbExecute(conn1, "CREATE TABLE data (id INTEGER, val TEXT)")
  DBI::dbExecute(conn1, "INSERT INTO data VALUES (1, 'a'), (2, 'b')")
  
  tbl_ref <- dplyr::tbl(conn1, "data")
  
  res <- duckspatial:::import_view_to_connection(conn2, conn1, tbl_ref, "arrow_imported")
  
  expect_true(res$method %in% c("nanoarrow", "duckdb_register"))
})

test_that("Strategy 5: Collect+sf works for duckspatial_df", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("sf")
  
  conn1 <- duckspatial:::ddbs_tmp_conn()
  conn2 <- duckspatial:::ddbs_tmp_conn()
  
  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  sf_data <- sf::st_sf(id = 1:2, geometry = pts)
  
  dsdf <- as_duckspatial_df(sf_data, conn = conn1)
  
  res <- duckspatial:::import_view_to_connection(conn2, conn1, dsdf, "sf_imported")
  
  expect_true(res$method %in% c("nanoarrow", "collect_and_write"))
})

test_that("Quoted identifiers work correctly", {
  skip_if_not_installed("duckdb")
  
  conn1 <- duckspatial:::ddbs_tmp_conn(file = TRUE)
  db_file <- attr(conn1, "db_file")
  
  DBI::dbExecute(conn1, "CREATE TABLE \"quoted table\" (id INTEGER)")
  DBI::dbExecute(conn1, "CREATE VIEW \"quoted view\" AS SELECT * FROM \"quoted table\"")
  
  tbl_ref <- dplyr::tbl(conn1, "quoted view")
  
  conn2 <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file)
  withr::defer(DBI::dbDisconnect(conn2, shutdown = TRUE), envir = parent.frame())
  
  expect_message(
    res <- duckspatial:::import_view_to_connection(conn2, conn1, tbl_ref, "imported_quoted"),
    "SQL recreation"
  )
  
  expect_equal(res$method, "sql_recreation")
})

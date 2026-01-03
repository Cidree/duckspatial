
test_that("ddbs_tmp_conn: default in-memory connection", {
  conn <- ddbs_tmp_conn()
  expect_true(DBI::dbIsValid(conn))
  # dbdir is NULL for in-memory connections in this environment
  expect_true(is.null(DBI::dbGetInfo(conn)$dbdir) || DBI::dbGetInfo(conn)$dbdir == ":memory:")
  
  # Auto-close check
  get_conn <- function() {
    c <- ddbs_tmp_conn()
    c
  }
  ret_conn <- get_conn()
  expect_false(DBI::dbIsValid(ret_conn))
})

test_that("ddbs_tmp_conn: file-based temporary connection", {
  conn <- ddbs_tmp_conn(file = TRUE)
  expect_true(DBI::dbIsValid(conn))
  
  db_file <- attr(conn, "db_file")
  expect_true(is.character(db_file))
  expect_true(file.exists(db_file))
  
  # Cleanup check
  get_file_status <- function() {
    c <- ddbs_tmp_conn(file = TRUE)
    f <- attr(c, "db_file")
    list(conn = c, file = f)
  }
  status <- get_file_status()
  expect_false(DBI::dbIsValid(status$conn))
  expect_false(file.exists(status$file))
})

test_that("ddbs_tmp_conn: custom file path", {
  custom_path <- tempfile(fileext = ".myduck")
  conn <- ddbs_tmp_conn(file = custom_path)
  expect_true(DBI::dbIsValid(conn))
  expect_equal(attr(conn, "db_file"), custom_path)
  expect_true(file.exists(custom_path))
  
  # Manual cleanup trigger (by exiting scope)
  test_custom_cleanup <- function(path) {
    c <- ddbs_tmp_conn(file = path)
  }
  test_custom_cleanup(custom_path)
  expect_false(file.exists(custom_path))
})

test_that("ddbs_tmp_conn: custom path with cleanup = FALSE", {
  custom_path <- tempfile(fileext = ".persistent")
  
  test_no_cleanup <- function(path) {
    c <- ddbs_tmp_conn(file = path, cleanup = FALSE)
    DBI::dbExecute(c, "CREATE TABLE t(id INT)")
  }
  
  test_no_cleanup(custom_path)
  
  # File should STILL exist
  expect_true(file.exists(custom_path))
  
  # Verify we can connect to it
  c2 <- DBI::dbConnect(duckdb::duckdb(), dbdir = custom_path)
  expect_true("t" %in% DBI::dbListTables(c2))
  DBI::dbDisconnect(c2, shutdown = TRUE)
  
  # Clean up manually
  unlink(custom_path)
})

test_that("ddbs_tmp_conn: read_only file connection", {
  # Create a file first
  base_path <- tempfile(fileext = ".base")
  c1 <- DBI::dbConnect(duckdb::duckdb(), dbdir = base_path)
  DBI::dbExecute(c1, "CREATE TABLE t(id INT)")
  DBI::dbDisconnect(c1, shutdown = TRUE)
  
  # Use ddbs_tmp_conn as read-only
  test_ro <- function(path) {
    c <- ddbs_tmp_conn(file = path, read_only = TRUE, cleanup = FALSE)
    expect_error(DBI::dbExecute(c, "INSERT INTO t VALUES (1)"), "read-only")
  }
  test_ro(base_path)
  
  unlink(base_path)
})

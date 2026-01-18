library(testthat)
library(duckspatial)

test_that("ddbs_create_conn accepts threads and memory_limit_gb", {
  # Test with specific values
  conn <- tryCatch(
    ddbs_create_conn(threads = 1, memory_limit_gb = 1),
    error = function(e) skip(paste("Could not create connection:", e$message))
  )
  on.exit(ddbs_stop_conn(conn), add = TRUE)
  
  settings <- ddbs_get_resources(conn)
  expect_equal(as.integer(settings$threads), 1L)
  # DuckDB might report 953.6 MiB for 1GB
  expect_true(grepl("GB|MiB", settings$memory_limit, ignore.case = TRUE))
})

test_that("ddbs_temp_conn accepts threads and memory_limit_gb", {
  # In-memory path
  conn <- ddbs_temp_conn(threads = 2, memory_limit_gb = 2)
  # Cleanup is handled by ddbs_temp_conn's on.exit
  
  settings <- ddbs_get_resources(conn)
  expect_equal(as.integer(settings$threads), 2L)
  expect_true(grepl("GB|GiB", settings$memory_limit, ignore.case = TRUE))
  
  # File-based path
  conn2 <- ddbs_temp_conn(file = TRUE, threads = 1, memory_limit_gb = 1)
  settings2 <- ddbs_get_resources(conn2)
  expect_equal(as.integer(settings2$threads), 1L)
  expect_true(grepl("GB|MiB", settings2$memory_limit, ignore.case = TRUE))
})

test_that("ddbs_set_resources and ddbs_get_resources work", {
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn), add = TRUE)
  
  # Set new resources
  res <- ddbs_set_resources(conn, threads = 1, memory_limit_gb = 4)
  
  expect_equal(as.integer(res$threads), 1L)
  expect_true(grepl("4GB|3.7 GiB", res$memory_limit, ignore.case = TRUE))
  
  # Verify with get_resources
  res2 <- ddbs_get_resources(conn)
  expect_equal(res, res2)
  
  # Partial update
  ddbs_set_resources(conn, threads = 2)
  res3 <- ddbs_get_resources(conn)
  expect_equal(as.integer(res3$threads), 2L)
  expect_true(grepl("4GB|3.7 GiB", res3$memory_limit, ignore.case = TRUE))
})

test_that("assertions enforce valid inputs", {
  # ddbs_create_conn
  expect_error(ddbs_create_conn(threads = -1), "positive integer")
  expect_error(ddbs_create_conn(threads = 1.5), "positive integer")
  expect_error(ddbs_create_conn(memory_limit_gb = -1), "positive number")
  expect_error(ddbs_create_conn(memory_limit_gb = "invalid"), "positive number")
  
  # ddbs_temp_conn
  expect_error(ddbs_temp_conn(threads = 0), "positive integer")
  expect_error(ddbs_temp_conn(memory_limit_gb = 0), "positive number")
  
  # ddbs_set_resources
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn), add = TRUE)
  
  expect_error(ddbs_set_resources(conn, threads = -5), "positive integer")
  expect_error(ddbs_set_resources(conn, memory_limit_gb = -10), "positive number")
})

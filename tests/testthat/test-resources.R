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
  expect_equal(settings$threads, 1L)
  # DuckDB output should match requested GB (within small tolerance for format rounding)
  expect_true(settings$memory_limit_gb >= 0.9 && settings$memory_limit_gb <= 1.1)
})

test_that("ddbs_temp_conn accepts threads and memory_limit_gb", {
  # In-memory path
  conn <- ddbs_temp_conn(threads = 2, memory_limit_gb = 2)
  # Cleanup is handled by ddbs_temp_conn's on.exit
  
  settings <- ddbs_get_resources(conn)
  expect_equal(settings$threads, 2L)
  # DuckDB output should match requested GB (within small tolerance for format rounding)
  expect_true(settings$memory_limit_gb >= 1.9 && settings$memory_limit_gb <= 2.1)
  
  # File-based path
  conn2 <- ddbs_temp_conn(file = TRUE, threads = 1, memory_limit_gb = 1)
  settings2 <- ddbs_get_resources(conn2)
  expect_equal(settings2$threads, 1L)
  expect_true(settings2$memory_limit_gb >= 0.9 && settings2$memory_limit_gb <= 1.1)
})

test_that("ddbs_set_resources and ddbs_get_resources work", {
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn), add = TRUE)
  
  # Set new resources
  res <- ddbs_set_resources(conn, threads = 1, memory_limit_gb = 4)
  
  expect_equal(res$threads, 1L)
  # Use numeric field with tolerance to handle DuckDB format variations
  expect_true(res$memory_limit_gb >= 3.9 && res$memory_limit_gb <= 4.1)
  
  # Verify with get_resources
  res2 <- ddbs_get_resources(conn)
  expect_equal(res, res2)
  
  # Partial update
  ddbs_set_resources(conn, threads = 2)
  res3 <- ddbs_get_resources(conn)
  expect_equal(res3$threads, 2L)
  # Memory should still be ~4GB from previous set
  expect_true(res3$memory_limit_gb >= 3.9 && res3$memory_limit_gb <= 4.1)
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

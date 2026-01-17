
test_that("ddbs_options sets and retrieves options", {
  # Save original options to restore after test
  op_orig <- options()
  on.exit(options(op_orig))
  
  # Check default (empty call)
  defaults <- ddbs_options()
  expect_type(defaults, "list")
  expect_true("duckspatial.output_type" %in% names(defaults))

  # Set new option
  res <- ddbs_options(output_type = "sf")
  expect_equal(getOption("duckspatial.output_type"), "sf")
  expect_equal(res$duckspatial.output_type, "sf")
  
  res <- ddbs_options(output_type = "tibble")
  expect_equal(getOption("duckspatial.output_type"), "tibble")

  # Set new extended options
  ddbs_options(output_type = "raw")
  expect_equal(getOption("duckspatial.output_type"), "raw")
  
  ddbs_options(output_type = "geoarrow")
  expect_equal(getOption("duckspatial.output_type"), "geoarrow")
  
  # Invalid option should error
  expect_error(ddbs_options(output_type = "invalid"), "Invalid output_type")
  
  # Null should do nothing
  before <- getOption("duckspatial.output_type")
  ddbs_options(output_type = NULL)
  expect_equal(getOption("duckspatial.output_type"), before)
})

test_that("ddbs_sitrep runs without error", {
  # Just check it doesn't crash and returns invisible list
  expect_no_error({
    res <- ddbs_sitrep()
  })
  expect_type(res, "list")
  expect_true("output_type" %in% names(res))
  expect_true("connection_status" %in% names(res))
})

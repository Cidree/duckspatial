

# skip tests on CRAN
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

# helpers --------------------------------------------------------------

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## store countries
duckspatial::ddbs_write_vector(conn_test, countries_sf, "countries")

# expected behavior --------------------------------------------------------------

testthat::test_that("generate points as SF", {
    
    ## create from sf
    generated_pts_sf <- duckspatial::ddbs_generate_points(
      x = argentina_sf,
      n = 33
    )
  
    ## create from DuckDB table
    generated_pts_tbl_sf <- duckspatial::ddbs_generate_points(
        x     = "countries",
        n     = 58,
        conn  = conn_test,
        quiet = TRUE
    )

    ## checks
    testthat::expect_equal(nrow(generated_pts_sf), 33)
    testthat::expect_equal(nrow(generated_pts_tbl_sf), 58)
    testthat::expect_s3_class(generated_pts_tbl_sf, "sf")

})


testthat::test_that("generate points as DuckDB table", {
  
  ## create from sf
  generated_pts_msg <- duckspatial::ddbs_generate_points(
    x = countries_sf,
    n = 33,
    conn = conn_test,
    name = "generated_pts"
  )

  ## create from DuckDB table
  generated_pts_tbl_msg <- duckspatial::ddbs_generate_points(
    x    = "countries",
    n    = 33,
    conn = conn_test,
    name = "generated_pts_tbl",
    quiet = TRUE
  )

  ## read the data
  generated_pts_tbl_sf <- duckspatial::ddbs_read_vector(
    conn = conn_test,
    name = "generated_pts_tbl"
  )

  ## checks
  testthat::expect_true(generated_pts_msg)
  testthat::expect_true(generated_pts_tbl_msg)
  testthat::expect_equal(nrow(generated_pts_tbl_sf), 33)

  ## overwrite table
  generated_pts_tbl_msg_overwrite <- duckspatial::ddbs_generate_points(
    x         = "countries",
    n         = 55,
    conn      = conn_test,
    name      = "generated_pts_tbl",
    overwrite = TRUE
  )

  generated_pts_tbl_msg_overwrite_sf <- duckspatial::ddbs_read_vector(
    conn = conn_test,
    name = "generated_pts_tbl"
  )
  
  ## checks
  testthat::expect_true(generated_pts_tbl_msg_overwrite)
  testthat::expect_equal(nrow(generated_pts_tbl_msg_overwrite_sf), 55)

  ## check overwrite = FALSE throws an error when tbl exists
  testthat::expect_error(
    duckspatial::ddbs_generate_points(
      x         = "countries",
      n         = 55,
      conn      = conn_test,
      name      = "generated_pts_tbl",
      overwrite = FALSE
    )
  )


})


# expected errors --------------------------------------------------------------

testthat::test_that("errors with incorrect input", {

    testthat::expect_error(duckspatial::ddbs_generate_points(x = 999))
    testthat::expect_error(duckspatial::ddbs_generate_points(conn = 999))
    testthat::expect_error(duckspatial::ddbs_generate_points(overwrite = 999))
    testthat::expect_error(duckspatial::ddbs_generate_points(quiet = 999))

    testthat::expect_error(duckspatial::ddbs_generate_points(x = "999", conn = conn_test))

    testthat::expect_error(duckspatial::ddbs_generate_points(conn = conn_test, name = c('banana', 'banana')))


})


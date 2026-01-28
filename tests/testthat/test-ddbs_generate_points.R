
# 0. Set up --------------------------------------------------------------

# skip tests on CRAN
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write data
duckspatial::ddbs_write_vector(conn_test, argentina_sf, "argentina")


# 1. ddbs_generate_points() ----------------------------------------------

## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats, n works, and seed works
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: check different seeds
testthat::test_that("ddbs_generate_points(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_generate_points(argentina_ddbs, 50, seed = 123)
  output_sf   <- ddbs_generate_points(argentina_sf, 50, seed = 123)
  output_conn <- ddbs_generate_points("argentina", 50, conn = conn_test, seed = 123)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(nrow(ddbs_collect(output_ddbs)), 50)
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_generate_points(argentina_ddbs, 10, output = "geoarrow")
  output_sf_fmt       <- ddbs_generate_points(argentina_ddbs, 10, output = "sf")
  output_raw_fmt      <- ddbs_generate_points(argentina_ddbs, 10, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_generate_points(argentina_ddbs, 10))
  testthat::expect_message(ddbs_generate_points("argentina", 50, conn = conn_test, name = "generate_points"))
  testthat::expect_message(ddbs_generate_points("argentina", 50, conn = conn_test, name = "generate_points", overwrite = TRUE))
  testthat::expect_true(ddbs_generate_points("argentina", 50, conn = conn_test, name = "generate_points2"))

  testthat::expect_no_message(ddbs_generate_points(argentina_ddbs, 50, quiet = TRUE))
  testthat::expect_no_message(ddbs_generate_points("argentina", 50, seed = 123, conn = conn_test, name = "generate_points", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "generate_points")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )


  ## CHECK 1.5
  output_ddbs_2 <- ddbs_generate_points(argentina_ddbs, 50, seed = 678)

  testthat::expect_false(
    identical(ddbs_collect(output_ddbs), ddbs_collect(output_ddbs_2))
  )
    
})
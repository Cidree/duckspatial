
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write data
duckspatial::ddbs_write_vector(conn_test, countries_sf, "countries")


# 1. ddbs_boundary() -------------------------------------------------------

## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: geometry type should be line
testthat::test_that("ddbs_boundary(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_boundary(countries_ddbs)
  output_sf   <- ddbs_boundary(countries_sf)
  output_conn <- ddbs_boundary("countries", conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(nrow(ddbs_collect(output_ddbs)), nrow(countries_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_boundary(countries_ddbs, output = "geoarrow")
  output_sf_fmt       <- ddbs_boundary(countries_ddbs, output = "sf")
  output_raw_fmt      <- ddbs_boundary(countries_ddbs, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_boundary(countries_ddbs))
  testthat::expect_message(ddbs_boundary("countries", conn = conn_test, name = "boundary"))
  testthat::expect_message(ddbs_boundary("countries", conn = conn_test, name = "boundary", overwrite = TRUE))
  testthat::expect_true(ddbs_boundary("countries", conn = conn_test, name = "boundary2"))

  testthat::expect_no_message(ddbs_boundary(countries_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_boundary("countries", conn = conn_test, name = "boundary", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "boundary")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )

  ## CHECK 1.5
  geom_type <- sf::st_geometry_type(ddbs_collect(output_ddbs)) |> as.character()
  testthat::expect_in(geom_type, c("LINESTRING", "MULTILINESTRING"))
    
})

## 1.2. Errors -------------------------

## CHECK 2.1: general errors
testthat::test_that("ddbs_boundary(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_boundary("countries", conn = NULL))
    testthat::expect_error(ddbs_boundary(x = 999))
    testthat::expect_error(ddbs_boundary(countries_ddbs, conn = 999))
    testthat::expect_error(ddbs_boundary(countries_ddbs, new_column = 999))
    testthat::expect_error(ddbs_boundary(countries_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_boundary(countries_ddbs, quiet = 999))
    testthat::expect_error(ddbs_boundary(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_boundary(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})

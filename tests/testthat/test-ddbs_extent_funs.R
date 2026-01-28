
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



# 2. ddbs_envelope() -------------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: geometry type should be polygon
## - CHECK 1.6: by_feature works as expected
## - CHECK 1.7: extent should be the same as input
testthat::test_that("ddbs_envelope(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_envelope(countries_ddbs)
  output_sf   <- ddbs_envelope(countries_sf)
  output_conn <- ddbs_envelope("countries", conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_envelope(countries_ddbs, output = "geoarrow")
  output_sf_fmt       <- ddbs_envelope(countries_ddbs, output = "sf")
  output_raw_fmt      <- ddbs_envelope(countries_ddbs, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_envelope(countries_ddbs))
  testthat::expect_message(ddbs_envelope("countries", conn = conn_test, name = "envelope"))
  testthat::expect_message(ddbs_envelope("countries", conn = conn_test, name = "envelope", overwrite = TRUE))
  testthat::expect_true(ddbs_envelope("countries", conn = conn_test, name = "envelope2"))

  testthat::expect_no_message(ddbs_envelope(countries_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_envelope("countries", conn = conn_test, name = "envelope", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "envelope")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )

  ## CHECK 1.5
  geom_type <- sf::st_geometry_type(ddbs_collect(output_ddbs)) |> as.character()
  testthat::expect_in(geom_type, c("POLYGON", "MULTIPOLYGON"))

  ## CHECK 1.6
  bf_false <- ddbs_envelope(countries_sf, by_feature = FALSE) |> ddbs_collect()
  bf_true  <- ddbs_envelope(countries_sf, by_feature = TRUE) |> ddbs_collect()

  testthat::expect_equal(nrow(bf_false), 1)
  testthat::expect_equal(nrow(bf_true), nrow(countries_sf))

  ## CHECK 1.7
  extent_output <- ddbs_bbox(output_ddbs)
  extent_input <- ddbs_bbox(countries_ddbs)

  testthat::expect_equal(extent_output, extent_input)
    
})

## 2.2. Errors -------------------------

## CHECK 2.1: specific errors
## CHECK 2.2: general errors
testthat::test_that("ddbs_envelope(): errors work", {

  ## CHECK 2.1
  testthat::expect_error(ddbs_envelope(countries_ddbs, by_feature = "TRUE"))
  
  ## CHECK 2.2
  testthat::expect_error(ddbs_envelope("countries", conn = NULL))
  testthat::expect_error(ddbs_envelope(x = 999))
  testthat::expect_error(ddbs_envelope(countries_ddbs, conn = 999))
  testthat::expect_error(ddbs_envelope(countries_ddbs, new_column = 999))
  testthat::expect_error(ddbs_envelope(countries_ddbs, overwrite = 999))
  testthat::expect_error(ddbs_envelope(countries_ddbs, quiet = 999))
  testthat::expect_error(ddbs_envelope(x = "999", conn = conn_test))
  testthat::expect_error(ddbs_envelope(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})



# 3. ddbs_bbox() ---------------------------------------------------------

## 3.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: messages work
## - CHECK 1.3: writting a table works
## - CHECK 1.4: by_feature works as expected
testthat::test_that("ddbs_bbox(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_bbox(countries_ddbs)
  output_sf   <- ddbs_bbox(countries_sf)
  output_conn <- ddbs_bbox("countries", conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "data.frame")
  testthat::expect_equal(output_ddbs, output_sf)
  testthat::expect_equal(output_ddbs, output_conn)

  ## CHECK 1.3
  testthat::expect_message(ddbs_bbox(countries_ddbs))
  testthat::expect_message(ddbs_bbox("countries", conn = conn_test, name = "bbox"))
  testthat::expect_message(ddbs_bbox("countries", conn = conn_test, name = "bbox", overwrite = TRUE))
  testthat::expect_true(ddbs_bbox("countries", conn = conn_test, name = "bbox2"))

  testthat::expect_no_message(ddbs_bbox(countries_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_bbox("countries", conn = conn_test, name = "bbox", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- DBI::dbReadTable(conn_test, "bbox")
  testthat::expect_equal(output_ddbs, output_tbl)
    
})

## 1.2. Errors -------------------------

## CHECK 2.1: specific errors
## CHECK 2.2: general errors
testthat::test_that("ddbs_bbox(): errors work", {

  ## CHECK 2.1
  testthat::expect_error(ddbs_bbox(countries_ddbs, by_feature = "TRUE"))
  
  ## CHECK 2.2
  testthat::expect_error(ddbs_bbox("countries", conn = NULL))
  testthat::expect_error(ddbs_bbox(x = 999))
  testthat::expect_error(ddbs_bbox(countries_ddbs, conn = 999))
  testthat::expect_error(ddbs_bbox(countries_ddbs, new_column = 999))
  testthat::expect_error(ddbs_bbox(countries_ddbs, overwrite = 999))
  testthat::expect_error(ddbs_bbox(countries_ddbs, quiet = 999))
  testthat::expect_error(ddbs_bbox(x = "999", conn = conn_test))
  testthat::expect_error(ddbs_bbox(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})

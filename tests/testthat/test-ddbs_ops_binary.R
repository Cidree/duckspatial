
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()
conn_test_2 <- duckspatial::ddbs_create_conn()

## write data in the database
ddbs_write_vector(conn_test, points_sf, "points")
ddbs_write_vector(conn_test, argentina_sf, "argentina")
ddbs_write_vector(conn_test_2, argentina_sf, "argentina")


# 1. ddbs_intersection() -------------------------------------------------

## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: conn_x and conn_y work
## - CHECK 1.6: compare to sf
testthat::test_that("ddbs_intersection(): expected behavior", {
  
  ## CHECK 1.1
  output_1 <- ddbs_intersection(points_sf, argentina_sf)
  output_2 <- ddbs_intersection(points_ddbs, argentina_sf)
  output_3 <- ddbs_intersection(points_sf, argentina_ddbs)
  output_4 <- ddbs_intersection(points_ddbs, argentina_ddbs)  
  testthat::expect_warning(ddbs_intersection("points", argentina_ddbs, conn = conn_test))
  output_6 <- ddbs_intersection("points", argentina_sf, conn = conn_test)
  output_7 <- ddbs_intersection(points_sf, "argentina", conn = conn_test)
  testthat::expect_warning(ddbs_intersection(points_ddbs, "argentina", conn = conn_test))
  output_9 <- ddbs_intersection("points", "argentina", conn = conn_test)

  testthat::expect_s3_class(output_1, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_2))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_3))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_4))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_6))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_7))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_9))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_intersection(points_sf, argentina_ddbs, output = "geoarrow")
  output_sf_fmt       <- ddbs_intersection(points_sf, argentina_ddbs, output = "sf")
  output_raw_fmt      <- ddbs_intersection(points_sf, argentina_ddbs, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_intersection(points_sf, argentina_sf))
  testthat::expect_message(ddbs_intersection(points_sf, argentina_sf, conn = conn_test, name = "intersection"))
  testthat::expect_message(ddbs_intersection(points_sf, argentina_sf, conn = conn_test, name = "intersection", overwrite = TRUE))
  testthat::expect_true(ddbs_intersection(points_sf, argentina_sf, conn = conn_test, name = "intersection2"))

  testthat::expect_no_message(ddbs_intersection(points_sf, argentina_sf, quiet = TRUE))
  testthat::expect_no_message(ddbs_intersection("points", "argentina", conn = conn_test, name = "intersection", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "intersection")
  testthat::expect_equal(
    ddbs_collect(output_1)$geometry,
    output_tbl$geometry
  )


  ## CHECK 1.5
  output_10 <- ddbs_intersection("points", "argentina", conn_x = conn_test, conn_y = conn_test_2)
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_10))

  testthat::expect_message(ddbs_intersection("points", "argentina", conn_x = conn_test, conn_y = conn_test_2, name = "test"))
  testthat::expect_true(DBI::dbExistsTable(conn_test, "test"))
  testthat::expect_false(DBI::dbExistsTable(conn_test_2, "test"))


  ## CHECK 1.6
  sf_output   <- sf::st_intersection(points_sf, argentina_sf)
  ddbs_output <- ddbs_intersection(points_sf, argentina_sf) |> 
    sf::st_as_sf()

  testthat::expect_equal(sf_output$geometry, ddbs_output$geometry)
    
})



## 1.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_intersection(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_intersection(argentina_ddbs))
    testthat::expect_error(ddbs_intersection(y = argentina_ddbs))
    testthat::expect_error(ddbs_intersection("argentina", conn = NULL))
    testthat::expect_error(ddbs_intersection("points", "argentina", conn_x = conn_test))
    testthat::expect_error(ddbs_intersection("points", "argentina", conn_y = conn_test))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_intersection(x = 999))
    testthat::expect_error(ddbs_intersection(argentina_ddbs, points_sf, conn = 999))
    testthat::expect_error(ddbs_intersection(argentina_ddbs, points_sf, overwrite = 999))
    testthat::expect_error(ddbs_intersection(argentina_ddbs, points_sf, quiet = 999))
    testthat::expect_error(ddbs_intersection(x = "999", points_sf, conn = conn_test))
    testthat::expect_error(ddbs_intersection(argentina_ddbs, points_sf, conn = conn_test, name = c('banana', 'banana')))
  
})



# 2. ddbs_difference() -------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: conn_x and conn_y work
## - CHECK 1.6: compare to sf
testthat::test_that("ddbs_difference(): expected behavior", {
  
  ## CHECK 1.1
  output_1 <- ddbs_difference(points_sf, argentina_sf)
  output_2 <- ddbs_difference(points_ddbs, argentina_sf)
  output_3 <- ddbs_difference(points_sf, argentina_ddbs)
  output_4 <- ddbs_difference(points_ddbs, argentina_ddbs)  
  testthat::expect_warning(ddbs_difference("points", argentina_ddbs, conn = conn_test))
  output_6 <- ddbs_difference("points", argentina_sf, conn = conn_test)
  output_7 <- ddbs_difference(points_sf, "argentina", conn = conn_test)
  testthat::expect_warning(ddbs_difference(points_ddbs, "argentina", conn = conn_test))
  output_9 <- ddbs_difference("points", "argentina", conn = conn_test)

  testthat::expect_s3_class(output_1, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_2))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_3))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_4))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_6))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_7))
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_9))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_difference(points_sf, argentina_ddbs, output = "geoarrow")
  output_sf_fmt       <- ddbs_difference(points_sf, argentina_ddbs, output = "sf")
  output_raw_fmt      <- ddbs_difference(points_sf, argentina_ddbs, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_difference(points_sf, argentina_sf))
  testthat::expect_message(ddbs_difference(points_sf, argentina_sf, conn = conn_test, name = "difference"))
  testthat::expect_message(ddbs_difference(points_sf, argentina_sf, conn = conn_test, name = "difference", overwrite = TRUE))
  testthat::expect_true(ddbs_difference(points_sf, argentina_sf, conn = conn_test, name = "difference2"))

  testthat::expect_no_message(ddbs_difference(points_sf, argentina_sf, quiet = TRUE))
  testthat::expect_no_message(ddbs_difference("points", "argentina", conn = conn_test, name = "difference", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "difference")
  testthat::expect_equal(
    ddbs_collect(output_1)$geometry,
    output_tbl$geometry
  )


  ## CHECK 1.5
  output_10 <- ddbs_difference("points", "argentina", conn_x = conn_test, conn_y = conn_test_2)
  testthat::expect_equal(ddbs_collect(output_1), ddbs_collect(output_10))

  testthat::expect_message(ddbs_difference("points", "argentina", conn_x = conn_test, conn_y = conn_test_2, name = "diff3"))
  testthat::expect_true(DBI::dbExistsTable(conn_test, "diff3"))
  testthat::expect_false(DBI::dbExistsTable(conn_test_2, "diff3"))


  ## CHECK 1.6
  sf_output   <- sf::st_difference(points_sf, argentina_sf)
  ddbs_output <- ddbs_difference(points_sf, argentina_sf) |> 
    sf::st_as_sf()

  testthat::expect_equal(sf_output$geometry, ddbs_output$geometry)
    
})



## 2.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_difference(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_difference(argentina_ddbs))
    testthat::expect_error(ddbs_difference(y = argentina_ddbs))
    testthat::expect_error(ddbs_difference("argentina", conn = NULL))
    testthat::expect_error(ddbs_difference("points", "argentina", conn_x = conn_test))
    testthat::expect_error(ddbs_difference("points", "argentina", conn_y = conn_test))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_difference(x = 999))
    testthat::expect_error(ddbs_difference(argentina_ddbs, points_sf, conn = 999))
    testthat::expect_error(ddbs_difference(argentina_ddbs, points_sf, overwrite = 999))
    testthat::expect_error(ddbs_difference(argentina_ddbs, points_sf, quiet = 999))
    testthat::expect_error(ddbs_difference(x = "999", points_sf, conn = conn_test))
    testthat::expect_error(ddbs_difference(argentina_ddbs, points_sf, conn = conn_test, name = c('banana', 'banana')))
  
})

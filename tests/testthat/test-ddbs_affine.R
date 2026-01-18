

# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write some data
ddbs_write_vector(conn_test, argentina_sf, "argentina")
ddbs_write_vector(conn_test, nc_sf, "nc")

# 1. ddbs_rotate() -------------------------------------------------------


## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
testthat::test_that("ddbs_rotate(): expected behavior", {
    
    ## CHECK 1.1
    output_ddbs_1 <- ddbs_rotate(argentina_ddbs, 45)
    output_ddbs_2 <- ddbs_rotate(argentina_ddbs, 45, units = "radians")
    output_ddbs_3 <- ddbs_rotate(argentina_ddbs, 45, by_feature = TRUE)
    output_ddbs_4 <- ddbs_rotate(argentina_ddbs, 45, by_feature = TRUE, center_x = 0, center_y = 0)
    output_ddbs_5 <- ddbs_rotate(argentina_ddbs, 45, quiet = TRUE)
  
    testthat::expect_s3_class(output_ddbs_1, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_2, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_3, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_4, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_5, "duckspatial_df")

    ## CHECK 1.2
    output_ddbs_6 <- ddbs_rotate(argentina_ddbs, 45, output = "geoarrow")
    output_ddbs_7 <- ddbs_rotate(argentina_ddbs, 45, output = "sf")
    output_ddbs_8 <- ddbs_rotate(argentina_ddbs, 45, output = "raw")

    testthat::expect_s3_class(output_ddbs_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_ddbs_7, "sf")
    testthat::expect_s3_class(output_ddbs_8, "tbl_df")
  
    ## CHECK 1.3
    output_sf_1 <- ddbs_rotate(argentina_sf, 45)
    output_sf_2 <- ddbs_rotate(argentina_sf, 45, units = "radians")
    output_sf_3 <- ddbs_rotate(argentina_sf, 45, by_feature = TRUE)
    output_sf_4 <- ddbs_rotate(argentina_sf, 45, by_feature = TRUE, center_x = 0, center_y = 0)
    output_sf_5 <- ddbs_rotate(argentina_sf, 45, quiet = TRUE)
  
    testthat::expect_s3_class(output_sf_1, "duckspatial_df")
    testthat::expect_s3_class(output_sf_2, "duckspatial_df")
    testthat::expect_s3_class(output_sf_3, "duckspatial_df")
    testthat::expect_s3_class(output_sf_4, "duckspatial_df")
    testthat::expect_s3_class(output_sf_5, "duckspatial_df")

  
    ## CHECK 1.4
    output_sf_6 <- ddbs_rotate(argentina_sf, 45, output = "geoarrow")
    output_sf_7 <- ddbs_rotate(argentina_sf, 45, output = "sf")
    output_sf_8 <- ddbs_rotate(argentina_sf, 45, output = "raw")

    testthat::expect_s3_class(output_sf_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_7, "sf")
    testthat::expect_s3_class(output_sf_8, "tbl_df")
  
  
    ## CHECK 1.5
    output_conn_1 <- ddbs_rotate("argentina", 45, conn = conn_test)
    output_conn_2 <- ddbs_rotate("argentina", 45, conn = conn_test, units = "radians")
    output_conn_3 <- ddbs_rotate("argentina", 45, conn = conn_test, by_feature = TRUE)
    output_conn_4 <- ddbs_rotate("argentina", 45, conn = conn_test, by_feature = TRUE, center_x = 0, center_y = 0)
    output_conn_5 <- ddbs_rotate("argentina", 45, conn = conn_test, quiet = TRUE)
  
    testthat::expect_s3_class(output_conn_1, "duckspatial_df")
    testthat::expect_s3_class(output_conn_2, "duckspatial_df")
    testthat::expect_s3_class(output_conn_3, "duckspatial_df")
    testthat::expect_s3_class(output_conn_4, "duckspatial_df")
    testthat::expect_s3_class(output_conn_5, "duckspatial_df")

    ## CHECK 1.6
    output_conn_6 <- ddbs_rotate("argentina", 45, conn = conn_test, output = "geoarrow")
    output_conn_7 <- ddbs_rotate("argentina", 45, conn = conn_test, output = "sf")
    output_conn_8 <- ddbs_rotate("argentina", 45, conn = conn_test, output = "raw")

    testthat::expect_s3_class(output_conn_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_conn_7, "sf")
    testthat::expect_s3_class(output_conn_8, "tbl_df")
  
    ## CHECK 1.7
    testthat::expect_message(ddbs_rotate(argentina_ddbs, 50))
    testthat::expect_message(ddbs_rotate("argentina", 50, conn = conn_test, name = "rotated"))
    testthat::expect_message(ddbs_rotate("argentina", 50, conn = conn_test, name = "rotated", overwrite = TRUE))
    testthat::expect_true(ddbs_rotate("argentina", 50, conn = conn_test, name = "rotated2"))
  
  
    ## CHECK 1.8
    testthat::expect_no_message(ddbs_rotate(argentina_ddbs, 50, quiet = TRUE))
    testthat::expect_no_message(ddbs_rotate("argentina", 50, conn = conn_test, name = "rotated", overwrite = TRUE, quiet = TRUE))
    
})

## 1.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_rotate(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_rotate(argentina_ddbs))
    testthat::expect_error(ddbs_rotate(argentina_ddbs, by_feature = FALSE, center_x = 5, center_y = 5))
    testthat::expect_error(ddbs_rotate(argentina_ddbs, by_feature = TRUE, center_x = 5))
    testthat::expect_error(ddbs_rotate(argentina_ddbs, by_feature = TRUE, center_y = 5))
    testthat::expect_error(ddbs_rotate("argentina", conn = NULL))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_rotate(x = 999))
    testthat::expect_error(ddbs_rotate(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_rotate(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_rotate(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_rotate(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_rotate(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_rotate(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})


# 2. ddbs_rotate_3d ------------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
testthat::test_that("ddbs_rotate_3d(): expected behavior", {
    
    ## CHECK 1.1
    output_ddbs_1 <- ddbs_rotate_3d(argentina_ddbs, 45)
    output_ddbs_2 <- ddbs_rotate_3d(argentina_ddbs, 45, units = "radians")
    output_ddbs_3 <- ddbs_rotate_3d(argentina_ddbs, 90, axis = "y")
    output_ddbs_4 <- ddbs_rotate_3d(argentina_ddbs, 180, axis = "z")
    output_ddbs_5 <- ddbs_rotate_3d(argentina_ddbs, 45, quiet = TRUE)
  
    testthat::expect_s3_class(output_ddbs_1, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_2, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_3, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_4, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_5, "duckspatial_df")

    ## CHECK 1.2
    output_ddbs_6 <- ddbs_rotate_3d(argentina_ddbs, 45, output = "geoarrow")
    output_ddbs_7 <- ddbs_rotate_3d(argentina_ddbs, 45, output = "sf")
    output_ddbs_8 <- ddbs_rotate_3d(argentina_ddbs, 45, output = "raw")

    testthat::expect_s3_class(output_ddbs_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_ddbs_7, "sf")
    testthat::expect_s3_class(output_ddbs_8, "tbl_df")
  
    ## CHECK 1.3
    output_sf_1 <- ddbs_rotate_3d(argentina_sf, 45)
    output_sf_2 <- ddbs_rotate_3d(argentina_sf, 45, units = "radians")
    output_sf_3 <- ddbs_rotate_3d(argentina_sf, 90, axis = "y")
    output_sf_4 <- ddbs_rotate_3d(argentina_sf, 180, axis = "z")
    output_sf_5 <- ddbs_rotate_3d(argentina_sf, 45, quiet = TRUE)
  
    testthat::expect_s3_class(output_sf_1, "duckspatial_df")
    testthat::expect_s3_class(output_sf_2, "duckspatial_df")
    testthat::expect_s3_class(output_sf_3, "duckspatial_df")
    testthat::expect_s3_class(output_sf_4, "duckspatial_df")
    testthat::expect_s3_class(output_sf_5, "duckspatial_df")

  
    ## CHECK 1.4
    output_sf_6 <- ddbs_rotate_3d(argentina_sf, 45, output = "geoarrow")
    output_sf_7 <- ddbs_rotate_3d(argentina_sf, 45, output = "sf")
    output_sf_8 <- ddbs_rotate_3d(argentina_sf, 45, output = "raw")

    testthat::expect_s3_class(output_sf_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_7, "sf")
    testthat::expect_s3_class(output_sf_8, "tbl_df")
  
  
    ## CHECK 1.5    
    output_conn_1 <- ddbs_rotate_3d("argentina", 45, conn = conn_test)
    output_conn_2 <- ddbs_rotate_3d("argentina", 45, conn = conn_test, units = "radians")
    output_conn_3 <- ddbs_rotate_3d("argentina", 90, conn = conn_test, axis = "y")
    output_conn_4 <- ddbs_rotate_3d("argentina", 180, conn = conn_test, axis = "z")
    output_conn_5 <- ddbs_rotate_3d("argentina", 45, conn = conn_test, quiet = TRUE)
  
    testthat::expect_s3_class(output_conn_1, "duckspatial_df")
    testthat::expect_s3_class(output_conn_2, "duckspatial_df")
    testthat::expect_s3_class(output_conn_3, "duckspatial_df")
    testthat::expect_s3_class(output_conn_4, "duckspatial_df")
    testthat::expect_s3_class(output_conn_5, "duckspatial_df")

    ## CHECK 1.6
    output_conn_6 <- ddbs_rotate_3d("argentina", 45, conn = conn_test, output = "geoarrow")
    output_conn_7 <- ddbs_rotate_3d("argentina", 45, conn = conn_test, output = "sf")
    output_conn_8 <- ddbs_rotate_3d("argentina", 45, conn = conn_test, output = "raw")

    testthat::expect_s3_class(output_conn_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_conn_7, "sf")
    testthat::expect_s3_class(output_conn_8, "tbl_df")
  
    ## CHECK 1.7
    testthat::expect_message(ddbs_rotate_3d(argentina_ddbs, 50))
    testthat::expect_message(ddbs_rotate_3d("argentina", 50, conn = conn_test, name = "rotated_3d"))
    testthat::expect_message(ddbs_rotate_3d("argentina", 50, conn = conn_test, name = "rotated_3d", overwrite = TRUE))
    testthat::expect_true(ddbs_rotate_3d("argentina", 50, conn = conn_test, name = "rotated_3d2"))
  
  
    ## CHECK 1.8
    testthat::expect_no_message(ddbs_rotate_3d(argentina_ddbs, 50, quiet = TRUE))
    testthat::expect_no_message(ddbs_rotate_3d("argentina", 50, conn = conn_test, name = "rotated_3d", overwrite = TRUE, quiet = TRUE))
    
})



## 2.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_rotate_3d(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_rotate_3d(argentina_ddbs))
    testthat::expect_error(ddbs_rotate_3d(argentina_ddbs, units = "asdfasdf"))
    testthat::expect_error(ddbs_rotate_3d(argentina_ddbs, axis = "asdfasdf"))
    testthat::expect_error(ddbs_rotate_3d("argentina", conn = NULL))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_rotate_3d(x = 999))
    testthat::expect_error(ddbs_rotate_3d(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_rotate_3d(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_rotate_3d(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_rotate_3d(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_rotate_3d(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_rotate_3d(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})



# 3. ddbs_shift ------------------------------------------------------

## 3.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
testthat::test_that("ddbs_shift(): expected behavior", {
    
    ## CHECK 1.1
    output_ddbs_1 <- ddbs_shift(argentina_ddbs, 10)
    output_ddbs_2 <- ddbs_shift(argentina_ddbs, 10, 20)
    output_ddbs_3 <- ddbs_shift(argentina_ddbs, dy = 10)
    output_ddbs_4 <- ddbs_shift(argentina_ddbs, 45, quiet = TRUE)
  
    testthat::expect_s3_class(output_ddbs_1, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_2, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_3, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_4, "duckspatial_df")

    ## CHECK 1.2
    output_ddbs_6 <- ddbs_shift(argentina_ddbs, 45, output = "geoarrow")
    output_ddbs_7 <- ddbs_shift(argentina_ddbs, 45, output = "sf")
    output_ddbs_8 <- ddbs_shift(argentina_ddbs, 45, output = "raw")

    testthat::expect_s3_class(output_ddbs_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_ddbs_7, "sf")
    testthat::expect_s3_class(output_ddbs_8, "tbl_df")
  
    ## CHECK 1.3
    output_sf_1 <- ddbs_shift(argentina_sf, 10)
    output_sf_2 <- ddbs_shift(argentina_sf, 10, 20)
    output_sf_3 <- ddbs_shift(argentina_sf, dy = 10)
    output_sf_4 <- ddbs_shift(argentina_sf, 45, quiet = TRUE)
  
    testthat::expect_s3_class(output_sf_1, "duckspatial_df")
    testthat::expect_s3_class(output_sf_2, "duckspatial_df")
    testthat::expect_s3_class(output_sf_3, "duckspatial_df")
    testthat::expect_s3_class(output_sf_4, "duckspatial_df")

  
    ## CHECK 1.4
    output_sf_6 <- ddbs_shift(argentina_sf, 45, output = "geoarrow")
    output_sf_7 <- ddbs_shift(argentina_sf, 45, output = "sf")
    output_sf_8 <- ddbs_shift(argentina_sf, 45, output = "raw")

    testthat::expect_s3_class(output_sf_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_7, "sf")
    testthat::expect_s3_class(output_sf_8, "tbl_df")
  
  
    ## CHECK 1.5
    output_conn_1 <- ddbs_shift("argentina", 10, conn = conn_test)
    output_conn_2 <- ddbs_shift("argentina", 10, 20, conn = conn_test)
    output_conn_3 <- ddbs_shift("argentina", dy = 10, conn = conn_test)
    output_conn_4 <- ddbs_shift("argentina", 45, conn = conn_test, quiet = TRUE)
  
    testthat::expect_s3_class(output_conn_1, "duckspatial_df")
    testthat::expect_s3_class(output_conn_2, "duckspatial_df")
    testthat::expect_s3_class(output_conn_3, "duckspatial_df")
    testthat::expect_s3_class(output_conn_4, "duckspatial_df")

    ## CHECK 1.6
    output_conn_6 <- ddbs_shift("argentina", 45, conn = conn_test, output = "geoarrow")
    output_conn_7 <- ddbs_shift("argentina", 45, conn = conn_test, output = "sf")
    output_conn_8 <- ddbs_shift("argentina", 45, conn = conn_test, output = "raw")

    testthat::expect_s3_class(output_conn_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_conn_7, "sf")
    testthat::expect_s3_class(output_conn_8, "tbl_df")
  
    ## CHECK 1.7
    testthat::expect_message(ddbs_shift(argentina_ddbs, 50))
    testthat::expect_message(ddbs_shift("argentina", 50, conn = conn_test, name = "shift"))
    testthat::expect_message(ddbs_shift("argentina", 50, conn = conn_test, name = "shift", overwrite = TRUE))
    testthat::expect_true(ddbs_shift("argentina", 50, conn = conn_test, name = "shift2"))
  
  
    ## CHECK 1.8
    testthat::expect_no_message(ddbs_shift(argentina_ddbs, 50, quiet = TRUE))
    testthat::expect_no_message(ddbs_shift("argentina", 50, conn = conn_test, name = "shift", overwrite = TRUE, quiet = TRUE))
    
})



## 3.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_shift(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_shift(argentina_ddbs, dx = "10"))
    testthat::expect_error(ddbs_shift(argentina_ddbs, dy = "banana"))
    testthat::expect_error(ddbs_shift("argentina", conn = NULL))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_shift(x = 999))
    testthat::expect_error(ddbs_shift(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_shift(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_shift(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_shift(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_shift(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_shift(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})


# 4. ddbs_flip ------------------------------------------------------

## 4.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
testthat::test_that("ddbs_flip(): expected behavior", {
    
    ## CHECK 1.1
    output_ddbs_1 <- ddbs_flip(argentina_ddbs)
    output_ddbs_2 <- ddbs_flip(argentina_ddbs, "vertical")
    output_ddbs_3 <- ddbs_flip(nc_ddbs, by_feature = TRUE)
    output_ddbs_4 <- ddbs_flip(nc_ddbs, "vertical", by_feature = TRUE, quiet = TRUE)
  
    testthat::expect_s3_class(output_ddbs_1, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_2, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_3, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_4, "duckspatial_df")

    ## CHECK 1.2
    output_ddbs_6 <- ddbs_flip(argentina_ddbs, output = "geoarrow")
    output_ddbs_7 <- ddbs_flip(argentina_ddbs, output = "sf")
    output_ddbs_8 <- ddbs_flip(argentina_ddbs, output = "raw")

    testthat::expect_s3_class(output_ddbs_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_ddbs_7, "sf")
    testthat::expect_s3_class(output_ddbs_8, "tbl_df")
  
    ## CHECK 1.3
    output_sf_1 <- ddbs_flip(argentina_sf)
    output_sf_2 <- ddbs_flip(argentina_sf, "vertical")
    output_sf_3 <- ddbs_flip(nc_ddbs, by_feature = TRUE)
    output_sf_4 <- ddbs_flip(nc_sf, "vertical", by_feature = TRUE, quiet = TRUE)
  
    testthat::expect_s3_class(output_sf_1, "duckspatial_df")
    testthat::expect_s3_class(output_sf_2, "duckspatial_df")
    testthat::expect_s3_class(output_sf_3, "duckspatial_df")
    testthat::expect_s3_class(output_sf_4, "duckspatial_df")

  
    ## CHECK 1.4
    output_sf_6 <- ddbs_flip(argentina_sf, output = "geoarrow")
    output_sf_7 <- ddbs_flip(argentina_sf, output = "sf")
    output_sf_8 <- ddbs_flip(argentina_sf, output = "raw")

    testthat::expect_s3_class(output_sf_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_7, "sf")
    testthat::expect_s3_class(output_sf_8, "tbl_df")
  
  
    ## CHECK 1.5
    output_conn_1 <- ddbs_flip("nc", conn = conn_test)
    output_conn_2 <- ddbs_flip("nc", "vertical", conn = conn_test)
    output_conn_3 <- ddbs_flip("nc", by_feature = TRUE, conn = conn_test)
    output_conn_4 <- ddbs_flip("nc", "vertical", conn = conn_test, by_feature = TRUE, quiet = TRUE)
  
    testthat::expect_s3_class(output_conn_1, "duckspatial_df")
    testthat::expect_s3_class(output_conn_2, "duckspatial_df")
    testthat::expect_s3_class(output_conn_3, "duckspatial_df")
    testthat::expect_s3_class(output_conn_4, "duckspatial_df")

    ## CHECK 1.6
    output_conn_6 <- ddbs_flip("nc", conn = conn_test, output = "geoarrow")
    output_conn_7 <- ddbs_flip("nc", conn = conn_test, output = "sf")
    output_conn_8 <- ddbs_flip("nc", conn = conn_test, output = "raw")

    testthat::expect_s3_class(output_conn_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_conn_7, "sf")
    testthat::expect_s3_class(output_conn_8, "tbl_df")
  
    ## CHECK 1.7
    testthat::expect_message(ddbs_flip(nc_ddbs))
    testthat::expect_message(ddbs_flip("nc", conn = conn_test, name = "flip"))
    testthat::expect_message(ddbs_flip("nc", conn = conn_test, name = "flip", overwrite = TRUE))
    testthat::expect_true(ddbs_flip("nc", conn = conn_test, name = "flip2"))
  
  
    ## CHECK 1.8
    testthat::expect_no_message(ddbs_flip(argentina_ddbs, quiet = TRUE))
    testthat::expect_no_message(ddbs_flip("nc", conn = conn_test, name = "flip", overwrite = TRUE, quiet = TRUE))
    
})



## 4.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_flip(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_flip(argentina_ddbs, direction = "uptodown"))
    testthat::expect_error(ddbs_flip(argentina_ddbs, by_feature = "TRUE"))
    testthat::expect_error(ddbs_flip("argentina", conn = NULL))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_flip(x = 999))
    testthat::expect_error(ddbs_flip(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_flip(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_flip(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_flip(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_flip(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_flip(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})




# 5. ddbs_scale ------------------------------------------------------

## 5.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
testthat::test_that("ddbs_scale(): expected behavior", {
    
    ## CHECK 1.1
    output_ddbs_1 <- ddbs_scale(argentina_ddbs)
    output_ddbs_2 <- ddbs_scale(argentina_ddbs, y_scale = -1)
    output_ddbs_3 <- ddbs_scale(nc_ddbs, by_feature = TRUE)
  
    testthat::expect_s3_class(output_ddbs_1, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_2, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_3, "duckspatial_df")

    ## CHECK 1.2
    output_ddbs_6 <- ddbs_scale(argentina_ddbs, output = "geoarrow")
    output_ddbs_7 <- ddbs_scale(argentina_ddbs, output = "sf")
    output_ddbs_8 <- ddbs_scale(argentina_ddbs, output = "raw")

    testthat::expect_s3_class(output_ddbs_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_ddbs_7, "sf")
    testthat::expect_s3_class(output_ddbs_8, "tbl_df")
  
    ## CHECK 1.3
    output_sf_1 <- ddbs_scale(argentina_sf)
    output_sf_2 <- ddbs_scale(argentina_sf, y_scale = -1)
    output_sf_3 <- ddbs_scale(nc_ddbs, by_feature = TRUE)
  
    testthat::expect_s3_class(output_sf_1, "duckspatial_df")
    testthat::expect_s3_class(output_sf_2, "duckspatial_df")
    testthat::expect_s3_class(output_sf_3, "duckspatial_df")

  
    ## CHECK 1.4
    output_sf_6 <- ddbs_scale(argentina_sf, output = "geoarrow")
    output_sf_7 <- ddbs_scale(argentina_sf, output = "sf")
    output_sf_8 <- ddbs_scale(argentina_sf, output = "raw")

    testthat::expect_s3_class(output_sf_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_7, "sf")
    testthat::expect_s3_class(output_sf_8, "tbl_df")
  
  
    ## CHECK 1.5
    output_conn_1 <- ddbs_scale("nc", conn = conn_test)
    output_conn_2 <- ddbs_scale("nc", y_scale = -1, conn = conn_test)
    output_conn_3 <- ddbs_scale("nc", by_feature = TRUE, conn = conn_test)
  
    testthat::expect_s3_class(output_conn_1, "duckspatial_df")
    testthat::expect_s3_class(output_conn_2, "duckspatial_df")
    testthat::expect_s3_class(output_conn_3, "duckspatial_df")

    ## CHECK 1.6
    output_conn_6 <- ddbs_scale("nc", conn = conn_test, output = "geoarrow")
    output_conn_7 <- ddbs_scale("nc", conn = conn_test, output = "sf")
    output_conn_8 <- ddbs_scale("nc", conn = conn_test, output = "raw")

    testthat::expect_s3_class(output_conn_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_conn_7, "sf")
    testthat::expect_s3_class(output_conn_8, "tbl_df")
  
    ## CHECK 1.7
    testthat::expect_message(ddbs_scale(nc_ddbs))
    testthat::expect_message(ddbs_scale("nc", conn = conn_test, name = "shear"))
    testthat::expect_message(ddbs_scale("nc", conn = conn_test, name = "shear", overwrite = TRUE))
    testthat::expect_true(ddbs_scale("nc", conn = conn_test, name = "shear2"))
  
  
    ## CHECK 1.8
    testthat::expect_no_message(ddbs_scale(argentina_ddbs, quiet = TRUE))
    testthat::expect_no_message(ddbs_scale("nc", conn = conn_test, name = "shear", overwrite = TRUE, quiet = TRUE))
    
})



## 5.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_scale(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_scale(argentina_ddbs, x_scale = "23"))
    testthat::expect_error(ddbs_scale(argentina_ddbs, y_scale = "five"))
    testthat::expect_error(ddbs_scale(argentina_ddbs, by_feature = "TRUE"))
    testthat::expect_error(ddbs_scale("argentina", conn = NULL))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_scale(x = 999))
    testthat::expect_error(ddbs_scale(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_scale(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_scale(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_scale(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_scale(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_scale(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})




# 6. ddbs_shear ------------------------------------------------------

## 6.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
testthat::test_that("ddbs_shear(): expected behavior", {
    
    ## CHECK 1.1
    output_ddbs_1 <- ddbs_shear(argentina_ddbs)
    output_ddbs_2 <- ddbs_shear(argentina_ddbs, y_shear = -1)
    output_ddbs_3 <- ddbs_shear(nc_ddbs, by_feature = TRUE)
  
    testthat::expect_s3_class(output_ddbs_1, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_2, "duckspatial_df")
    testthat::expect_s3_class(output_ddbs_3, "duckspatial_df")

    ## CHECK 1.2
    output_ddbs_6 <- ddbs_shear(argentina_ddbs, output = "geoarrow")
    output_ddbs_7 <- ddbs_shear(argentina_ddbs, output = "sf")
    output_ddbs_8 <- ddbs_shear(argentina_ddbs, output = "raw")

    testthat::expect_s3_class(output_ddbs_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_ddbs_7, "sf")
    testthat::expect_s3_class(output_ddbs_8, "tbl_df")
  
    ## CHECK 1.3
    output_sf_1 <- ddbs_shear(argentina_sf)
    output_sf_2 <- ddbs_shear(argentina_sf, y_shear = -1)
    output_sf_3 <- ddbs_shear(nc_ddbs, by_feature = TRUE)
  
    testthat::expect_s3_class(output_sf_1, "duckspatial_df")
    testthat::expect_s3_class(output_sf_2, "duckspatial_df")
    testthat::expect_s3_class(output_sf_3, "duckspatial_df")

  
    ## CHECK 1.4
    output_sf_6 <- ddbs_shear(argentina_sf, output = "geoarrow")
    output_sf_7 <- ddbs_shear(argentina_sf, output = "sf")
    output_sf_8 <- ddbs_shear(argentina_sf, output = "raw")

    testthat::expect_s3_class(output_sf_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_7, "sf")
    testthat::expect_s3_class(output_sf_8, "tbl_df")
  
  
    ## CHECK 1.5
    output_conn_1 <- ddbs_shear("nc", conn = conn_test)
    output_conn_2 <- ddbs_shear("nc", y_shear = -1, conn = conn_test)
    output_conn_3 <- ddbs_shear("nc", by_feature = TRUE, conn = conn_test)
  
    testthat::expect_s3_class(output_conn_1, "duckspatial_df")
    testthat::expect_s3_class(output_conn_2, "duckspatial_df")
    testthat::expect_s3_class(output_conn_3, "duckspatial_df")

    ## CHECK 1.6
    output_conn_6 <- ddbs_shear("nc", conn = conn_test, output = "geoarrow")
    output_conn_7 <- ddbs_shear("nc", conn = conn_test, output = "sf")
    output_conn_8 <- ddbs_shear("nc", conn = conn_test, output = "raw")

    testthat::expect_s3_class(output_conn_6$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_conn_7, "sf")
    testthat::expect_s3_class(output_conn_8, "tbl_df")
  
    ## CHECK 1.7
    testthat::expect_message(ddbs_shear(nc_ddbs))
    testthat::expect_message(ddbs_shear("nc", conn = conn_test, name = "scale"))
    testthat::expect_message(ddbs_shear("nc", conn = conn_test, name = "scale", overwrite = TRUE))
    testthat::expect_true(ddbs_shear("nc", conn = conn_test, name = "scale2"))
  
  
    ## CHECK 1.8
    testthat::expect_no_message(ddbs_shear(argentina_ddbs, quiet = TRUE))
    testthat::expect_no_message(ddbs_shear("nc", conn = conn_test, name = "scale", overwrite = TRUE, quiet = TRUE))
    
})



## 6.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_shear(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_shear(argentina_ddbs, x_scale = "23"))
    testthat::expect_error(ddbs_shear(argentina_ddbs, y_shear = "five"))
    testthat::expect_error(ddbs_shear(argentina_ddbs, by_feature = "TRUE"))
    testthat::expect_error(ddbs_shear("argentina", conn = NULL))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_shear(x = 999))
    testthat::expect_error(ddbs_shear(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_shear(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_shear(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_shear(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_shear(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_shear(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)

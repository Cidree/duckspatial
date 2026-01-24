
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## create a random variable for quad keys
rand_sf <- sf::st_sample(argentina_sf, 100) |> sf::st_as_sf()
rand_sf["var"] <- runif(100)
rand_ddbs <- as_duckspatial_df(rand_sf)

## write data
duckspatial::ddbs_write_vector(conn_test, rand_sf, "points")


# 1. ddbs_quadkey() --------------------------------------------------------

## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all input formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, tbl, raster)
## - CHECK 1.3: messages work
#  - CHECK 1.4: "var" argument works
#  - CHECK 1.5: "fun" argument works
#  - CHECK 1.6: "background" argument works
testthat::test_that("ddbs_quadkey(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_quadkey(rand_ddbs, level = 1)
  output_sf   <- ddbs_quadkey(rand_sf, level = 1)
  output_conn <- ddbs_quadkey("points", conn = conn_test, level = 1)

  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_tbl_fmt    <- ddbs_quadkey(rand_ddbs, level = 5, output = "tilexy")
  output_raster_fmt <- ddbs_quadkey(rand_ddbs, level = 5, output = "raster")

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_s3_class(output_tbl_fmt, "tbl_df")
  testthat::expect_s4_class(output_raster_fmt, "SpatRaster")
  testthat::expect_true(
    all(terra::values(output_raster_fmt, mat = FALSE, na.rm = TRUE) == 1)
  )


  ## CHECK 1.3
  testthat::expect_message(ddbs_quadkey(rand_ddbs, level = 1))
  testthat::expect_message(ddbs_quadkey("points", conn = conn_test, name = "quadkey"))
  testthat::expect_message(ddbs_quadkey("points", conn = conn_test, name = "quadkey", overwrite = TRUE))
  testthat::expect_true(ddbs_quadkey("points", conn = conn_test, name = "quadkey2"))

  testthat::expect_no_message(ddbs_quadkey(rand_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_quadkey("points", conn = conn_test, name = "quadkey", overwrite = TRUE, quiet = TRUE))
    

  ## CHECK 1.4
  output_raster <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", output = "raster")
  testthat::expect_false(
    all(terra::values(output_raster, mat = FALSE, na.rm = TRUE) == 1)
  )


  ## CHECK 1.5
  output_raster_min <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", fun = "min", output = "raster")
  output_raster_max <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", fun = "max", output = "raster")

  testthat::expect_gt(
    terra::minmax(output_raster_max)[2],
    terra::minmax(output_raster_min)[2]
  )


  ## CHECK 1.6
  output_raster_bg <- ddbs_quadkey(
    rand_ddbs, level = 5, 
    field = "var", 
    background = 0, 
    output = "raster"
  )

  testthat::expect_equal(terra::minmax(output_raster_bg)[1], 0)
  testthat::expect_false(NA %in% terra::values(output_raster_bg, mat = FALSE))


})


## 1.2. Errors -------------------------

## CHECK 2.1: specific errors
## CHECK 2.2: general errors
testthat::test_that("ddbs_quadkey(): errors work", {
  
  ## CHECK 2.1
  ## - doesn't work with polygons or lines
  testthat::expect_error(ddbs_quadkey(argentina_ddbs))
  testthat::expect_error(ddbs_quadkey(rivers_ddbs))
  ## - invalid arguments
  testthat::expect_error(ddbs_quadkey(points_ddbs, level = 0))
  testthat::expect_error(ddbs_quadkey(points_ddbs, level = 100))
  testthat::expect_error(ddbs_quadkey(points_ddbs, level = "10"))
  testthat::expect_error(ddbs_quadkey(points_ddbs, level = FALSE))
  testthat::expect_error(ddbs_quadkey(points_ddbs, field = 2))
  testthat::expect_error(ddbs_quadkey(points_ddbs, field = TRUE))
  testthat::expect_error(ddbs_quadkey(points_ddbs, fun = TRUE, output = "raster"))
  testthat::expect_error(ddbs_quadkey(points_ddbs, fun = 27, output = "raster"))
  testthat::expect_error(ddbs_quadkey(points_ddbs, fun = 27, output = "banada"))


  ## CHECK 2.2.
  testthat::expect_error(ddbs_quadkey(x = 999))
  testthat::expect_error(ddbs_quadkey(points_ddbs, conn = 999))
  testthat::expect_error(ddbs_quadkey(points_ddbs, overwrite = 999))
  testthat::expect_error(ddbs_quadkey(points_ddbs, quiet = 999))
  testthat::expect_error(ddbs_quadkey(x = "999", conn = conn_test))
  testthat::expect_error(ddbs_quadkey(points_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})


# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write data
duckspatial::ddbs_write_vector(conn_test, points_sf, "points")
duckspatial::ddbs_write_vector(conn_test, argentina_sf, "argentina")
duckspatial::ddbs_write_vector(conn_test, countries_sf, "countries")


# 1. ddbs_buffer() -------------------------------------------------------

## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: compare to sf
testthat::test_that("ddbs_buffer(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_buffer(points_ddbs, 50)
  output_sf   <- ddbs_buffer(points_sf, 50)
  output_conn <- ddbs_buffer("points", 50, conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_buffer(points_ddbs, 10, output = "geoarrow")
  output_sf_fmt       <- ddbs_buffer(points_ddbs, 10, output = "sf")
  output_raw_fmt      <- ddbs_buffer(points_ddbs, 10, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_buffer(points_ddbs, 10))
  testthat::expect_message(ddbs_buffer("points", 50, conn = conn_test, name = "buffer"))
  testthat::expect_message(ddbs_buffer("points", 50, conn = conn_test, name = "buffer", overwrite = TRUE))
  testthat::expect_true(ddbs_buffer("points", 50, conn = conn_test, name = "buffer2"))

  testthat::expect_no_message(ddbs_buffer(points_ddbs, 50, quiet = TRUE))
  testthat::expect_no_message(ddbs_buffer("points", 50, conn = conn_test, name = "buffer", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "buffer")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )

  ## CHECK 1.5
  point_planar <- sf::st_transform(points_sf[1, ], "EPSG:3857")
  sf_output   <- sf::st_buffer(point_planar, 100, nQuadSegs = 8)
  ddbs_output <- ddbs_buffer(point_planar, 100) |> 
    sf::st_as_sf() |> 
    dplyr::select(-crs_duckspatial)

  testthat::expect_equal(sf_output$geometry, ddbs_output$geometry)
    
})

## 1.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_buffer(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_buffer(points_ddbs))
    testthat::expect_error(ddbs_buffer(points_ddbs, distance = "12"))
    testthat::expect_error(ddbs_buffer("points", conn = NULL))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_buffer(x = 999))
    testthat::expect_error(ddbs_buffer(points_ddbs, conn = 999))
    testthat::expect_error(ddbs_buffer(points_ddbs, new_column = 999))
    testthat::expect_error(ddbs_buffer(points_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_buffer(points_ddbs, quiet = 999))
    testthat::expect_error(ddbs_buffer(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_buffer(points_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})

# 2. ddbs_centroid() -----------------------------------------------------


## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
#  - CHECK 1.4: writting table works
## TODO - Review differences with sf::st_centroid()
testthat::test_that("ddbs_centroid(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_centroid(argentina_ddbs)
  output_sf   <- ddbs_centroid(argentina_sf)
  output_conn <- ddbs_centroid("argentina", conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_centroid(argentina_ddbs, output = "geoarrow")
  output_sf_fmt       <- ddbs_centroid(argentina_ddbs, output = "sf")
  output_raw_fmt      <- ddbs_centroid(argentina_ddbs, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_centroid(argentina_ddbs))
  testthat::expect_message(ddbs_centroid("argentina", conn = conn_test, name = "centroid"))
  testthat::expect_message(ddbs_centroid("argentina", conn = conn_test, name = "centroid", overwrite = TRUE))
  testthat::expect_true(ddbs_centroid("argentina", conn = conn_test, name = "centroid2"))

  testthat::expect_no_message(ddbs_centroid(argentina_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_centroid("argentina", conn = conn_test, name = "centroid", overwrite = TRUE, quiet = TRUE))
    
  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "centroid")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )

})

## 2.2. Errors -------------------------

## CHECK 2.1: errors
testthat::test_that("ddbs_centroid(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_centroid("argentina", conn = NULL))
    testthat::expect_error(ddbs_centroid(x = 999))
    testthat::expect_error(ddbs_centroid(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_centroid(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_centroid(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_centroid(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_centroid(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_centroid(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})


# 3. ddbs_is_valid() -----------------------------------------------------

## 3.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: vector works on all formats
## - CHECK 1.2: data frame works on all formats
## - CHECK 1.3: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.4: messages work
## - CHECK 1.5: compare with SF
testthat::test_that("ddbs_is_valid(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs_vec <- ddbs_is_valid(countries_ddbs)
  output_sf_vec   <- ddbs_is_valid(countries_sf)
  output_conn_vec <- ddbs_is_valid("countries", conn = conn_test)

  testthat::expect_type(output_ddbs_vec, "logical")
  testthat::expect_equal(output_ddbs_vec, output_sf_vec)
  testthat::expect_equal(output_ddbs_vec, output_conn_vec)

  ## CHECK 1.2
  output_ddbs <- ddbs_is_valid(countries_ddbs, new_column = "is_valid")
  output_sf   <- ddbs_is_valid(countries_sf, new_column = "is_valid")
  output_conn <- ddbs_is_valid("countries", conn = conn_test, new_column = "is_valid")

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.3
  output_geoarrow_fmt <- ddbs_is_valid(countries_ddbs, new_column = "is_valid", output = "geoarrow")
  output_sf_fmt       <- ddbs_is_valid(countries_ddbs, new_column = "is_valid", output = "sf")
  output_raw_fmt      <- ddbs_is_valid(countries_ddbs, new_column = "is_valid", output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_is_valid(countries_ddbs))
  testthat::expect_message(ddbs_is_valid("countries", new_column = "is_valid", conn = conn_test, name = "is_valid_tbl"))
  testthat::expect_message(ddbs_is_valid("countries", new_column = "is_valid", conn = conn_test, name = "is_valid_tbl", overwrite = TRUE))
  testthat::expect_true(ddbs_is_valid("countries", new_column = "is_valid", conn = conn_test, name = "is_valid_tbl2"))

  testthat::expect_no_message(ddbs_is_valid(countries_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_is_valid("countries", new_column = "is_valid", conn = conn_test, name = "is_valid_tbl", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "is_valid_tbl")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )


  ## CHECK 1.5
  sf_output <- sf::st_is_valid(countries_sf)
  testthat::expect_equal(output_ddbs_vec, sf_output)
    
})

## 3.2. Errors -------------------------

## CHECK 2.1: function specific errors
## CHECK 2.2: other errors
testthat::test_that("ddbs_is_valid(): errors work", {

  ## CHECK 2.1
  testthat::expect_error(ddbs_is_valid(countries_sf, conn = conn_test, name = "tbl"))
  testthat::expect_error(ddbs_is_valid(countries_sf, new_column = 5))


  ## CHECK 2.2
  testthat::expect_error(ddbs_is_valid("countries", conn = NULL))
  testthat::expect_error(ddbs_is_valid(x = 999))
  testthat::expect_error(ddbs_is_valid(countries_ddbs, conn = 999))
  testthat::expect_error(ddbs_is_valid(countries_ddbs, new_column = 999))
  testthat::expect_error(ddbs_is_valid(countries_ddbs, overwrite = 999))
  testthat::expect_error(ddbs_is_valid(countries_ddbs, quiet = 999))
  testthat::expect_error(ddbs_is_valid(x = "999", conn = conn_test))
  testthat::expect_error(ddbs_is_valid(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})



# 4. ddbs_is_simple() -----------------------------------------------------

## 4.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: vector works on all formats
## - CHECK 1.2: data frame works on all formats
## - CHECK 1.3: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.4: messages work
## - CHECK 1.5: compare with SF
testthat::test_that("ddbs_is_simple(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs_vec <- ddbs_is_simple(countries_ddbs)
  output_sf_vec   <- ddbs_is_simple(countries_sf)
  output_conn_vec <- ddbs_is_simple("countries", conn = conn_test)

  testthat::expect_type(output_ddbs_vec, "logical")
  testthat::expect_equal(output_ddbs_vec, output_sf_vec)
  testthat::expect_equal(output_ddbs_vec, output_conn_vec)

  ## CHECK 1.2
  output_ddbs <- ddbs_is_simple(countries_ddbs, new_column = "is_simple")
  output_sf   <- ddbs_is_simple(countries_sf, new_column = "is_simple")
  output_conn <- ddbs_is_simple("countries", conn = conn_test, new_column = "is_simple")

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.3
  output_geoarrow_fmt <- ddbs_is_simple(countries_ddbs, new_column = "is_simple", output = "geoarrow")
  output_sf_fmt       <- ddbs_is_simple(countries_ddbs, new_column = "is_simple", output = "sf")
  output_raw_fmt      <- ddbs_is_simple(countries_ddbs, new_column = "is_simple", output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_is_simple(countries_ddbs))
  testthat::expect_message(ddbs_is_simple("countries", new_column = "is_simple", conn = conn_test, name = "is_simple_tbl"))
  testthat::expect_message(ddbs_is_simple("countries", new_column = "is_simple", conn = conn_test, name = "is_simple_tbl", overwrite = TRUE))
  testthat::expect_true(ddbs_is_simple("countries", new_column = "is_simple", conn = conn_test, name = "is_simple_tbl2"))

  testthat::expect_no_message(ddbs_is_simple(countries_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_is_simple("countries", new_column = "is_simple", conn = conn_test, name = "is_simple_tbl", overwrite = TRUE, quiet = TRUE))


  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "is_simple_tbl")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )


  ## CHECK 1.5
  sf_output <- sf::st_is_simple(countries_sf)
  testthat::expect_equal(output_ddbs_vec, sf_output)
    
})

## 4.2. Errors -------------------------

## CHECK 2.1: function specific errors
## CHECK 2.2: other errors
testthat::test_that("ddbs_is_simple(): errors work", {

  ## CHECK 2.1
  testthat::expect_error(ddbs_is_simple(countries_sf, conn = conn_test, name = "tbl"))
  testthat::expect_error(ddbs_is_simple(countries_sf, new_column = 5))


  ## CHECK 2.2
  testthat::expect_error(ddbs_is_simple("countries", conn = NULL))
  testthat::expect_error(ddbs_is_simple(x = 999))
  testthat::expect_error(ddbs_is_simple(countries_ddbs, conn = 999))
  testthat::expect_error(ddbs_is_simple(countries_ddbs, new_column = 999))
  testthat::expect_error(ddbs_is_simple(countries_ddbs, overwrite = 999))
  testthat::expect_error(ddbs_is_simple(countries_ddbs, quiet = 999))
  testthat::expect_error(ddbs_is_simple(x = "999", conn = conn_test))
  testthat::expect_error(ddbs_is_simple(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})


# 5. ddbs_make_valid() -----------------------------------------------------

## 5.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
testthat::test_that("ddbs_make_valid(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_make_valid(argentina_ddbs)
  output_sf   <- ddbs_make_valid(argentina_sf)
  output_conn <- ddbs_make_valid("argentina", conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_make_valid(argentina_ddbs, output = "geoarrow")
  output_sf_fmt       <- ddbs_make_valid(argentina_ddbs, output = "sf")
  output_raw_fmt      <- ddbs_make_valid(argentina_ddbs, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_make_valid(argentina_ddbs))
  testthat::expect_message(ddbs_make_valid("argentina", conn = conn_test, name = "make_valid"))
  testthat::expect_message(ddbs_make_valid("argentina", conn = conn_test, name = "make_valid", overwrite = TRUE))
  testthat::expect_true(ddbs_make_valid("argentina", conn = conn_test, name = "make_valid2"))

  testthat::expect_no_message(ddbs_make_valid(argentina_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_make_valid("argentina", conn = conn_test, name = "make_valid", overwrite = TRUE, quiet = TRUE))
    

  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "make_valid")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )

})

## 5.2. Errors -------------------------

## CHECK 2.1: errors
testthat::test_that("ddbs_make_valid(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_make_valid("argentina", conn = NULL))
    testthat::expect_error(ddbs_make_valid(x = 999))
    testthat::expect_error(ddbs_make_valid(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_make_valid(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_make_valid(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_make_valid(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_make_valid(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_make_valid(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})




# 6. ddbs_simplify() -----------------------------------------------------


## 6.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
#  - CHECK 1.4: writting table works
testthat::test_that("ddbs_simplify(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_simplify(argentina_ddbs, tolerance = 0.01)
  output_sf   <- ddbs_simplify(argentina_sf, tolerance = 0.01)
  output_conn <- ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_simplify(argentina_ddbs, tolerance = 0.01, output = "geoarrow")
  output_sf_fmt       <- ddbs_simplify(argentina_ddbs, tolerance = 0.01, output = "sf")
  output_raw_fmt      <- ddbs_simplify(argentina_ddbs, tolerance = 0.01, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_simplify(argentina_ddbs, tolerance = 0.01))
  testthat::expect_message(ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test, name = "simplify"))
  testthat::expect_message(ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test, name = "simplify", overwrite = TRUE))
  testthat::expect_true(ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test, name = "simplify2"))

  testthat::expect_no_message(ddbs_simplify(argentina_ddbs, tolerance = 0.01, quiet = TRUE))
  testthat::expect_no_message(ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test, name = "simplify", overwrite = TRUE, quiet = TRUE))
    
  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "simplify")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )

})

## 6.2. Errors -------------------------

## CHECK 2.1: errors
testthat::test_that("ddbs_simplify(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_simplify(argentina_ddbs))
    testthat::expect_error(ddbs_simplify("argentina", conn = NULL))
    testthat::expect_error(ddbs_simplify(x = 999))
    testthat::expect_error(ddbs_simplify(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_simplify(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_simplify(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_simplify(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_simplify(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_simplify(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})


# 7. ddbs_exterior_ring() -----------------------------------------------------

## 7.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1.5: geometry type
testthat::test_that("ddbs_exterior_ring(): expected behavior", {
  
  ## CHECK 1.1
  output_ddbs <- ddbs_exterior_ring(argentina_ddbs)
  output_sf   <- ddbs_exterior_ring(argentina_sf)
  output_conn <- ddbs_exterior_ring("argentina", conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_exterior_ring(argentina_ddbs, output = "geoarrow")
  output_sf_fmt       <- ddbs_exterior_ring(argentina_ddbs, output = "sf")
  output_raw_fmt      <- ddbs_exterior_ring(argentina_ddbs, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_exterior_ring(argentina_ddbs))
  testthat::expect_message(ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring"))
  testthat::expect_message(ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring", overwrite = TRUE))
  testthat::expect_true(ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring2"))

  testthat::expect_no_message(ddbs_exterior_ring(argentina_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring", overwrite = TRUE, quiet = TRUE))
    

  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "exterior_ring")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )


  ## CHECK 1.5
  geom_type <- ddbs_collect(output_ddbs) |> sf::st_geometry_type() |> as.character()
  testthat::expect_equal(geom_type, "LINESTRING")

})

## 7.2. Errors -------------------------

## CHECK 2.1: errors
testthat::test_that("ddbs_exterior_ring(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_exterior_ring("argentina", conn = NULL))
    testthat::expect_error(ddbs_exterior_ring(x = 999))
    testthat::expect_error(ddbs_exterior_ring(argentina_ddbs, conn = 999))
    testthat::expect_error(ddbs_exterior_ring(argentina_ddbs, new_column = 999))
    testthat::expect_error(ddbs_exterior_ring(argentina_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_exterior_ring(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_exterior_ring(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_exterior_ring(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})




# 8. ddbs_make_polygon() -----------------------------------------------------

## 8.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1.5: geometry type
testthat::test_that("ddbs_make_polygon(): expected behavior", {

  ## create a linestring object
  ext_ring_ddbs <- ddbs_exterior_ring(argentina_ddbs)
  ext_ring_sf <- st_as_sf(ext_ring_ddbs)
  
  ## CHECK 1.1
  output_ddbs <- ddbs_make_polygon(ext_ring_ddbs)
  output_sf   <- ddbs_make_polygon(ext_ring_sf)
  output_conn <- ddbs_make_polygon("exterior_ring", conn = conn_test)

  testthat::expect_s3_class(output_ddbs, "duckspatial_df")
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
  testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
  

  ## CHECK 1.2
  output_geoarrow_fmt <- ddbs_make_polygon(ext_ring_ddbs, output = "geoarrow")
  output_sf_fmt       <- ddbs_make_polygon(ext_ring_ddbs, output = "sf")
  output_raw_fmt      <- ddbs_make_polygon(ext_ring_ddbs, output = "raw")

  testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
  testthat::expect_s3_class(output_sf_fmt, "sf")
  testthat::expect_s3_class(output_raw_fmt, "tbl_df")


  ## CHECK 1.3
  testthat::expect_message(ddbs_make_polygon(ext_ring_ddbs))
  testthat::expect_message(ddbs_make_polygon("exterior_ring", conn = conn_test, name = "make_polygon"))
  testthat::expect_message(ddbs_make_polygon("exterior_ring", conn = conn_test, name = "make_polygon", overwrite = TRUE))
  testthat::expect_true(ddbs_make_polygon("exterior_ring", conn = conn_test, name = "make_polygon2"))

  testthat::expect_no_message(ddbs_make_polygon(ext_ring_ddbs, quiet = TRUE))
  testthat::expect_no_message(ddbs_make_polygon("exterior_ring", conn = conn_test, name = "make_polygon", overwrite = TRUE, quiet = TRUE))
    

  ## CHECK 1.4
  output_tbl <- ddbs_read_vector(conn_test, "make_polygon")
  testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
    output_tbl$geometry
  )


  ## CHECK 1.5
  geom_type <- ddbs_collect(output_ddbs) |> sf::st_geometry_type() |> as.character()
  testthat::expect_equal(geom_type, "POLYGON")

})

## 8.2. Errors -------------------------

## CHECK 2.1: function specific errors
## CHECK 2.2: other errors
testthat::test_that("ddbs_make_polygon(): errors work", {

    ## CHECK 2.1
     testthat::expect_error(ddbs_make_polygon(argentina_ddbs))
  
    ## CHECK 2.2
    testthat::expect_error(ddbs_make_polygon("ext_ring", conn = NULL))
    testthat::expect_error(ddbs_make_polygon(x = 999))
    testthat::expect_error(ddbs_make_polygon(ext_ring_ddbs, conn = 999))
    testthat::expect_error(ddbs_make_polygon(ext_ring_ddbs, new_column = 999))
    testthat::expect_error(ddbs_make_polygon(ext_ring_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_make_polygon(ext_ring_ddbs, quiet = 999))
    testthat::expect_error(ddbs_make_polygon(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_make_polygon(ext_ring_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})


## stop connection
ddbs_stop_conn(conn_test)

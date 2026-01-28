
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## insert data
ddbs_write_vector(conn_test, countries_sf, "countries")


# 1. ddbs_as_text() ------------------------------------------------------


## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (we can't because DuckDB retrieves more decimals)
testthat::test_that("ddbs_as_text(): expected behaviour", {

  ## CHECK 1.1
  output_sf   <- ddbs_as_text(countries_sf)
  output_ddbs <- ddbs_as_text(countries_ddbs)
  output_conn <- ddbs_as_text("countries", conn_test)

  testthat::expect_equal(output_sf, output_ddbs)
  testthat::expect_equal(output_sf, output_conn)

  ## CHECK 1.2.
  testthat::expect_message(ddbs_as_text(countries_sf))
  testthat::expect_no_message(ddbs_as_text(countries_sf, quiet = TRUE))

})

## 1.2. Errors -------------------------

## CHECK 2.1: errors
testthat::test_that("ddbs_as_text(): errors work", {

    testthat::expect_error(ddbs_as_text(x = 999))
    testthat::expect_error(ddbs_as_text(countries_ddbs, conn = 999))
    testthat::expect_error(ddbs_as_text(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_as_text(x = "999", conn = conn_test))
  
})



# 2. ddbs_as_wkb ---------------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (the class is different, so we compare the first 
## and last elements)
testthat::test_that("ddbs_as_wkb(): expected behaviour", {

  ## CHECK 1.1
  output_sf   <- ddbs_as_wkb(countries_sf)
  output_ddbs <- ddbs_as_wkb(countries_ddbs)
  output_conn <- ddbs_as_wkb("countries", conn_test)

  testthat::expect_equal(output_sf, output_ddbs)
  testthat::expect_equal(output_sf, output_conn)

  ## CHECK 1.2.
  testthat::expect_message(ddbs_as_wkb(countries_sf))
  testthat::expect_no_message(ddbs_as_wkb(countries_sf, quiet = TRUE))

  ## CHECK 1.3
  sf_output <- sf::st_as_binary(countries_sf$geometry)

  testthat::expect_equal(output_sf[[1]], sf_output[[1]])
  testthat::expect_equal(length(output_sf), length(sf_output))
  testthat::expect_equal(output_sf[[length(output_sf)]], sf_output[[length(sf_output)]])

})

## 2.2. Errors -------------------------

## CHECK 2.1: errors
testthat::test_that("ddbs_as_wkb(): errors work", {

    testthat::expect_error(ddbs_as_wkb(x = 999))
    testthat::expect_error(ddbs_as_wkb(countries_ddbs, conn = 999))
    testthat::expect_error(ddbs_as_wkb(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_as_wkb(x = "999", conn = conn_test))
  
})



# 3. ddbs_as_hexwkb() ----------------------------------------------------

## 3.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (the class is different, so we compare the first 
## and last elements)
testthat::test_that("ddbs_as_hexwkb(): expected behaviour", {

  ## CHECK 1.1
  output_sf   <- ddbs_as_hexwkb(countries_sf)
  output_ddbs <- ddbs_as_hexwkb(countries_ddbs)
  output_conn <- ddbs_as_hexwkb("countries", conn_test)

  testthat::expect_equal(output_sf, output_ddbs)
  testthat::expect_equal(output_sf, output_conn)

  ## CHECK 1.2.
  testthat::expect_message(ddbs_as_hexwkb(countries_sf))
  testthat::expect_no_message(ddbs_as_hexwkb(countries_sf, quiet = TRUE))

  ## CHECK 1.3
  ## - convert duckspatial result to lower case
  sf_output <- sf::st_as_binary(countries_sf$geometry, hex = T)

  output_sf_lower <- lapply(output_sf, tolower)

  testthat::expect_equal(output_sf_lower[[1]], sf_output[[1]])
  testthat::expect_equal(length(output_sf_lower), length(sf_output))
  testthat::expect_equal(output_sf_lower[[length(output_sf_lower)]], sf_output[[length(sf_output)]])

})

## 2.2. Errors -------------------------

## CHECK 2.1: errors
testthat::test_that("ddbs_as_hexwkb(): errors work", {

    testthat::expect_error(ddbs_as_hexwkb(x = 999))
    testthat::expect_error(ddbs_as_hexwkb(countries_ddbs, conn = 999))
    testthat::expect_error(ddbs_as_hexwkb(argentina_ddbs, quiet = 999))
    testthat::expect_error(ddbs_as_hexwkb(x = "999", conn = conn_test))
  
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)
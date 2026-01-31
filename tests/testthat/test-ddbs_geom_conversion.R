
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

## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (we can't because DuckDB retrieves more decimals)
## - CHECK 2.1: errors
describe("ddbs_as_text()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------
  describe("expected behavior", {

    it("works on all input formats", {
      output_sf   <- ddbs_as_text(countries_sf)
      output_ddbs <- ddbs_as_text(countries_ddbs)
      output_conn <- ddbs_as_text("countries", conn_test)

      expect_equal(output_sf, output_ddbs)
      expect_equal(output_sf, output_conn)
    })

    it("shows and suppresses messages correctly", {
      expect_message(ddbs_as_text(countries_sf))
      expect_no_message(ddbs_as_text(countries_sf, quiet = TRUE))
    })

  })

  ### ERRORS ------------------------------------------------------------
  describe("errors work", {

    it("throws errors for invalid inputs", {
      expect_error(ddbs_as_text(x = 999))
      expect_error(ddbs_as_text(countries_ddbs, conn = 999))
      expect_error(ddbs_as_text(argentina_ddbs, quiet = 999))
      expect_error(ddbs_as_text(x = "999", conn = conn_test))
    })

  })

})



# 2. ddbs_as_wkb ---------------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (the class is different, so we compare the first 
## and last elements)
## - CHECK 2.1: errors
describe("ddbs_as_wkb()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------
  describe("expected behavior", {

    it("works on all input formats and produces consistent WKB", {
      output_sf   <- ddbs_as_wkb(countries_sf)
      output_ddbs <- ddbs_as_wkb(countries_ddbs)
      output_conn <- ddbs_as_wkb("countries", conn_test)

      expect_equal(output_sf, output_ddbs)
      expect_equal(output_sf, output_conn)
    })

    it("shows and suppresses messages correctly", {
      expect_message(ddbs_as_wkb(countries_sf))
      expect_no_message(ddbs_as_wkb(countries_sf, quiet = TRUE))
    })

    it("matches SF WKB output at first and last elements", {
      sf_output <- sf::st_as_binary(countries_sf$geometry)
      output_sf   <- ddbs_as_wkb(countries_sf)

      expect_equal(output_sf[[1]], sf_output[[1]])
      expect_equal(length(output_sf), length(sf_output))
      expect_equal(output_sf[[length(output_sf)]], sf_output[[length(sf_output)]])
    })

  })

  ### ERRORS ------------------------------------------------------------
  describe("errors work", {

    it("throws errors for invalid inputs", {
      expect_error(ddbs_as_wkb(x = 999))
      expect_error(ddbs_as_wkb(countries_ddbs, conn = 999))
      expect_error(ddbs_as_wkb(argentina_ddbs, quiet = 999))
      expect_error(ddbs_as_wkb(x = "999", conn = conn_test))
    })

  })

})


# 3. ddbs_as_hexwkb() ----------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (the class is different, so we compare the first 
## and last elements)
## - CHECK 2.1: errors
describe("ddbs_as_hexwkb()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------
  describe("expected behavior", {

    it("works on all input formats and produces consistent HEX WKB", {
      output_sf   <- ddbs_as_hexwkb(countries_sf)
      output_ddbs <- ddbs_as_hexwkb(countries_ddbs)
      output_conn <- ddbs_as_hexwkb("countries", conn_test)

      expect_equal(output_sf, output_ddbs)
      expect_equal(output_sf, output_conn)
    })

    it("shows and suppresses messages correctly", {
      expect_message(ddbs_as_hexwkb(countries_sf))
      expect_no_message(ddbs_as_hexwkb(countries_sf, quiet = TRUE))
    })

    it("matches SF HEX WKB output at first and last elements", {
      sf_output <- sf::st_as_binary(countries_sf$geometry, hex = TRUE)
      output_sf   <- ddbs_as_hexwkb(countries_sf)
      output_sf_lower <- lapply(output_sf, tolower)

      expect_equal(output_sf_lower[[1]], sf_output[[1]])
      expect_equal(length(output_sf_lower), length(sf_output))
      expect_equal(output_sf_lower[[length(output_sf_lower)]], sf_output[[length(sf_output)]])
    })

  })

  ### ERRORS ------------------------------------------------------------
  describe("errors work", {

    it("throws errors for invalid inputs", {
      expect_error(ddbs_as_hexwkb(x = 999))
      expect_error(ddbs_as_hexwkb(countries_ddbs, conn = 999))
      expect_error(ddbs_as_hexwkb(argentina_ddbs, quiet = 999))
      expect_error(ddbs_as_hexwkb(x = "999", conn = conn_test))
    })

  })

})



## stop connection
duckspatial::ddbs_stop_conn(conn_test)

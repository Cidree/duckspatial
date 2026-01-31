
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write data
duckspatial::ddbs_write_vector(conn_test, countries_sf, "countries")


# 1. ddbs_is_valid() -----------------------------------------------------

## - CHECK 1.1: vector works on all formats
## - CHECK 1.2: messages work
## - CHECK 1.3: compare with SF
## - CHECK 2.1: errors
describe("ddbs_is_valid()", {
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats", {
      output_ddbs_vec <- ddbs_is_valid(countries_ddbs)
      output_sf_vec   <- ddbs_is_valid(countries_sf)
      output_conn_vec <- ddbs_is_valid("countries", conn = conn_test)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })

    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_valid(countries_ddbs))
      expect_no_message(ddbs_is_valid(countries_ddbs, quiet = TRUE))
    })
    
    it("matches sf::st_is_valid results", {
      output_ddbs_vec <- ddbs_is_valid(countries_ddbs)
      sf_output <- sf::st_is_valid(countries_sf)
      
      expect_equal(output_ddbs_vec, sf_output)
    })
  })
  
  describe("errors", {
      
    it("requires connection when using table names", {
      expect_error(ddbs_is_valid("countries", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_is_valid(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_is_valid(countries_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_is_valid(countries_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_is_valid(countries_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_is_valid(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_is_valid(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })

  })
})



# 2. ddbs_is_simple() -----------------------------------------------------

## - CHECK 1.1: vector works on all formats
## - CHECK 1.2: messages work
## - CHECK 1.3: compare with SF
## - CHECK 2.1: errors

describe("ddbs_is_simple()", {

  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats", {
      output_ddbs_vec <- ddbs_is_simple(countries_ddbs)
      output_sf_vec   <- ddbs_is_simple(countries_sf)
      output_conn_vec <- ddbs_is_simple("countries", conn = conn_test)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_simple(countries_ddbs))
      expect_no_message(ddbs_is_simple(countries_ddbs, quiet = TRUE))
    })
    
    it("matches sf::st_is_simple results", {
      output_ddbs_vec <- ddbs_is_simple(countries_ddbs)
      sf_output <- sf::st_is_simple(countries_sf)
      
      expect_equal(output_ddbs_vec, sf_output)
    })
  })


  ### EXPECTED ERRORS
  
  describe("errors", {
      
    it("requires connection when using table names", {
      expect_error(ddbs_is_simple("countries", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_is_simple(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_is_simple(countries_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_is_simple(countries_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_is_simple(countries_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_is_simple(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_is_simple(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })

  })
})

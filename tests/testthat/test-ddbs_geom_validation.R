
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## read countries
countries_2_sf <- sf::read_sf(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
) |> 
    sf::st_transform("EPSG:4326")

## add an empty polygon to countries
countries_2_sf[nrow(countries_2_sf) + 1, ] <- countries_2_sf[nrow(countries_2_sf) + 1, ] 
countries_2_ddbs <- countries_2_sf

## write data
duckspatial::ddbs_write_vector(conn_test, countries_2_sf, "countries")


# 1. ddbs_is_valid() -----------------------------------------------------

## - CHECK 1.1: vector works on all formats
## - CHECK 1.2: messages work
## - CHECK 1.3: compare with SF
## - CHECK 2.1: errors
describe("ddbs_is_valid()", {
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats", {
      output_ddbs_vec <- ddbs_is_valid(countries_2_ddbs)
      output_sf_vec   <- ddbs_is_valid(countries_2_sf)
      output_conn_vec <- ddbs_is_valid("countries", conn = conn_test)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })

    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_valid(countries_2_ddbs))
      expect_no_message(ddbs_is_valid(countries_2_ddbs, quiet = TRUE))
    })
    
    it("matches sf::st_is_valid results", {
      ## Review why Antarctica is not valid in Duckdb, but it's valid in sf
      output_ddbs_vec <- ddbs_is_valid(countries_2_ddbs)
      sf_output <- sf::st_is_valid(countries_2_sf)
      
      expect_equal(output_ddbs_vec[-4], sf_output[-4])
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
      expect_error(ddbs_is_valid(countries_2_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_is_valid(countries_2_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_is_valid(countries_2_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_is_valid(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_is_valid(countries_2_ddbs, conn = conn_test, name = c('banana', 'banana')))
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
      output_ddbs_vec <- ddbs_is_simple(countries_2_ddbs)
      output_sf_vec   <- ddbs_is_simple(countries_2_sf)
      output_conn_vec <- ddbs_is_simple("countries", conn = conn_test)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_simple(countries_2_ddbs))
      expect_no_message(ddbs_is_simple(countries_2_ddbs, quiet = TRUE))
    })
    
    it("matches sf::st_is_simple results", {
      output_ddbs_vec <- ddbs_is_simple(countries_2_ddbs)
      sf_output <- sf::st_is_simple(countries_2_sf)
      
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
      expect_error(ddbs_is_simple(countries_2_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_is_simple(countries_2_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_is_simple(countries_2_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_is_simple(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_is_simple(countries_2_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })

  })
})



# 3. ddbs_is_empty() -----------------------------------------------------

## - CHECK 1.1: vector works on all formats
## - CHECK 1.2: messages work
## - CHECK 1.3: compare with SF
## - CHECK 2.1: errors

describe("ddbs_is_empty()", {

  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats", {
      output_ddbs_vec <- ddbs_is_empty(countries_2_ddbs)
      output_sf_vec   <- ddbs_is_empty(countries_2_sf)
      output_conn_vec <- ddbs_is_empty("countries", conn = conn_test)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_empty(countries_2_ddbs))
      expect_no_message(ddbs_is_empty(countries_2_ddbs, quiet = TRUE))
    })

    it("last polygon should be empty", {
      output_ddbs_vec <- ddbs_is_empty(countries_2_ddbs)
      testthat::expect_true(output_ddbs_vec[length(output_ddbs_vec)])
    })
    
    it("matches sf::st_is_empty results", {
      output_ddbs_vec <- ddbs_is_empty(countries_2_ddbs)
      sf_output <- sf::st_is_empty(countries_2_sf)
      
      expect_equal(output_ddbs_vec, sf_output)
    })
  })


  ### EXPECTED ERRORS
  
  describe("errors", {
      
    it("requires connection when using table names", {
      expect_error(ddbs_is_empty("countries", conn = NULL))
    })
    
    it("emptyates x argument type", {
      expect_error(ddbs_is_empty(x = 999))
    })
    
    it("emptyates conn argument type", {
      expect_error(ddbs_is_empty(countries_2_ddbs, conn = 999))
    })
    
    it("emptyates overwrite argument type", {
      expect_error(ddbs_is_empty(countries_2_ddbs, overwrite = 999))
    })
    
    it("emptyates quiet argument type", {
      expect_error(ddbs_is_empty(countries_2_ddbs, quiet = 999))
    })
    
    it("emptyates table name exists", {
      expect_error(ddbs_is_empty(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_is_empty(countries_2_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })

  })
})



# 4. ddbs_is_ring() -----------------------------------------------------

## - CHECK 1.1: vector works on all formats
## - CHECK 1.2: messages work
## - CHECK 1.3: compare with SF
## - CHECK 2.1: errors

describe("ddbs_is_ring()", {

  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats", {
      output_ddbs_vec <- ddbs_is_ring(countries_2_ddbs)
      output_sf_vec   <- ddbs_is_ring(countries_2_sf)
      output_conn_vec <- ddbs_is_ring("countries", conn = conn_test)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_ring(countries_2_ddbs))
      expect_no_message(ddbs_is_ring(countries_2_ddbs, quiet = TRUE))
    })

    it("returns TRUE in a closed linestring", {
      coords <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))
      closed_ring_sf <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_linestring(coords))
      )

      expect_true(ddbs_is_ring(closed_ring_sf))

    })

  })


  ### EXPECTED ERRORS
  
  describe("errors", {
      
    it("requires connection when using table names", {
      expect_error(ddbs_is_ring("countries", conn = NULL))
    })
    
    it("ringates x argument type", {
      expect_error(ddbs_is_ring(x = 999))
    })
    
    it("ringates conn argument type", {
      expect_error(ddbs_is_ring(countries_2_ddbs, conn = 999))
    })
    
    it("ringates overwrite argument type", {
      expect_error(ddbs_is_ring(countries_2_ddbs, overwrite = 999))
    })
    
    it("ringates quiet argument type", {
      expect_error(ddbs_is_ring(countries_2_ddbs, quiet = 999))
    })
    
    it("ringates table name exists", {
      expect_error(ddbs_is_ring(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_is_ring(countries_2_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })

  })
})


# 5. ddbs_is_closed() -----------------------------------------------------

## - CHECK 1.1: vector works on all formats
## - CHECK 1.2: messages work
## - CHECK 1.3: compare with SF
## - CHECK 2.1: errors

describe("ddbs_is_closed()", {

  # Closed ring - first and last points are identical
  coords_closed <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))

  # Not closed - first and last points are different
  coords_open <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1))

  # Combine in one SF object
  lines_sf <- sf::st_sf(
    id = c(1, 2),
    type = c("closed", "open"),
    geometry = sf::st_sfc(
      sf::st_linestring(coords_closed),
      sf::st_linestring(coords_open)
    )
  )

  lines_ddbs <- as_duckspatial_df(lines_sf)
  ddbs_write_vector(conn_test, lines_ddbs, "lines")

  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats", {
      output_ddbs_vec <- ddbs_is_closed(lines_ddbs)
      output_sf_vec   <- ddbs_is_closed(lines_sf)
      output_conn_vec <- ddbs_is_closed("lines", conn = conn_test)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_closed(lines_ddbs))
      expect_no_message(ddbs_is_closed(lines_ddbs, quiet = TRUE))
    })

    it("returns TRUE/FALSE correctly", {
      output_ddbs_vec <- ddbs_is_closed(lines_ddbs)

      expect_equal(
        output_ddbs_vec,
        c(TRUE, FALSE)
      )

    })

  })


  ### EXPECTED ERRORS
  
  describe("errors", {

    it("fails on polygons and points", {
      expect_error(ddbs_is_closed(countries_ddbs))
      expect_error(ddbs_is_closed(points_ddbs))
    })
      
    it("requires connection when using table names", {
      expect_error(ddbs_is_closed("countries", conn = NULL))
    })
    
    it("closedates x argument type", {
      expect_error(ddbs_is_closed(x = 999))
    })
    
    it("closedates conn argument type", {
      expect_error(ddbs_is_closed(lines_ddbs, conn = 999))
    })
    
    it("closedates overwrite argument type", {
      expect_error(ddbs_is_closed(lines_ddbs, overwrite = 999))
    })
    
    it("closedates quiet argument type", {
      expect_error(ddbs_is_closed(lines_ddbs, quiet = 999))
    })
    
    it("closedates table name exists", {
      expect_error(ddbs_is_closed(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character stclosed", {
      expect_error(ddbs_is_closed(lines_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })

  })
})


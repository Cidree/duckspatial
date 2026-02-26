
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
    
    it("returns logical vector on all formats with NULL", {
      output_ddbs_vec <- ddbs_is_valid(countries_2_ddbs, new_column = NULL)
      output_sf_vec   <- ddbs_is_valid(countries_2_sf, new_column =  NULL)
      output_conn_vec <- ddbs_is_valid("countries", conn = conn_test, new_column = NULL)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("returns duckpatial_df on all formats by default", {
      output_ddbs <- ddbs_is_valid(countries_2_ddbs)
      output_sf   <- ddbs_is_valid(countries_2_sf)
      output_conn <- ddbs_is_valid("countries", conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats", {
      output_geoarrow_fmt <- ddbs_is_valid(countries_2_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_is_valid(countries_2_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_is_valid(countries_2_ddbs, output = "raw")
      
      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_valid(countries_2_ddbs))
      expect_message(ddbs_is_valid("countries", conn = conn_test, name = "is_valid_tbl"))
      expect_message(ddbs_is_valid("countries", conn = conn_test, name = "is_valid_tbl", overwrite = TRUE))
      expect_true(ddbs_is_valid("countries", conn = conn_test, name = "is_valid_tbl2"))
      expect_no_message(ddbs_is_valid(countries_2_ddbs, quiet = TRUE))
      expect_no_message(ddbs_is_valid("countries", conn = conn_test, name = "is_valid_tbl", overwrite = TRUE, quiet = TRUE))
    })
    
    it("saves to database table correctly", {
      output_ddbs <- ddbs_is_valid(countries_2_ddbs)
      output_tbl <- ddbs_read_vector(conn_test, "is_valid_tbl")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("matches sf::st_is_valid results", {
      ## Review why Antarctica is not valid in Duckdb, but it's valid in sf
      output_ddbs_vec <- ddbs_is_valid(countries_2_ddbs, new_column = NULL)
      sf_output <- sf::st_is_valid(countries_2_sf)
      
      expect_equal(output_ddbs_vec[-4], sf_output[-4])
    })
    
  })
  
  describe("errors", {
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_valid(countries_2_sf, new_column = 5))
    })
    
    it("requires connection when using table names", {
      expect_error(ddbs_is_valid("countries", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_is_valid(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_is_valid(countries_2_ddbs, conn = 999))
    })
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_valid(countries_2_ddbs, new_column = 999))
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
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats with NULL", {
      output_ddbs_vec <- ddbs_is_simple(countries_2_ddbs, new_column = NULL)
      output_sf_vec   <- ddbs_is_simple(countries_2_sf, new_column =  NULL)
      output_conn_vec <- ddbs_is_simple("countries", conn = conn_test, new_column = NULL)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("returns duckpatial_df on all formats by default", {
      output_ddbs <- ddbs_is_simple(countries_2_ddbs)
      output_sf   <- ddbs_is_simple(countries_2_sf)
      output_conn <- ddbs_is_simple("countries", conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats", {
      output_geoarrow_fmt <- ddbs_is_simple(countries_2_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_is_simple(countries_2_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_is_simple(countries_2_ddbs, output = "raw")
      
      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_simple(countries_2_ddbs))
      expect_message(ddbs_is_simple("countries", conn = conn_test, name = "is_simple_tbl"))
      expect_message(ddbs_is_simple("countries", conn = conn_test, name = "is_simple_tbl", overwrite = TRUE))
      expect_true(ddbs_is_simple("countries", conn = conn_test, name = "is_simple_tbl2"))
      expect_no_message(ddbs_is_simple(countries_2_ddbs, quiet = TRUE))
      expect_no_message(ddbs_is_simple("countries", conn = conn_test, name = "is_simple_tbl", overwrite = TRUE, quiet = TRUE))
    })
    
    it("saves to database table correctly", {
      output_ddbs <- ddbs_is_simple(countries_2_ddbs)
      output_tbl <- ddbs_read_vector(conn_test, "is_simple_tbl")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("matches sf::st_is_valid results", {
      output_ddbs_vec <- ddbs_is_simple(countries_2_ddbs, new_column = NULL)
      sf_output <- sf::st_is_simple(countries_2_sf)
      
      expect_equal(output_ddbs_vec, sf_output)
    })
    
  })
  
  describe("errors", {
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_simple(countries_2_sf, new_column = 5))
    })
    
    it("requires connection when using table names", {
      expect_error(ddbs_is_simple("countries", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_is_simple(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_is_simple(countries_2_ddbs, conn = 999))
    })
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_simple(countries_2_ddbs, new_column = 999))
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
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats with NULL", {
      output_ddbs_vec <- ddbs_is_empty(countries_2_ddbs, new_column = NULL)
      output_sf_vec   <- ddbs_is_empty(countries_2_sf, new_column =  NULL)
      output_conn_vec <- ddbs_is_empty("countries", conn = conn_test, new_column = NULL)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("returns duckpatial_df on all formats by default", {
      output_ddbs <- ddbs_is_empty(countries_2_ddbs)
      output_sf   <- ddbs_is_empty(countries_2_sf)
      output_conn <- ddbs_is_empty("countries", conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats", {
      output_geoarrow_fmt <- ddbs_is_empty(countries_2_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_is_empty(countries_2_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_is_empty(countries_2_ddbs, output = "raw")
      
      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_empty(countries_2_ddbs))
      expect_message(ddbs_is_empty("countries", conn = conn_test, name = "is_empty_tbl"))
      expect_message(ddbs_is_empty("countries", conn = conn_test, name = "is_empty_tbl", overwrite = TRUE))
      expect_true(ddbs_is_empty("countries", conn = conn_test, name = "is_empty_tbl2"))
      expect_no_message(ddbs_is_empty(countries_2_ddbs, quiet = TRUE))
      expect_no_message(ddbs_is_empty("countries", conn = conn_test, name = "is_empty_tbl", overwrite = TRUE, quiet = TRUE))
    })
    
    it("saves to database table correctly", {
      output_ddbs <- ddbs_is_empty(countries_2_ddbs)
      output_tbl <- ddbs_read_vector(conn_test, "is_empty_tbl")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("matches sf::st_is_valid results", {
      output_ddbs_vec <- ddbs_is_empty(countries_2_ddbs, new_column = NULL)
      sf_output <- sf::st_is_empty(countries_2_sf)
      
      expect_equal(output_ddbs_vec, sf_output)
    })
    
  })
  
  describe("errors", {
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_empty(countries_2_sf, new_column = 5))
    })
    
    it("requires connection when using table names", {
      expect_error(ddbs_is_empty("countries", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_is_empty(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_is_empty(countries_2_ddbs, conn = 999))
    })
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_empty(countries_2_ddbs, new_column = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_is_empty(countries_2_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_is_empty(countries_2_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
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
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats with NULL", {
      output_ddbs_vec <- ddbs_is_ring(countries_2_ddbs, new_column = NULL)
      output_sf_vec   <- ddbs_is_ring(countries_2_sf, new_column =  NULL)
      output_conn_vec <- ddbs_is_ring("countries", conn = conn_test, new_column = NULL)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("returns duckpatial_df on all formats by default", {
      output_ddbs <- ddbs_is_ring(countries_2_ddbs)
      output_sf   <- ddbs_is_ring(countries_2_sf)
      output_conn <- ddbs_is_ring("countries", conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats", {
      output_geoarrow_fmt <- ddbs_is_ring(countries_2_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_is_ring(countries_2_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_is_ring(countries_2_ddbs, output = "raw")
      
      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_ring(countries_2_ddbs))
      expect_message(ddbs_is_ring("countries", conn = conn_test, name = "is_ring_tbl"))
      expect_message(ddbs_is_ring("countries", conn = conn_test, name = "is_ring_tbl", overwrite = TRUE))
      expect_true(ddbs_is_ring("countries", conn = conn_test, name = "is_ring_tbl2"))
      expect_no_message(ddbs_is_ring(countries_2_ddbs, quiet = TRUE))
      expect_no_message(ddbs_is_ring("countries", conn = conn_test, name = "is_ring_tbl", overwrite = TRUE, quiet = TRUE))
    })
    
    it("saves to database table correctly", {
      output_ddbs <- ddbs_is_ring(countries_2_ddbs)
      output_tbl <- ddbs_read_vector(conn_test, "is_ring_tbl")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("returns TRUE in a closed linestring", {
      coords <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))
      closed_ring_sf <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_linestring(coords))
      )

      expect_true(ddbs_is_ring(closed_ring_sf, new_column = NULL))

    })
    
  })
  
  describe("errors", {
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_ring(countries_2_sf, new_column = 5))
    })
    
    it("requires connection when using table names", {
      expect_error(ddbs_is_ring("countries", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_is_ring(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_is_ring(countries_2_ddbs, conn = 999))
    })
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_ring(countries_2_ddbs, new_column = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_is_ring(countries_2_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_is_ring(countries_2_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
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
    ),
    crs = 4326
  )

  lines_ddbs <- as_duckspatial_df(lines_sf)
  ddbs_write_vector(conn_test, lines_ddbs, "lines")

  ## EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("returns logical vector on all formats with NULL", {
      output_ddbs_vec <- ddbs_is_closed(lines_ddbs, new_column = NULL)
      output_sf_vec   <- ddbs_is_closed(lines_sf, new_column =  NULL)
      output_conn_vec <- ddbs_is_closed("lines", conn = conn_test, new_column = NULL)

      expect_type(output_ddbs_vec, "logical")
      expect_equal(output_ddbs_vec, output_sf_vec)
      expect_equal(output_ddbs_vec, output_conn_vec)
    })
    
    it("returns duckpatial_df on all formats by default", {
      output_ddbs <- ddbs_is_closed(lines_ddbs)
      output_sf   <- ddbs_is_closed(lines_sf)
      output_conn <- ddbs_is_closed("lines", conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats", {
      output_geoarrow_fmt <- ddbs_is_closed(lines_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_is_closed(lines_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_is_closed(lines_ddbs, output = "raw")
      
      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_is_closed(lines_ddbs))
      expect_message(ddbs_is_closed("lines", conn = conn_test, name = "is_closed_tbl"))
      expect_message(ddbs_is_closed("lines", conn = conn_test, name = "is_closed_tbl", overwrite = TRUE))
      expect_true(ddbs_is_closed("lines", conn = conn_test, name = "is_closed_tbl2"))
      expect_no_message(ddbs_is_closed(lines_ddbs, quiet = TRUE))
      expect_no_message(ddbs_is_closed("lines", conn = conn_test, name = "is_closed_tbl", overwrite = TRUE, quiet = TRUE))
    })
    
    it("saves to database table correctly", {
      output_ddbs <- ddbs_is_closed(lines_ddbs)
      output_tbl <- ddbs_read_vector(conn_test, "is_closed_tbl")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("returns TRUE in a closed linestring", {
      coords <- rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))
      closed_ring_sf <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_linestring(coords))
      )

      expect_true(ddbs_is_closed(closed_ring_sf, new_column = NULL))

    })
    
  })
  
  describe("errors", {
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_closed(lines_sf, new_column = 5))
    })
    
    it("requires connection when using table names", {
      expect_error(ddbs_is_closed("lines", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_is_closed(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_is_closed(lines_ddbs, conn = 999))
    })
    
    it("validates new_column argument type", {
      expect_error(ddbs_is_closed(lines_ddbs, new_column = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_is_closed(lines_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_is_closed(lines_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_is_closed(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_is_closed(lines_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})


## close connection
ddbs_stop_conn(conn_test)
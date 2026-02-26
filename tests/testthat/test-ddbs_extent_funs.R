
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

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: geometry type should be line
## - CHECK 2.1: general errors
describe("ddbs_boundary()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on all input formats", {
      output_ddbs <- ddbs_boundary(countries_ddbs)
      output_sf   <- ddbs_boundary(countries_sf)
      output_conn <- ddbs_boundary("countries", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(nrow(ddbs_collect(output_ddbs)), nrow(countries_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats", {
      output_geoarrow_fmt <- ddbs_boundary(countries_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_boundary(countries_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_boundary(countries_ddbs, output = "raw")

      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })

    it("shows and suppresses messages correctly", {
      expect_message(ddbs_boundary(countries_ddbs))
      expect_message(ddbs_boundary("countries", conn = conn_test, name = "boundary"))
      expect_message(ddbs_boundary("countries", conn = conn_test, name = "boundary", overwrite = TRUE))
      expect_true(ddbs_boundary("countries", conn = conn_test, name = "boundary2"))

      expect_no_message(ddbs_boundary(countries_ddbs, quiet = TRUE))
      expect_no_message(ddbs_boundary("countries", conn = conn_test, name = "boundary", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables correctly to DuckDB", {
      output_tbl <- ddbs_read_vector(conn_test, "boundary")
      expect_equal(
        ddbs_collect(ddbs_boundary(countries_ddbs))$geometry,
        output_tbl$geometry
      )
    })

    it("produces LINESTRING / MULTILINESTRING geometry", {
      geom_type <- sf::st_geometry_type(ddbs_collect(ddbs_boundary(countries_ddbs))) |> as.character()
      expect_in(geom_type, c("LINESTRING", "MULTILINESTRING"))
    })
  })

  ### ERRORS

  describe("errors", {

    it("requires a valid connection when using table name", {
      expect_error(ddbs_boundary("countries", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_boundary(x = 999))
      expect_error(ddbs_boundary(x = "999", conn = conn_test))
    })

    it("validates conn argument type", {
      expect_error(ddbs_boundary(countries_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_boundary(countries_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_boundary(countries_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_boundary(countries_ddbs, quiet = 999))
    })

    it("requires name to be a single character string", {
      expect_error(ddbs_boundary(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})


# 2. ddbs_envelope() -------------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: geometry type should be polygon
## - CHECK 1.6: by_feature works as expected
## - CHECK 1.7: extent should be the same as input
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_envelope()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on all input formats", {
      output_ddbs <- ddbs_envelope(countries_ddbs)
      output_sf   <- ddbs_envelope(countries_sf)
      output_conn <- ddbs_envelope("countries", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats", {
      output_geoarrow_fmt <- ddbs_envelope(countries_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_envelope(countries_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_envelope(countries_ddbs, output = "raw")

      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })

    it("shows and suppresses messages correctly", {
      expect_message(ddbs_envelope(countries_ddbs))
      expect_message(ddbs_envelope("countries", conn = conn_test, name = "envelope"))
      expect_message(ddbs_envelope("countries", conn = conn_test, name = "envelope", overwrite = TRUE))
      expect_true(ddbs_envelope("countries", conn = conn_test, name = "envelope2"))

      expect_no_message(ddbs_envelope(countries_ddbs, quiet = TRUE))
      expect_no_message(ddbs_envelope("countries", conn = conn_test, name = "envelope", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables correctly to DuckDB", {
      output_tbl <- ddbs_read_vector(conn_test, "envelope")
      expect_equal(
        ddbs_collect(ddbs_envelope(countries_ddbs))$geometry,
        output_tbl$geometry
      )
    })

    it("produces POLYGON / MULTIPOLYGON geometry", {
      geom_type <- sf::st_geometry_type(ddbs_collect(ddbs_envelope(countries_ddbs))) |> as.character()
      expect_in(geom_type, c("POLYGON", "MULTIPOLYGON"))
    })

    it("respects by_feature argument", {
      bf_false <- ddbs_envelope(countries_sf, by_feature = FALSE) |> ddbs_collect()
      bf_true  <- ddbs_envelope(countries_sf, by_feature = TRUE) |> ddbs_collect()

      expect_equal(nrow(bf_false), 1)
      expect_equal(nrow(bf_true), nrow(countries_sf))
    })

    it("produces the same extent as input", {
      output <- ddbs_envelope(countries_ddbs)
      extent_output <- ddbs_bbox(output)
      extent_input  <- ddbs_bbox(countries_ddbs)

      expect_equal(extent_output, extent_input)
    })

  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("validates by_feature argument", {
      expect_error(ddbs_envelope(countries_ddbs, by_feature = "TRUE"))
    })

    it("requires a valid connection when using table name", {
      expect_error(ddbs_envelope("countries", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_envelope(x = 999))
      expect_error(ddbs_envelope(x = "999", conn = conn_test))
    })

    it("validates conn argument type", {
      expect_error(ddbs_envelope(countries_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_envelope(countries_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_envelope(countries_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_envelope(countries_ddbs, quiet = 999))
    })

    it("requires name to be a single character string", {
      expect_error(ddbs_envelope(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})



# 3. ddbs_bbox() ---------------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: messages work
## - CHECK 1.3: writting a table works
## - CHECK 1.4: by_feature works as expected
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_bbox()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on all input formats", {
      output_ddbs <- ddbs_bbox(countries_ddbs)
      output_sf   <- ddbs_bbox(countries_sf)
      output_conn <- ddbs_bbox("countries", conn = conn_test)

      expect_s3_class(output_ddbs, "data.frame")
      expect_equal(output_ddbs, output_sf)
      expect_equal(output_ddbs, output_conn)
    })

    it("shows and suppresses messages correctly", {
      expect_message(ddbs_bbox(countries_ddbs))
      expect_message(ddbs_bbox("countries", conn = conn_test, name = "bbox"))
      expect_message(ddbs_bbox("countries", conn = conn_test, name = "bbox", overwrite = TRUE))
      expect_true(ddbs_bbox("countries", conn = conn_test, name = "bbox2"))

      expect_no_message(ddbs_bbox(countries_ddbs, quiet = TRUE))
      expect_no_message(ddbs_bbox("countries", conn = conn_test, name = "bbox", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables correctly to DuckDB", {
      output_tbl <- DBI::dbReadTable(conn_test, "bbox")
      expect_equal(ddbs_bbox(countries_ddbs), output_tbl)
    })

  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("validates by_feature argument", {
      expect_error(ddbs_bbox(countries_ddbs, by_feature = "TRUE"))
    })

    it("requires a valid connection when using table name", {
      expect_error(ddbs_bbox("countries", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_bbox(x = 999))
      expect_error(ddbs_bbox(x = "999", conn = conn_test))
    })

    it("validates conn argument type", {
      expect_error(ddbs_bbox(countries_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_bbox(countries_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_bbox(countries_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_bbox(countries_ddbs, quiet = 999))
    })

    it("requires name to be a single character string", {
      expect_error(ddbs_bbox(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)



# 0. Set up --------------------------------------------------------------

# skip tests on CRAN
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write data
duckspatial::ddbs_write_vector(conn_test, argentina_sf, "argentina")


# 1. ddbs_generate_points() ----------------------------------------------

## - CHECK 1.1: works on all formats, n works, and seed works
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: check different seeds
describe("ddbs_generate_points()", {

  ### EXPECTED BEHAVIOR

  describe("expected behavior", {

    it("works on all input formats, n works, and seed works", {
      output_ddbs <- ddbs_generate_points(argentina_ddbs, 50, seed = 123)
      output_sf   <- ddbs_generate_points(argentina_sf, 50, seed = 123)
      output_conn <- ddbs_generate_points("argentina", 50, conn = conn_test, seed = 123)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(nrow(ddbs_collect(output_ddbs)), 50)
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different outputs based on output argument", {
      output_geoarrow_fmt <- ddbs_generate_points(argentina_ddbs, 10, output = "geoarrow")
      output_sf_fmt       <- ddbs_generate_points(argentina_ddbs, 10, output = "sf")
      output_raw_fmt      <- ddbs_generate_points(argentina_ddbs, 10, output = "raw")

      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })

    it("shows and suppresses messages correctly", {
      expect_message(ddbs_generate_points(argentina_ddbs, 10))
      expect_message(ddbs_generate_points("argentina", 50, conn = conn_test, name = "generate_points"))
      expect_message(ddbs_generate_points("argentina", 50, conn = conn_test, name = "generate_points", overwrite = TRUE))
      expect_true(ddbs_generate_points("argentina", 50, conn = conn_test, name = "generate_points2"))

      expect_no_message(ddbs_generate_points(argentina_ddbs, 50, quiet = TRUE))
      expect_no_message(ddbs_generate_points("argentina", 50, seed = 123, conn = conn_test, name = "generate_points", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables correctly to DuckDB", {
      output_ddbs <- ddbs_generate_points(argentina_ddbs, 50, seed = 123)
      output_tbl <- ddbs_read_vector(conn_test, "generate_points")
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("produces different points with different seeds", {
      output_ddbs <- ddbs_generate_points(argentina_ddbs, 50, seed = 123)
      output_ddbs_2 <- ddbs_generate_points(argentina_ddbs, 50, seed = 678)
      expect_false(
        identical(ddbs_collect(output_ddbs), ddbs_collect(output_ddbs_2))
      )
    })

  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("requires a valid connection when using table name", {
      expect_error(ddbs_generate_points("countries", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_generate_points(x = 999))
      expect_error(ddbs_generate_points(x = "999", conn = conn_test))
    })

    it("validates conn argument type", {
      expect_error(ddbs_generate_points(countries_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_generate_points(countries_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_generate_points(countries_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_generate_points(countries_ddbs, quiet = 999))
    })

    it("requires name to be a single character string", {
      expect_error(ddbs_generate_points(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
  
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)


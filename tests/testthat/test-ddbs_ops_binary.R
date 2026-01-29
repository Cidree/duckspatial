
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()
conn_test_2 <- duckspatial::ddbs_create_conn()

## write data in the database
ddbs_write_vector(conn_test, points_sf, "points")
ddbs_write_vector(conn_test, argentina_sf, "argentina")
ddbs_write_vector(conn_test_2, argentina_sf, "argentina")


# 1. ddbs_intersection() -------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: conn_x and conn_y work
## - CHECK 1.6: compare to sf
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
# ddbs_intersection() -----------------------------------------------------

describe("ddbs_intersection()", {

  describe("expected behavior", {

    it("works on all formats and matches results", {
      output_1 <- ddbs_intersection(points_sf, argentina_sf)
      output_2 <- ddbs_intersection(points_ddbs, argentina_sf)
      output_3 <- ddbs_intersection(points_sf, argentina_ddbs)
      output_4 <- ddbs_intersection(points_ddbs, argentina_ddbs)

      expect_warning(ddbs_intersection("points", argentina_ddbs, conn = conn_test))
      output_6 <- ddbs_intersection("points", argentina_sf, conn = conn_test)
      output_7 <- ddbs_intersection(points_sf, "argentina", conn = conn_test)
      expect_warning(ddbs_intersection(points_ddbs, "argentina", conn = conn_test))
      output_9 <- ddbs_intersection("points", "argentina", conn = conn_test)

      expect_s3_class(output_1, "duckspatial_df")
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_2))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_3))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_4))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_6))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_7))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_9))
    })

    it("returns different outputs depending on 'output' argument", {
      output_geoarrow_fmt <- ddbs_intersection(points_sf, argentina_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_intersection(points_sf, argentina_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_intersection(points_sf, argentina_ddbs, output = "raw")

      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })

    it("handles messages correctly", {
      expect_message(ddbs_intersection(points_sf, argentina_sf))
      expect_message(ddbs_intersection(points_sf, argentina_sf, conn = conn_test, name = "intersection"))
      expect_message(ddbs_intersection(points_sf, argentina_sf, conn = conn_test, name = "intersection", overwrite = TRUE))
      expect_true(ddbs_intersection(points_sf, argentina_sf, conn = conn_test, name = "intersection2"))

      expect_no_message(ddbs_intersection(points_sf, argentina_sf, quiet = TRUE))
      expect_no_message(ddbs_intersection("points", "argentina", conn = conn_test, name = "intersection", overwrite = TRUE, quiet = TRUE))
    })

    it("writes a table correctly", {
      output_1 <- ddbs_intersection(points_sf, argentina_sf)
      output_tbl <- ddbs_read_vector(conn_test, "intersection")
      expect_equal(ddbs_collect(output_1)$geometry, output_tbl$geometry)
    })

    it("works with separate connections for x and y", {
      output_1 <- ddbs_intersection(points_sf, argentina_sf)
      output_10 <- ddbs_intersection("points", "argentina", conn_x = conn_test, conn_y = conn_test_2)
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_10))

      expect_message(ddbs_intersection("points", "argentina", conn_x = conn_test, conn_y = conn_test_2, name = "test"))
      expect_true(DBI::dbExistsTable(conn_test, "test"))
      expect_false(DBI::dbExistsTable(conn_test_2, "test"))
    })

    it("matches sf::st_intersection results", {
      sf_output   <- sf::st_intersection(points_sf, argentina_sf)
      ddbs_output <- ddbs_intersection(points_sf, argentina_sf) |> sf::st_as_sf()
      expect_equal(sf_output$geometry, ddbs_output$geometry)
    })

  })

  describe("errors", {

    it("errors on missing or invalid arguments", {
      expect_error(ddbs_intersection(argentina_ddbs))
      expect_error(ddbs_intersection(y = argentina_ddbs))
      expect_error(ddbs_intersection("argentina", conn = NULL))
      expect_error(ddbs_intersection("points", "argentina", conn_x = conn_test))
      expect_error(ddbs_intersection("points", "argentina", conn_y = conn_test))
    })

    it("errors on other invalid inputs", {
      expect_error(ddbs_intersection(x = 999))
      expect_error(ddbs_intersection(argentina_ddbs, points_sf, conn = 999))
      expect_error(ddbs_intersection(argentina_ddbs, points_sf, overwrite = 999))
      expect_error(ddbs_intersection(argentina_ddbs, points_sf, quiet = 999))
      expect_error(ddbs_intersection(x = "999", points_sf, conn = conn_test))
      expect_error(ddbs_intersection(argentina_ddbs, points_sf, conn = conn_test, name = c('banana', 'banana')))
    })

  })

})



# 2. ddbs_difference() -------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting a table works
## - CHECK 1.5: conn_x and conn_y work
## - CHECK 1.6: compare to sf
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
describe("ddbs_difference()", {

  describe("expected behavior", {

    it("works on all formats and matches results", {
      output_1 <- ddbs_difference(points_sf, argentina_sf)
      output_2 <- ddbs_difference(points_ddbs, argentina_sf)
      output_3 <- ddbs_difference(points_sf, argentina_ddbs)
      output_4 <- ddbs_difference(points_ddbs, argentina_ddbs)

      expect_warning(ddbs_difference("points", argentina_ddbs, conn = conn_test))
      output_6 <- ddbs_difference("points", argentina_sf, conn = conn_test)
      output_7 <- ddbs_difference(points_sf, "argentina", conn = conn_test)
      expect_warning(ddbs_difference(points_ddbs, "argentina", conn = conn_test))
      output_9 <- ddbs_difference("points", "argentina", conn = conn_test)

      expect_s3_class(output_1, "duckspatial_df")
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_2))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_3))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_4))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_6))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_7))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_9))
    })

    it("returns different outputs depending on 'output' argument", {
      output_geoarrow_fmt <- ddbs_difference(points_sf, argentina_ddbs, output = "geoarrow")
      output_sf_fmt       <- ddbs_difference(points_sf, argentina_ddbs, output = "sf")
      output_raw_fmt      <- ddbs_difference(points_sf, argentina_ddbs, output = "raw")

      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })

    it("handles messages correctly", {
      expect_message(ddbs_difference(points_sf, argentina_sf))
      expect_message(ddbs_difference(points_sf, argentina_sf, conn = conn_test, name = "difference"))
      expect_message(ddbs_difference(points_sf, argentina_sf, conn = conn_test, name = "difference", overwrite = TRUE))
      expect_true(ddbs_difference(points_sf, argentina_sf, conn = conn_test, name = "difference2"))

      expect_no_message(ddbs_difference(points_sf, argentina_sf, quiet = TRUE))
      expect_no_message(ddbs_difference("points", "argentina", conn = conn_test, name = "difference", overwrite = TRUE, quiet = TRUE))
    })

    it("writes a table correctly", {
      output_1 <- ddbs_difference(points_sf, argentina_sf)
      output_tbl <- ddbs_read_vector(conn_test, "difference")
      expect_equal(ddbs_collect(output_1)$geometry, output_tbl$geometry)
    })

    it("works with separate connections for x and y", {
      output_1 <- ddbs_difference(points_sf, argentina_sf)
      output_10 <- ddbs_difference("points", "argentina", conn_x = conn_test, conn_y = conn_test_2)
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_10))

      expect_message(ddbs_difference("points", "argentina", conn_x = conn_test, conn_y = conn_test_2, name = "diff3"))
      expect_true(DBI::dbExistsTable(conn_test, "diff3"))
      expect_false(DBI::dbExistsTable(conn_test_2, "diff3"))
    })

    it("matches sf::st_difference results", {
      sf_output   <- sf::st_difference(points_sf, argentina_sf)
      ddbs_output <- ddbs_difference(points_sf, argentina_sf) |> sf::st_as_sf()
      expect_equal(sf_output$geometry, ddbs_output$geometry)
    })

  })

  describe("errors", {

    it("errors on missing or invalid arguments", {
      expect_error(ddbs_difference(argentina_ddbs))
      expect_error(ddbs_difference(y = argentina_ddbs))
      expect_error(ddbs_difference("argentina", conn = NULL))
      expect_error(ddbs_difference("points", "argentina", conn_x = conn_test))
      expect_error(ddbs_difference("points", "argentina", conn_y = conn_test))
    })

    it("errors on other invalid inputs", {
      expect_error(ddbs_difference(x = 999))
      expect_error(ddbs_difference(argentina_ddbs, points_sf, conn = 999))
      expect_error(ddbs_difference(argentina_ddbs, points_sf, overwrite = 999))
      expect_error(ddbs_difference(argentina_ddbs, points_sf, quiet = 999))
      expect_error(ddbs_difference(x = "999", points_sf, conn = conn_test))
      expect_error(ddbs_difference(argentina_ddbs, points_sf, conn = conn_test, name = c('banana', 'banana')))
    })

  })

})

## stop connection
duckspatial::ddbs_stop_conn(conn_test)


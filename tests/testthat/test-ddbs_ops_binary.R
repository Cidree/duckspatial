
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create two overlapping polygons for testing
poly1 <- st_polygon(list(matrix(c(
  0, 0,
  4, 0,
  4, 4,
  0, 4,
  0, 0
), ncol = 2, byrow = TRUE)))

poly2 <- st_polygon(list(matrix(c(
  2, 2,
  6, 2,
  6, 6,
  2, 6,
  2, 2
), ncol = 2, byrow = TRUE)))

poly1_sf <- st_sf(id = 1, geometry = st_sfc(poly1), crs = 4326)
poly2_sf <- st_sf(id = 2, geometry = st_sfc(poly2), crs = 4326)

poly1_ddbs <- as_duckspatial_df(poly1_sf)
poly2_ddbs <- as_duckspatial_df(poly2_sf)

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()
conn_test_2 <- duckspatial::ddbs_create_conn()

## write data in the database
ddbs_write_table(conn_test, poly1_sf, "poly1")
ddbs_write_table(conn_test, poly2_sf, "poly2")
ddbs_write_table(conn_test_2, poly2_sf, "poly2")


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
      output_1 <- ddbs_intersection(poly1_sf, poly2_sf)
      output_2 <- ddbs_intersection(poly1_ddbs, poly2_sf)
      output_3 <- ddbs_intersection(poly1_sf, poly2_ddbs)
      output_4 <- ddbs_intersection(poly1_ddbs, poly2_ddbs)

      expect_warning(ddbs_intersection("poly1", poly2_ddbs, conn = conn_test))
      output_6 <- ddbs_intersection("poly1", poly2_sf, conn = conn_test)
      output_7 <- ddbs_intersection(poly1_sf, "poly2", conn = conn_test)
      expect_warning(ddbs_intersection(poly1_ddbs, "poly2", conn = conn_test))
      output_9 <- ddbs_intersection("poly1", "poly2", conn = conn_test)

      expect_s3_class(output_1, "duckspatial_df")
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_2))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_3))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_4))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_6))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_7))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_9))
    })

    it("returns different outputs depending on 'mode' argument", {
      output_sf_fmt <- ddbs_intersection(poly1_sf, poly2_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("handles messages correctly", {
      expect_no_message(ddbs_intersection(poly1_sf, poly2_sf))
      expect_message(ddbs_intersection(poly1_sf, poly2_sf, conn = conn_test, name = "intersection"))
      expect_message(ddbs_intersection(poly1_sf, poly2_sf, conn = conn_test, name = "intersection", overwrite = TRUE))
      expect_true(ddbs_intersection(poly1_sf, poly2_sf, conn = conn_test, name = "intersection2"))

      expect_no_message(ddbs_intersection(poly1_sf, poly2_sf, quiet = TRUE))
      expect_no_message(ddbs_intersection("poly1", "poly2", conn = conn_test, name = "intersection", overwrite = TRUE, quiet = TRUE))
    })

    it("writes a table correctly", {
      output_1 <- ddbs_intersection(poly1_sf, poly2_sf)
      output_tbl <- ddbs_read_table(conn_test, "intersection")
      expect_equal(ddbs_collect(output_1)$geometry, output_tbl$geometry)
    })

    it("works with separate connections for x and y", {
      output_1 <- ddbs_intersection(poly1_sf, poly2_sf)
      output_10 <- ddbs_intersection("poly1", "poly2", conn_x = conn_test, conn_y = conn_test_2)
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_10))

      expect_message(ddbs_intersection("poly1", "poly2", conn_x = conn_test, conn_y = conn_test_2, name = "test"))
      expect_true(DBI::dbExistsTable(conn_test, "test"))
      expect_false(DBI::dbExistsTable(conn_test_2, "test"))
    })

    it("matches sf::st_intersection results", {
      sf_output   <- sf::st_intersection(poly1_sf, poly2_sf)
      ddbs_output <- ddbs_intersection(poly1_sf, poly2_sf) |> sf::st_as_sf()
      
      ## Note that ddbs_intersection produces more accurate results,
      ## therefore, just check the bounding box
      bbox_ddbs <- round(ddbs_bbox(ddbs_output, mode = "sf"))
      bbox_sf   <- round(ddbs_bbox(sf_output, mode = "sf"))

      expect_equal(bbox_ddbs, bbox_sf)
    })

  })

  describe("errors", {

    it("errors on missing or invalid arguments", {
      expect_error(ddbs_intersection(poly2_ddbs))
      expect_error(ddbs_intersection(y = poly2_ddbs))
      expect_error(ddbs_intersection("poly2", conn = NULL))
      expect_error(ddbs_intersection("poly1", "poly2", conn_x = conn_test))
      expect_error(ddbs_intersection("poly1", "poly2", conn_y = conn_test))
    })

    it("errors on other invalid inputs", {
      expect_error(ddbs_intersection(x = 999))
      expect_error(ddbs_intersection(poly2_ddbs, poly1_sf, conn = 999))
      expect_error(ddbs_intersection(poly2_ddbs, poly1_sf, overwrite = 999))
      expect_error(ddbs_intersection(poly2_ddbs, poly1_sf, quiet = 999))
      expect_error(ddbs_intersection(x = "999", poly1_sf, conn = conn_test))
      expect_error(ddbs_intersection(poly2_ddbs, poly1_sf, conn = conn_test, name = c('banana', 'banana')))
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
      output_1 <- ddbs_difference(poly1_sf, poly2_sf)
      output_2 <- ddbs_difference(poly1_ddbs, poly2_sf)
      output_3 <- ddbs_difference(poly1_sf, poly2_ddbs)
      output_4 <- ddbs_difference(poly1_ddbs, poly2_ddbs)

      expect_warning(ddbs_difference("poly1", poly2_ddbs, conn = conn_test))
      output_6 <- ddbs_difference("poly1", poly2_sf, conn = conn_test)
      output_7 <- ddbs_difference(poly1_sf, "poly2", conn = conn_test)
      expect_warning(ddbs_difference(poly1_ddbs, "poly2", conn = conn_test))
      output_9 <- ddbs_difference("poly1", "poly2", conn = conn_test)

      expect_s3_class(output_1, "duckspatial_df")
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_2))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_3))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_4))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_6))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_7))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_9))
    })

    it("returns different outputs depending on 'mode' argument", {
      output_sf_fmt <- ddbs_difference(poly1_sf, poly2_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("handles messages correctly", {
      expect_no_message(ddbs_difference(poly1_sf, poly2_sf))
      expect_message(ddbs_difference(poly1_sf, poly2_sf, conn = conn_test, name = "difference"))
      expect_message(ddbs_difference(poly1_sf, poly2_sf, conn = conn_test, name = "difference", overwrite = TRUE))
      expect_true(ddbs_difference(poly1_sf, poly2_sf, conn = conn_test, name = "difference2"))

      expect_no_message(ddbs_difference(poly1_sf, poly2_sf, quiet = TRUE))
      expect_no_message(ddbs_difference("poly1", "poly2", conn = conn_test, name = "difference", overwrite = TRUE, quiet = TRUE))
    })

    it("writes a table correctly", {
      output_1 <- ddbs_difference(poly1_sf, poly2_sf)
      output_tbl <- ddbs_read_table(conn_test, "difference")
      expect_equal(ddbs_collect(output_1)$geometry, output_tbl$geometry)
    })

    it("works with separate connections for x and y", {
      output_1 <- ddbs_difference(poly1_sf, poly2_sf)
      output_10 <- ddbs_difference("poly1", "poly2", conn_x = conn_test, conn_y = conn_test_2)
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_10))

      expect_message(ddbs_difference("poly1", "poly2", conn_x = conn_test, conn_y = conn_test_2, name = "diff3"))
      expect_true(DBI::dbExistsTable(conn_test, "diff3"))
      expect_false(DBI::dbExistsTable(conn_test_2, "diff3"))
    })

    it("matches sf::st_difference results", {
      sf_output   <- sf::st_difference(poly1_sf, poly2_sf)
      ddbs_output <- ddbs_difference(poly1_sf, poly2_sf) |> sf::st_as_sf()
      
      ## Note that ddbs_difference produces more accurate results,
      ## therefore, just check the bounding box
      bbox_ddbs <- round(ddbs_bbox(ddbs_output, mode = "sf"))
      bbox_sf   <- round(ddbs_bbox(sf_output, mode = "sf"))

      expect_equal(bbox_ddbs, bbox_sf)
    })

  })

  describe("errors", {

    it("errors on missing or invalid arguments", {
      expect_error(ddbs_difference(poly2_ddbs))
      expect_error(ddbs_difference(y = poly2_ddbs))
      expect_error(ddbs_difference("poly2", conn = NULL))
      expect_error(ddbs_difference("poly1", "poly2", conn_x = conn_test))
      expect_error(ddbs_difference("poly1", "poly2", conn_y = conn_test))
    })

    it("errors on other invalid inputs", {
      expect_error(ddbs_difference(x = 999))
      expect_error(ddbs_difference(poly2_ddbs, poly1_sf, conn = 999))
      expect_error(ddbs_difference(poly2_ddbs, poly1_sf, overwrite = 999))
      expect_error(ddbs_difference(poly2_ddbs, poly1_sf, quiet = 999))
      expect_error(ddbs_difference(x = "999", poly1_sf, conn = conn_test))
      expect_error(ddbs_difference(poly2_ddbs, poly1_sf, conn = conn_test, name = c('banana', 'banana')))
    })

  })

})


# 3. ddbs_sym_difference() -----------------------------------------------

describe("ddbs_sym_difference()", {

  describe("expected behavior", {

    it("works on all formats and matches results", {
      output_1 <- ddbs_sym_difference(poly1_sf, poly2_sf)
      output_2 <- ddbs_sym_difference(poly1_ddbs, poly2_sf)
      output_3 <- ddbs_sym_difference(poly1_sf, poly2_ddbs)
      output_4 <- ddbs_sym_difference(poly1_ddbs, poly2_ddbs)

      expect_warning(ddbs_sym_difference("poly1", poly2_ddbs, conn = conn_test))
      output_6 <- ddbs_sym_difference("poly1", poly2_sf, conn = conn_test)
      output_7 <- ddbs_sym_difference(poly1_sf, "poly2", conn = conn_test)
      expect_warning(ddbs_sym_difference(poly1_ddbs, "poly2", conn = conn_test))
      output_9 <- ddbs_sym_difference("poly1", "poly2", conn = conn_test)

      expect_s3_class(output_1, "duckspatial_df")
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_2))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_3))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_4))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_6))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_7))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_9))
    })

    it("returns different outputs depending on 'mode' argument", {
      output_sf_fmt <- ddbs_sym_difference(poly1_sf, poly2_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("handles messages correctly", {
      expect_no_message(ddbs_sym_difference(poly1_sf, poly2_sf))
      expect_message(ddbs_sym_difference(poly1_sf, poly2_sf, conn = conn_test, name = "symdifference"))
      expect_message(ddbs_sym_difference(poly1_sf, poly2_sf, conn = conn_test, name = "symdifference", overwrite = TRUE))
      expect_true(ddbs_sym_difference(poly1_sf, poly2_sf, conn = conn_test, name = "symdifference2"))

      expect_no_message(ddbs_sym_difference(poly1_sf, poly2_sf, quiet = TRUE))
      expect_no_message(ddbs_sym_difference("poly1", "poly2", conn = conn_test, name = "symdifference", overwrite = TRUE, quiet = TRUE))
    })

    it("writes a table correctly", {
      output_1 <- ddbs_sym_difference(poly1_sf, poly2_sf)
      output_tbl <- ddbs_read_table(conn_test, "symdifference")
      expect_equal(ddbs_collect(output_1)$geometry, output_tbl$geometry)
    })

    it("works with separate connections for x and y", {
      output_1 <- ddbs_sym_difference(poly1_sf, poly2_sf)
      output_10 <- ddbs_sym_difference("poly1", "poly2", conn_x = conn_test, conn_y = conn_test_2)
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_10))

      expect_message(ddbs_sym_difference("poly1", "poly2", conn_x = conn_test, conn_y = conn_test_2, name = "symdiff3"))
      expect_true(DBI::dbExistsTable(conn_test, "symdiff3"))
      expect_false(DBI::dbExistsTable(conn_test_2, "symdiff3"))
    })

    it("matches sf::st_sym_difference results", {
      sf_output   <- sf::st_sym_difference(poly1_sf, poly2_sf)
      ddbs_output <- ddbs_sym_difference(poly1_sf, poly2_sf) |> sf::st_as_sf()
      
      ## Note that ddbs_sym_difference produces more accurate results,
      ## therefore, just check the bounding box
      bbox_ddbs <- round(ddbs_bbox(ddbs_output, mode = "sf"))
      bbox_sf   <- round(ddbs_bbox(sf_output, mode = "sf"))

      expect_equal(bbox_ddbs, bbox_sf)
    })

    it("produces symmetric results", {
      # Symmetric difference should be commutative: symdiff(A,B) == symdiff(B,A)
      output_xy <- ddbs_sym_difference(poly1_sf, poly2_sf)
      output_yx <- ddbs_sym_difference(poly2_sf, poly1_sf)
      
      expect_equal(
        ddbs_bbox(output_xy, mode = "sf"),
        ddbs_bbox(output_yx, mode = "sf")
      )
    })

  })

  describe("errors", {

    it("errors on missing or invalid arguments", {
      expect_error(ddbs_sym_difference(poly2_ddbs))
      expect_error(ddbs_sym_difference(y = poly2_ddbs))
      expect_error(ddbs_sym_difference("poly2", conn = NULL))
      expect_error(ddbs_sym_difference("poly1", "poly2", conn_x = conn_test))
      expect_error(ddbs_sym_difference("poly1", "poly2", conn_y = conn_test))
    })

    it("errors on other invalid inputs", {
      expect_error(ddbs_sym_difference(x = 999))
      expect_error(ddbs_sym_difference(poly2_ddbs, poly1_sf, conn = 999))
      expect_error(ddbs_sym_difference(poly2_ddbs, poly1_sf, overwrite = 999))
      expect_error(ddbs_sym_difference(poly2_ddbs, poly1_sf, quiet = 999))
      expect_error(ddbs_sym_difference(x = "999", poly1_sf, conn = conn_test))
      expect_error(ddbs_sym_difference(poly2_ddbs, poly1_sf, conn = conn_test, name = c('banana', 'banana')))
    })

  })

})





## stop connection
duckspatial::ddbs_stop_conn(conn_test)


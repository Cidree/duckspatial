
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create two overlapping polygons for testing
poly1 <- sf::st_polygon(list(matrix(c(
  0, 0,
  4, 0,
  4, 4,
  0, 4,
  0, 0
), ncol = 2, byrow = TRUE)))

poly2 <- sf::st_polygon(list(matrix(c(
  2, 2,
  6, 2,
  6, 6,
  2, 6,
  2, 2
), ncol = 2, byrow = TRUE)))

poly1_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(poly1), crs = 4326)
poly2_sf <- sf::st_sf(id = 2, geometry = sf::st_sfc(poly2), crs = 4326)

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
      
      expect_in("test", ddbs_list_tables(conn_test)$table_name)
      expect_disjoint("test", ddbs_list_tables(conn_test_2)$table_name)
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

      expect_in("diff3", ddbs_list_tables(conn_test)$table_name)
      expect_disjoint("diff3", ddbs_list_tables(conn_test_2)$table_name)
      # expect_true(DBI::dbExistsTable(conn_test, "diff3"))
      # expect_false(DBI::dbExistsTable(conn_test_2, "diff3"))
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

      expect_in("symdiff3", ddbs_list_tables(conn_test)$table_name)
      expect_disjoint("symdiff3", ddbs_list_tables(conn_test_2)$table_name)
      # expect_true(DBI::dbExistsTable(conn_test, "symdiff3"))
      # expect_false(DBI::dbExistsTable(conn_test_2, "symdiff3"))
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





# 4. ddbs_crop() ---------------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns different output modes
## - CHECK 1.3: messages work
## - CHECK 1.4: writing a table works
## - CHECK 1.5: warns when mixing connections
## - CHECK 1.6: compare to sf
## - CHECK 2.1: missing / invalid arguments
## - CHECK 2.2: other errors

describe("ddbs_crop()", {

  describe("expected behavior", {

    it("works on all formats and matches results", {
      output_1 <- ddbs_crop(poly1_sf, poly2_sf)
      output_2 <- ddbs_crop(poly1_ddbs, poly2_sf)
      output_3 <- ddbs_crop(poly1_sf, poly2_ddbs)
      output_4 <- ddbs_crop(poly1_ddbs, poly2_ddbs)

      expect_s3_class(output_1, "duckspatial_df")
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_2))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_3))
      expect_equal(ddbs_collect(output_1), ddbs_collect(output_4))
    })

    it("returns different outputs depending on 'mode' argument", {
      output_sf_fmt <- ddbs_crop(poly1_sf, poly2_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("handles messages correctly", {
      expect_no_message(ddbs_crop(poly1_sf, poly2_sf))
      expect_message(ddbs_crop(poly1_sf, poly2_sf, conn = conn_test, name = "crop"))
      expect_message(ddbs_crop(poly1_sf, poly2_sf, conn = conn_test, name = "crop", overwrite = TRUE))
      expect_true(ddbs_crop(poly1_sf, poly2_sf, conn = conn_test, name = "crop2"))

      expect_no_message(ddbs_crop(poly1_sf, poly2_sf, quiet = TRUE))
    })

    it("writes a table correctly", {
      output_1   <- ddbs_crop(poly1_sf, poly2_sf)
      output_tbl <- ddbs_read_table(conn_test, "crop")
      expect_equal(ddbs_collect(output_1)$geometry, output_tbl$geometry)
    })

    it("warns when mixing a table name with a duckspatial_df from another connection", {
      expect_warning(ddbs_crop("poly1", poly2_ddbs, conn = conn_test))
    })

    it("matches sf::st_crop results", {
      sf_output   <- sf::st_crop(poly1_sf, poly2_sf)
      ddbs_output <- ddbs_crop(poly1_sf, poly2_sf) |> sf::st_as_sf()

      bbox_ddbs <- round(ddbs_bbox(ddbs_output, mode = "sf"))
      bbox_sf   <- round(ddbs_bbox(sf_output, mode = "sf"))

      expect_equal(bbox_ddbs, bbox_sf)
    })

  })

  describe("errors", {

    it("errors on missing or invalid arguments", {
      expect_error(ddbs_crop(poly2_ddbs))
      expect_error(ddbs_crop(y = poly2_ddbs))
      expect_error(ddbs_crop("poly2", poly1_sf, conn = NULL))
    })

    it("errors on other invalid inputs", {
      expect_error(ddbs_crop(999, poly1_sf))
      expect_error(ddbs_crop(poly2_ddbs, poly1_sf, conn = 999))
      expect_error(ddbs_crop(poly2_ddbs, poly1_sf, overwrite = 999))
      expect_error(ddbs_crop(poly2_ddbs, poly1_sf, quiet = 999))
    })

  })

})



# ddbs_shortest_line() ---------------------------------------------------

## Fixtures: two separate POINT datasets in a projected CRS.
## (0,0) → (3,4): known Euclidean distance = 5 m in EPSG:3857.
pt_a_sf   <- sf::st_as_sf(data.frame(id = 1L, x = 0, y = 0), coords = c("x", "y"), crs = "EPSG:3857")
pt_b_sf   <- sf::st_as_sf(data.frame(id = 2L, x = 3, y = 4), coords = c("x", "y"), crs = "EPSG:3857")
pt_a_ddbs <- as_duckspatial_df(pt_a_sf)
pt_b_ddbs <- as_duckspatial_df(pt_b_sf)
ddbs_write_table(conn_test, pt_a_sf, "pt_a")
ddbs_write_table(conn_test, pt_b_sf, "pt_b")

describe("ddbs_shortest_line()", {

  describe("expected behavior on sf input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_shortest_line(pt_a_sf, pt_b_sf)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns an sf object with mode sf", {
      output <- ddbs_shortest_line(pt_a_sf, pt_b_sf, mode = "sf")
      expect_s3_class(output, "sf")
    })

    it("output geometry is LINESTRING", {
      output <- ddbs_shortest_line(pt_a_sf, pt_b_sf, mode = "sf")
      expect_equal(as.character(sf::st_geometry_type(output)), "LINESTRING")
    })

    it("shortest line length equals the planar distance between the points", {
      line   <- ddbs_shortest_line(pt_a_sf, pt_b_sf, mode = "sf")
      expect_equal(as.numeric(sf::st_length(line)), 5, tolerance = 1e-6)
    })

    it("writes to a table when name is provided", {
      output <- ddbs_shortest_line(pt_a_sf, pt_b_sf, conn = conn_test, name = "shortest_tbl")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_message(
        ddbs_shortest_line(pt_a_sf, pt_b_sf, conn = conn_test, name = "shortest_tbl2")
      )
      expect_no_message(
        ddbs_shortest_line(pt_a_sf, pt_b_sf, conn = conn_test, name = "shortest_tbl3", quiet = TRUE)
      )
    })
  })

  describe("expected behavior on duckspatial_df input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_shortest_line(pt_a_ddbs, pt_b_ddbs)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns an sf object with mode sf", {
      output <- ddbs_shortest_line(pt_a_ddbs, pt_b_ddbs, mode = "sf")
      expect_s3_class(output, "sf")
    })

    it("warns when mixing connections", {
      expect_warning(ddbs_shortest_line(pt_a_ddbs, pt_b_sf, conn = conn_test))
    })

    it("matches sf input result", {
      out_sf   <- ddbs_shortest_line(pt_a_sf,   pt_b_sf,   mode = "sf")
      out_ddbs <- ddbs_shortest_line(pt_a_ddbs, pt_b_ddbs, mode = "sf")
      expect_equal(sf::st_geometry(out_sf), sf::st_geometry(out_ddbs))
    })
  })

  describe("expected behavior on DuckDB table input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_shortest_line("pt_a", "pt_b", conn = conn_test)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns an sf object with mode sf", {
      output <- ddbs_shortest_line("pt_a", "pt_b", conn = conn_test, mode = "sf")
      expect_s3_class(output, "sf")
    })

    it("matches sf input result", {
      out_sf   <- ddbs_shortest_line(pt_a_sf, pt_b_sf, mode = "sf")
      out_conn <- ddbs_shortest_line("pt_a",  "pt_b",  conn = conn_test, mode = "sf")
      expect_equal(sf::st_geometry(out_sf), sf::st_geometry(out_conn))
    })
  })

  describe("errors", {

    it("requires both x and y", {
      expect_error(ddbs_shortest_line(pt_a_sf))
      expect_error(ddbs_shortest_line(y = pt_b_sf))
    })

    it("requires a connection for character table names", {
      expect_error(ddbs_shortest_line("pt_a", "pt_b", conn = NULL))
    })

    it("rejects mismatched CRS", {
      pt_b_4326_sf <- sf::st_transform(pt_b_sf, "EPSG:4326")
      expect_error(ddbs_shortest_line(pt_a_sf, pt_b_4326_sf))
    })

    it("rejects invalid x and y types", {
      expect_error(ddbs_shortest_line(999, pt_b_sf))
      expect_error(ddbs_shortest_line(pt_a_sf, 999))
    })

    it("validates conn, overwrite, quiet argument types", {
      expect_error(ddbs_shortest_line(pt_a_sf, pt_b_sf, conn = 999))
      expect_error(ddbs_shortest_line(pt_a_sf, pt_b_sf, overwrite = 999))
      expect_error(ddbs_shortest_line(pt_a_sf, pt_b_sf, quiet = 999))
    })
  })
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)


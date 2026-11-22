
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## helper to create a unit square with lower-left corner at (x0, y0)
make_square <- function(x0, y0) {
  sf::st_polygon(list(
    rbind(c(x0, y0), c(x0 + 1, y0), c(x0 + 1, y0 + 1), c(x0, y0 + 1), c(x0, y0))
  ))
}

## valid coverage: 2x2 grid of adjacent unit squares
grid_sf <- sf::st_sf(
  id       = 1:4,
  geometry = sf::st_sfc(
    make_square(0, 0), make_square(1, 0),
    make_square(0, 1), make_square(1, 1),
    crs = 4326
  )
)
grid_ddbs <- as_duckspatial_df(grid_sf)

## invalid coverage: two squares that should share the edge x = 1, but the
## second one has a mismatched vertex
bad_cov_sf <- sf::st_sf(
  id       = 1:2,
  geometry = sf::st_sfc(
    make_square(0, 0),
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 0.9), c(1, 0)))),
    crs = 4326
  )
)
bad_cov_ddbs <- as_duckspatial_df(bad_cov_sf)

## write data
duckspatial::ddbs_write_table(conn_test, grid_sf, "grid")
duckspatial::ddbs_write_table(conn_test, bad_cov_sf, "bad_cov")


# 1. ddbs_coverage_union() -----------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns different output formats (duckspatial_df, sf)
## - CHECK 1.3: messages work
## - CHECK 1.4: writing a table works
## - CHECK 1.5: returns a single POLYGON with the total area and extent
## - CHECK 1.6: CRS is preserved
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_coverage_union()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on all input formats", {
      output_ddbs <- ddbs_coverage_union(grid_ddbs)
      output_sf   <- ddbs_coverage_union(grid_sf)
      output_conn <- ddbs_coverage_union("grid", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats", {
      output_ddbs <- ddbs_coverage_union(grid_ddbs, mode = "duckspatial")
      output_sf   <- ddbs_coverage_union(grid_ddbs, mode = "sf")

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_coverage_union(grid_ddbs))
      expect_message(ddbs_coverage_union("grid", conn = conn_test, name = "cov_union"))
      expect_message(ddbs_coverage_union("grid", conn = conn_test, name = "cov_union", overwrite = TRUE))
      expect_true(ddbs_coverage_union("grid", conn = conn_test, name = "cov_union2"))

      expect_no_message(ddbs_coverage_union(grid_ddbs, quiet = TRUE))
      expect_no_message(ddbs_coverage_union("grid", conn = conn_test, name = "cov_union", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables correctly to DuckDB", {
      output_tbl <- ddbs_read_table(conn_test, "cov_union")
      expect_equal(
        ddbs_collect(ddbs_coverage_union(grid_ddbs))$geometry,
        output_tbl$geometry
      )
    })

    it("returns a single POLYGON with the total area and extent", {
      output <- ddbs_coverage_union(grid_ddbs, mode = "sf")

      expect_equal(nrow(output), 1L)
      expect_equal(as.character(sf::st_geometry_type(output)), "POLYGON")
      expect_equal(
        sf::st_area(sf::st_set_crs(output, NA)),
        sum(sf::st_area(sf::st_set_crs(grid_sf, NA)))
      )
      expect_equal(
        as.numeric(sf::st_bbox(output)),
        as.numeric(sf::st_bbox(grid_sf))
      )
    })

    it("matches the result of sf::st_union()", {
      output   <- ddbs_coverage_union(grid_sf, mode = "sf")
      expected <- sf::st_union(sf::st_set_crs(grid_sf, NA))

      expect_true(
        sf::st_equals(sf::st_set_crs(output, NA), expected, sparse = FALSE)[1, 1]
      )
    })

    it("preserves the CRS", {
      output <- ddbs_coverage_union(grid_ddbs, mode = "sf")
      expect_equal(sf::st_crs(output), sf::st_crs(grid_sf))
    })

  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("requires polygon geometries", {
      expect_error(ddbs_coverage_union(rivers_ddbs))
      expect_error(ddbs_coverage_union(points_sf))
    })

    it("requires a valid connection when using table name", {
      expect_error(ddbs_coverage_union("grid", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_coverage_union(x = 999))
      expect_error(ddbs_coverage_union(x = "999", conn = conn_test))
    })

    it("validates conn argument type", {
      expect_error(ddbs_coverage_union(grid_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_coverage_union(grid_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_coverage_union(grid_ddbs, quiet = 999))
    })

    it("requires name to be a single character string", {
      expect_error(ddbs_coverage_union(grid_ddbs, conn = conn_test, name = c("a", "b")))
    })

    it("errors on unexpected arguments", {
      expect_error(ddbs_coverage_union(grid_ddbs, new_column = 999))
    })

  })

})



# 2. ddbs_coverage_invalid_edges() ---------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns different output formats (duckspatial_df, sf)
## - CHECK 1.3: messages work
## - CHECK 1.4: writing a table works
## - CHECK 1.5: valid coverage returns an empty geometry
## - CHECK 1.6: invalid coverage returns the offending edges
## - CHECK 1.7: tolerance argument is accepted
## - CHECK 1.8: CRS is preserved
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_coverage_invalid_edges()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on all input formats", {
      output_ddbs <- ddbs_coverage_invalid_edges(bad_cov_ddbs)
      output_sf   <- ddbs_coverage_invalid_edges(bad_cov_sf)
      output_conn <- ddbs_coverage_invalid_edges("bad_cov", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats", {
      output_ddbs <- ddbs_coverage_invalid_edges(bad_cov_ddbs, mode = "duckspatial")
      output_sf   <- ddbs_coverage_invalid_edges(bad_cov_ddbs, mode = "sf")

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_coverage_invalid_edges(bad_cov_ddbs))
      expect_message(ddbs_coverage_invalid_edges("bad_cov", conn = conn_test, name = "cov_edges"))
      expect_message(ddbs_coverage_invalid_edges("bad_cov", conn = conn_test, name = "cov_edges", overwrite = TRUE))
      expect_true(ddbs_coverage_invalid_edges("bad_cov", conn = conn_test, name = "cov_edges2"))

      expect_no_message(ddbs_coverage_invalid_edges(bad_cov_ddbs, quiet = TRUE))
      expect_no_message(ddbs_coverage_invalid_edges("bad_cov", conn = conn_test, name = "cov_edges", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables correctly to DuckDB", {
      output_tbl <- ddbs_read_table(conn_test, "cov_edges")
      expect_equal(
        ddbs_collect(ddbs_coverage_invalid_edges(bad_cov_ddbs))$geometry,
        output_tbl$geometry
      )
    })

    it("returns an empty geometry for a valid coverage", {
      output <- ddbs_coverage_invalid_edges(grid_ddbs, mode = "sf")

      expect_equal(nrow(output), 1L)
      expect_true(sf::st_is_empty(output))
    })

    it("returns the invalid edges for an invalid coverage", {
      output <- ddbs_coverage_invalid_edges(bad_cov_ddbs, mode = "sf")

      expect_equal(nrow(output), 1L)
      expect_false(sf::st_is_empty(output))

      ## the invalid edges lie within the extent of the second polygon
      bbox_out <- sf::st_bbox(output)
      expect_gte(bbox_out[["xmin"]], 1)
      expect_lte(bbox_out[["xmax"]], 2)
    })

    it("accepts a tolerance argument", {
      output <- ddbs_coverage_invalid_edges(grid_ddbs, tolerance = 0.1, mode = "sf")
      expect_s3_class(output, "sf")
      expect_true(sf::st_is_empty(output))
    })

    it("preserves the CRS", {
      output <- ddbs_coverage_invalid_edges(bad_cov_ddbs, mode = "sf")
      expect_equal(sf::st_crs(output), sf::st_crs(bad_cov_sf))
    })

  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("validates tolerance argument type", {
      expect_error(ddbs_coverage_invalid_edges(grid_ddbs, tolerance = "0"))
    })

    it("requires polygon geometries", {
      expect_error(ddbs_coverage_invalid_edges(rivers_ddbs))
      expect_error(ddbs_coverage_invalid_edges(points_sf))
    })

    it("requires a valid connection when using table name", {
      expect_error(ddbs_coverage_invalid_edges("bad_cov", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_coverage_invalid_edges(x = 999))
      expect_error(ddbs_coverage_invalid_edges(x = "999", conn = conn_test))
    })

    it("validates conn argument type", {
      expect_error(ddbs_coverage_invalid_edges(grid_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_coverage_invalid_edges(grid_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_coverage_invalid_edges(grid_ddbs, quiet = 999))
    })

    it("requires name to be a single character string", {
      expect_error(ddbs_coverage_invalid_edges(grid_ddbs, conn = conn_test, name = c("a", "b")))
    })

    it("errors on unexpected arguments", {
      expect_error(ddbs_coverage_invalid_edges(grid_ddbs, new_column = 999))
    })

  })

})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)

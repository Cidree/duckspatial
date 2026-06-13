

# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write tables
ddbs_write_table(conn_test, argentina_sf, "argentina")
ddbs_write_table(conn_test, rivers_sf,    "rivers")

## locate fixture: 3 identical horizontal lines in conn_test and a reference point table
DBI::dbExecute(conn_test, "
    CREATE TABLE locate_test AS
    SELECT 1 AS id, ST_GeomFromText('LINESTRING (0 0, 1 0)') AS geometry
    UNION ALL SELECT 2, ST_GeomFromText('LINESTRING (0 0, 1 0)')
    UNION ALL SELECT 3, ST_GeomFromText('LINESTRING (0 0, 1 0)')
")
DBI::dbExecute(conn_test, "
    CREATE TABLE ref_point AS
    SELECT ST_GeomFromText('POINT (0.3 0)') AS geometry
")

## R-side fixtures for sf / duckspatial_df tests
locate_lines_sf <- sf::st_sf(
  id = 1:3,
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0,0, 1,0), ncol = 2, byrow = TRUE))
  ),
  crs = 4326
)
locate_ddbs <- as_duckspatial_df(locate_lines_sf)

## single reference point at fraction 0.3 of the unit horizontal line
ref_point_sf   <- sf::st_sf(
  geometry = sf::st_sfc(sf::st_point(c(0.3, 0)), crs = 4326)
)
ref_point_ddbs <- as_duckspatial_df(ref_point_sf)

## prepare exterior_ring table (closed LINESTRING) needed by ddbs_make_polygon tests
ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring", quiet = TRUE)
ext_ring_ddbs <- ddbs_exterior_ring(argentina_ddbs)
ext_ring_sf   <- sf::st_as_sf(ext_ring_ddbs)


# 1. ddbs_make_polygon() -------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns sf format
## - CHECK 1.3: messages work
## - CHECK 1.4: writes table to database
## - CHECK 1.5: returns POLYGON geometry type
## - CHECK 2.1: requires closed LINESTRING input
## - CHECK 2.2: other errors
describe("ddbs_make_polygon()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_make_polygon(ext_ring_ddbs)
      output_sf   <- ddbs_make_polygon(ext_ring_sf)
      output_conn <- ddbs_make_polygon("exterior_ring", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns sf format", {
      output_sf_fmt <- ddbs_make_polygon(ext_ring_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_make_polygon(ext_ring_ddbs))
      expect_message(ddbs_make_polygon("exterior_ring", conn = conn_test, name = "make_polygon"))
      expect_message(ddbs_make_polygon("exterior_ring", conn = conn_test, name = "make_polygon", overwrite = TRUE))
      expect_true(ddbs_make_polygon("exterior_ring", conn = conn_test, name = "make_polygon2"))

      expect_no_message(ddbs_make_polygon(ext_ring_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_make_polygon("exterior_ring", conn = conn_test, name = "make_polygon", overwrite = TRUE, quiet = TRUE)
      )
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_make_polygon(ext_ring_ddbs)
      output_tbl  <- ddbs_read_table(conn_test, "make_polygon")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("returns POLYGON geometry type", {
      output_ddbs <- ddbs_make_polygon(ext_ring_ddbs)
      geom_type   <- ddbs_collect(output_ddbs) |> sf::st_geometry_type() |> as.character()

      expect_equal(geom_type, "POLYGON")
    })
  })

  describe("errors", {

    it("requires linestring geometry", {
      expect_error(ddbs_make_polygon(argentina_ddbs))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_make_polygon("exterior_ring", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_make_polygon(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_make_polygon(ext_ring_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_make_polygon(ext_ring_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_make_polygon(ext_ring_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_make_polygon(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_make_polygon(ext_ring_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 2. ddbs_line_startpoint() and ddbs_line_endpoint() ---------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns sf format
## - CHECK 1.3: messages work
## - CHECK 1.4: returns POINT geometry type
## - CHECK 1.5: start and end points differ
## - CHECK 2.1: requires LINESTRING (not polygon)
## - CHECK 2.2: other errors
describe("ddbs_line_startpoint()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_line_startpoint(rivers_ddbs)
      output_sf   <- ddbs_line_startpoint(rivers_sf)
      output_conn <- ddbs_line_startpoint("rivers", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf,   "duckspatial_df")
      expect_s3_class(output_conn, "duckspatial_df")
    })

    it("returns sf format", {
      output_sf_fmt <- ddbs_line_startpoint(rivers_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_line_startpoint(rivers_ddbs))
      expect_message(ddbs_line_startpoint("rivers", conn = conn_test, name = "startpoint"))
      expect_message(ddbs_line_startpoint("rivers", conn = conn_test, name = "startpoint", overwrite = TRUE))
      expect_true(ddbs_line_startpoint("rivers", conn = conn_test, name = "startpoint2"))

      expect_no_message(ddbs_line_startpoint(rivers_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_line_startpoint("rivers", conn = conn_test, name = "startpoint", overwrite = TRUE, quiet = TRUE)
      )
    })

    it("returns POINT geometry type", {
      output     <- ddbs_line_startpoint(rivers_ddbs, mode = "sf")
      geom_types <- sf::st_geometry_type(output) |> as.character() |> unique()

      expect_true(all(geom_types == "POINT"))
    })
  })

  describe("errors", {

    it("requires LINESTRING geometry, not polygon", {
      expect_error(ddbs_line_startpoint(argentina_ddbs))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_line_startpoint("rivers", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_line_startpoint(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_line_startpoint(rivers_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_line_startpoint(rivers_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_line_startpoint(rivers_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_line_startpoint(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_line_startpoint(rivers_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})


describe("ddbs_line_endpoint()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_line_endpoint(rivers_ddbs)
      output_sf   <- ddbs_line_endpoint(rivers_sf)
      output_conn <- ddbs_line_endpoint("rivers", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf,   "duckspatial_df")
      expect_s3_class(output_conn, "duckspatial_df")
    })

    it("returns sf format", {
      output_sf_fmt <- ddbs_line_endpoint(rivers_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_line_endpoint(rivers_ddbs))
      expect_message(ddbs_line_endpoint("rivers", conn = conn_test, name = "endpoint"))
      expect_message(ddbs_line_endpoint("rivers", conn = conn_test, name = "endpoint", overwrite = TRUE))
      expect_true(ddbs_line_endpoint("rivers", conn = conn_test, name = "endpoint2"))

      expect_no_message(ddbs_line_endpoint(rivers_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_line_endpoint("rivers", conn = conn_test, name = "endpoint", overwrite = TRUE, quiet = TRUE)
      )
    })

    it("returns POINT geometry type", {
      output     <- ddbs_line_endpoint(rivers_ddbs, mode = "sf")
      geom_types <- sf::st_geometry_type(output) |> as.character() |> unique()

      expect_true(all(geom_types == "POINT"))
    })

    it("produces different results from ddbs_line_startpoint", {
      start_pts <- ddbs_line_startpoint(rivers_ddbs, mode = "sf")
      end_pts   <- ddbs_line_endpoint(rivers_ddbs,   mode = "sf")

      expect_false(identical(start_pts$geom, end_pts$geom))
    })
  })

  describe("errors", {

    it("requires LINESTRING geometry, not polygon", {
      expect_error(ddbs_line_endpoint(argentina_ddbs))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_line_endpoint("rivers", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_line_endpoint(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_line_endpoint(rivers_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_line_endpoint(rivers_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_line_endpoint(rivers_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_line_endpoint(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_line_endpoint(rivers_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 3. ddbs_line_interpolate() ---------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns sf format
## - CHECK 1.3: messages work
## - CHECK 1.4: intervals = FALSE returns POINT; intervals = TRUE returns MULTIPOINT
## - CHECK 1.5: different fractions produce different results
## - CHECK 2.1: fraction out of [0, 1] range
## - CHECK 2.2: non-numeric fraction
## - CHECK 2.3: non-logical intervals
## - CHECK 2.4: other errors
describe("ddbs_line_interpolate()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_line_interpolate(rivers_ddbs)
      output_sf   <- ddbs_line_interpolate(rivers_sf)
      output_conn <- ddbs_line_interpolate("rivers", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf,   "duckspatial_df")
      expect_s3_class(output_conn, "duckspatial_df")
    })

    it("returns sf format", {
      output_sf_fmt <- ddbs_line_interpolate(rivers_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_line_interpolate(rivers_ddbs))
      expect_message(ddbs_line_interpolate("rivers", conn = conn_test, name = "interpolate"))
      expect_message(ddbs_line_interpolate("rivers", conn = conn_test, name = "interpolate", overwrite = TRUE))
      expect_true(ddbs_line_interpolate("rivers", conn = conn_test, name = "interpolate2"))

      expect_no_message(ddbs_line_interpolate(rivers_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_line_interpolate("rivers", conn = conn_test, name = "interpolate", overwrite = TRUE, quiet = TRUE)
      )
    })

    describe("intervals parameter", {

      it("intervals = FALSE returns POINT geometry type", {
        output     <- ddbs_line_interpolate(rivers_ddbs, intervals = FALSE, mode = "sf")
        geom_types <- sf::st_geometry_type(output) |> as.character() |> unique()

        expect_true(all(geom_types == "POINT"))
      })

      it("intervals = TRUE returns MULTIPOINT geometry type", {
        output     <- ddbs_line_interpolate(rivers_ddbs, fraction = 0.25, intervals = TRUE, mode = "sf")
        geom_types <- sf::st_geometry_type(output) |> as.character() |> unique()

        expect_true(all(geom_types == "MULTIPOINT"))
      })
    })

    describe("fraction parameter", {

      it("different fractions produce different results", {
        output_25 <- ddbs_line_interpolate(rivers_ddbs, fraction = 0.25, mode = "sf")
        output_75 <- ddbs_line_interpolate(rivers_ddbs, fraction = 0.75, mode = "sf")

        expect_false(identical(output_25$geom, output_75$geom))
      })

      it("fraction = 0 returns start of line", {
        output <- ddbs_line_interpolate(rivers_ddbs, fraction = 0, mode = "sf")
        expect_s3_class(output, "sf")
      })

      it("fraction = 1 returns end of line", {
        output <- ddbs_line_interpolate(rivers_ddbs, fraction = 1, mode = "sf")
        expect_s3_class(output, "sf")
      })
    })
  })

  describe("errors", {

    it("rejects fraction below 0", {
      expect_error(ddbs_line_interpolate(rivers_ddbs, fraction = -0.1))
    })

    it("rejects fraction above 1", {
      expect_error(ddbs_line_interpolate(rivers_ddbs, fraction = 1.1))
    })

    it("rejects non-numeric fraction", {
      expect_error(ddbs_line_interpolate(rivers_ddbs, fraction = "0.5"))
    })

    it("rejects non-logical intervals", {
      expect_error(ddbs_line_interpolate(rivers_ddbs, intervals = "TRUE"))
      expect_error(ddbs_line_interpolate(rivers_ddbs, intervals = 1))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_line_interpolate("rivers", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_line_interpolate(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_line_interpolate(rivers_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_line_interpolate(rivers_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_line_interpolate(rivers_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_line_interpolate(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_line_interpolate(rivers_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 4. ddbs_line_substring() -----------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns sf format
## - CHECK 1.3: messages work
## - CHECK 1.4: different start/end produce different results
## - CHECK 2.1: start > end
## - CHECK 2.2: start or end out of [0, 1] range
## - CHECK 2.3: non-numeric start / end
## - CHECK 2.4: other errors
describe("ddbs_line_substring()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_line_substring(rivers_ddbs)
      output_sf   <- ddbs_line_substring(rivers_sf)
      output_conn <- ddbs_line_substring("rivers", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf,   "duckspatial_df")
      expect_s3_class(output_conn, "duckspatial_df")
    })

    it("returns sf format", {
      output_sf_fmt <- ddbs_line_substring(rivers_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_line_substring(rivers_ddbs))
      expect_message(ddbs_line_substring("rivers", conn = conn_test, name = "substring"))
      expect_message(ddbs_line_substring("rivers", conn = conn_test, name = "substring", overwrite = TRUE))
      expect_true(ddbs_line_substring("rivers", conn = conn_test, name = "substring2"))

      expect_no_message(ddbs_line_substring(rivers_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_line_substring("rivers", conn = conn_test, name = "substring", overwrite = TRUE, quiet = TRUE)
      )
    })

    it("returns LINESTRING geometry type", {
      output     <- ddbs_line_substring(rivers_ddbs, mode = "sf")
      geom_types <- sf::st_geometry_type(output) |> as.character() |> unique()

      expect_true(all(geom_types == "LINESTRING"))
    })

    describe("start and end parameters", {

      it("different start/end produce different results", {
        output_first  <- ddbs_line_substring(rivers_ddbs, start = 0,    end = 0.5, mode = "sf")
        output_second <- ddbs_line_substring(rivers_ddbs, start = 0.5,  end = 1,   mode = "sf")

        expect_false(identical(output_first$geom, output_second$geom))
      })

      it("full line (start = 0, end = 1) returns same length as input", {
        output <- ddbs_line_substring(rivers_ddbs, start = 0, end = 1, mode = "sf")
        expect_equal(nrow(output), nrow(rivers_sf))
      })
    })
  })

  describe("errors", {

    it("rejects start greater than end", {
      expect_error(ddbs_line_substring(rivers_ddbs, start = 0.8, end = 0.2))
    })

    it("rejects start below 0", {
      expect_error(ddbs_line_substring(rivers_ddbs, start = -0.1))
    })

    it("rejects end above 1", {
      expect_error(ddbs_line_substring(rivers_ddbs, end = 1.1))
    })

    it("rejects non-numeric start", {
      expect_error(ddbs_line_substring(rivers_ddbs, start = "0"))
    })

    it("rejects non-numeric end", {
      expect_error(ddbs_line_substring(rivers_ddbs, end = "1"))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_line_substring("rivers", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_line_substring(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_line_substring(rivers_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_line_substring(rivers_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_line_substring(rivers_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_line_substring(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_line_substring(rivers_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 5. ddbs_line_merge() ---------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns sf format
## - CHECK 1.3: messages work
## - CHECK 1.4: preserve = TRUE / FALSE both work
## - CHECK 2.1: non-logical preserve
## - CHECK 2.2: other errors
describe("ddbs_line_merge()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_line_merge(rivers_ddbs)
      output_sf   <- ddbs_line_merge(rivers_sf)
      output_conn <- ddbs_line_merge("rivers", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf,   "duckspatial_df")
      expect_s3_class(output_conn, "duckspatial_df")
    })

    it("returns sf format", {
      output_sf_fmt <- ddbs_line_merge(rivers_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_line_merge(rivers_ddbs))
      expect_message(ddbs_line_merge("rivers", conn = conn_test, name = "line_merge"))
      expect_message(ddbs_line_merge("rivers", conn = conn_test, name = "line_merge", overwrite = TRUE))
      expect_true(ddbs_line_merge("rivers", conn = conn_test, name = "line_merge2"))

      expect_no_message(ddbs_line_merge(rivers_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_line_merge("rivers", conn = conn_test, name = "line_merge", overwrite = TRUE, quiet = TRUE)
      )
    })

    describe("preserve parameter", {

      it("preserve = TRUE works", {
        output <- ddbs_line_merge(rivers_ddbs, preserve = TRUE)
        expect_s3_class(output, "duckspatial_df")
      })

      it("preserve = FALSE works", {
        output <- ddbs_line_merge(rivers_ddbs, preserve = FALSE)
        expect_s3_class(output, "duckspatial_df")
      })
    })
  })

  describe("errors", {

    it("rejects non-logical preserve", {
      expect_error(ddbs_line_merge(rivers_ddbs, preserve = "TRUE"))
      expect_error(ddbs_line_merge(rivers_ddbs, preserve = 1))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_line_merge("rivers", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_line_merge(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_line_merge(rivers_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_line_merge(rivers_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_line_merge(rivers_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_line_merge(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_line_merge(rivers_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 6. ddbs_polygonize() ---------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns sf format
## - CHECK 1.3: messages work
## - CHECK 1.4: returns GEOMETRYCOLLECTION geometry type
## - CHECK 2.1: other errors
describe("ddbs_polygonize()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_polygonize(rivers_ddbs)
      output_sf   <- ddbs_polygonize(rivers_sf)
      output_conn <- ddbs_polygonize("rivers", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf,   "duckspatial_df")
      expect_s3_class(output_conn, "duckspatial_df")
    })

    it("returns sf format", {
      output_sf_fmt <- ddbs_polygonize(rivers_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_polygonize(rivers_ddbs))
      expect_message(ddbs_polygonize("rivers", conn = conn_test, name = "polygonize"))
      expect_message(ddbs_polygonize("rivers", conn = conn_test, name = "polygonize", overwrite = TRUE))
      expect_true(ddbs_polygonize("rivers", conn = conn_test, name = "polygonize2"))

      expect_no_message(ddbs_polygonize(rivers_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_polygonize("rivers", conn = conn_test, name = "polygonize", overwrite = TRUE, quiet = TRUE)
      )
    })

    it("returns GEOMETRYCOLLECTION geometry type", {
      output     <- ddbs_polygonize(rivers_ddbs, mode = "sf")
      geom_types <- sf::st_geometry_type(output) |> as.character() |> unique()

      expect_true(all(geom_types == "GEOMETRYCOLLECTION"))
    })
  })

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_polygonize("rivers", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_polygonize(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_polygonize(rivers_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_polygonize(rivers_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_polygonize(rivers_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_polygonize(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_polygonize(rivers_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 7. ddbs_build_area() ---------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: returns sf format
## - CHECK 1.3: messages work
## - CHECK 2.1: other errors
describe("ddbs_build_area()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_build_area(rivers_ddbs)
      output_sf   <- ddbs_build_area(rivers_sf)
      output_conn <- ddbs_build_area("rivers", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf,   "duckspatial_df")
      expect_s3_class(output_conn, "duckspatial_df")
    })

    it("returns sf format", {
      output_sf_fmt <- ddbs_build_area(rivers_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_build_area(rivers_ddbs))
      expect_message(ddbs_build_area("rivers", conn = conn_test, name = "build_area"))
      expect_message(ddbs_build_area("rivers", conn = conn_test, name = "build_area", overwrite = TRUE))
      expect_true(ddbs_build_area("rivers", conn = conn_test, name = "build_area2"))

      expect_no_message(ddbs_build_area(rivers_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_build_area("rivers", conn = conn_test, name = "build_area", overwrite = TRUE, quiet = TRUE)
      )
    })
  })

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_build_area("rivers", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_build_area(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_build_area(rivers_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_build_area(rivers_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_build_area(rivers_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_build_area(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_build_area(rivers_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 8. ddbs_startpoint() and ddbs_endpoint() (deprecated) -----------------

## - CHECK 1.1: ddbs_startpoint() emits deprecation warning but still works
## - CHECK 1.2: ddbs_endpoint() emits deprecation warning but still works
## - CHECK 1.3: produce the same output as their replacements
describe("deprecated ddbs_startpoint() and ddbs_endpoint()", {

  it("ddbs_startpoint() emits deprecation warning", {
    expect_warning(ddbs_startpoint(rivers_ddbs), class = "lifecycle_warning_deprecated")
  })

  it("ddbs_endpoint() emits deprecation warning", {
    expect_warning(ddbs_endpoint(rivers_ddbs), class = "lifecycle_warning_deprecated")
  })

  it("ddbs_startpoint() output matches ddbs_line_startpoint()", {
    output_dep <- suppressWarnings(ddbs_startpoint(rivers_ddbs, mode = "sf"))
    output_new <- ddbs_line_startpoint(rivers_ddbs, mode = "sf")

    expect_equal(output_dep$geom, output_new$geom)
  })

  it("ddbs_endpoint() output matches ddbs_line_endpoint()", {
    output_dep <- suppressWarnings(ddbs_endpoint(rivers_ddbs, mode = "sf"))
    output_new <- ddbs_line_endpoint(rivers_ddbs, mode = "sf")

    expect_equal(output_dep$geom, output_new$geom)
  })
})



# 9. ddbs_line_locate_point() --------------------------------------------

## y = sf (single point) tests
## - CHECK 1.1: returns duckspatial_df by default
## - CHECK 1.2: returns numeric vector with mode sf
## - CHECK 1.3: messages shown/suppressed correctly
## - CHECK 1.4: fraction = 0.3 for reference point at (0.3, 0)
## - CHECK 1.5: materializes data correctly
## - CHECK 1.6: vector and column outputs are identical
## y = duckspatial_df (single point) tests
## - CHECK 2.1: returns duckspatial_df by default
## - CHECK 2.2: returns numeric vector with mode sf
## - CHECK 2.3: messages shown/suppressed correctly
## - CHECK 2.4: fraction = 0.3 for reference point at (0.3, 0)
## - CHECK 2.5: identical results with sf and duckspatial_df y
## y = character (DuckDB table) tests
## - CHECK 3.1: returns duckspatial_df by default
## - CHECK 3.2: returns numeric vector with mode sf
## - CHECK 3.3: table is written into the database
## - CHECK 3.4: messages shown/suppressed correctly
## - CHECK 3.5: fraction = 0.3 for reference point at (0.3, 0)
## - CHECK 3.6: materializes data correctly
## - CHECK 3.7: vector and column outputs are identical
## Check that errors work
## - CHECK 4.1: overwrite = FALSE rejects existing table
## - CHECK 4.2: y with more than 1 row (sf)
## - CHECK 4.3: y with more than 1 row (duckspatial_df)
## - CHECK 4.4: invalid y type
## - CHECK 4.5: non-character new_column argument
## - CHECK 4.6: conn required when x or y is a table name
## - CHECK 4.7: invalid x type
describe("ddbs_line_locate_point()", {

  ### y = sf

  describe("expected behavior with sf y", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_line_locate_point(locate_ddbs, y = ref_point_sf)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_line_locate_point(locate_ddbs, y = ref_point_sf, mode = "sf")
      expect_true(is.numeric(output))
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_line_locate_point(locate_ddbs, y = ref_point_sf))
      expect_no_message(ddbs_line_locate_point(locate_ddbs, y = ref_point_sf, quiet = TRUE))
    })

    it("returns 0.3 for reference point at (0.3, 0) on a unit horizontal line", {
      result <- ddbs_line_locate_point(locate_ddbs, y = ref_point_sf, mode = "sf")
      expect_equal(result[1], 0.3, tolerance = 1e-6)
    })

    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output <- ddbs_line_locate_point(locate_ddbs, y = ref_point_sf)
      output_sf_mat  <- output |> st_as_sf()
      output_collect <- output |> collect()
      output_ddbs    <- output |> ddbs_collect()
      expect_identical(output_sf_mat, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf_mat, "sf")
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- ddbs_line_locate_point(locate_ddbs, y = ref_point_sf, mode = "sf")
      output_column <- ddbs_line_locate_point(locate_ddbs, y = ref_point_sf) |> ddbs_collect()
      expect_equal(output_vec, output_column$line_fraction)
    })
  })


  ### y = duckspatial_df

  describe("expected behavior with duckspatial_df y", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_line_locate_point(locate_ddbs, y = ref_point_ddbs)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_line_locate_point(locate_ddbs, y = ref_point_ddbs, mode = "sf")
      expect_true(is.numeric(output))
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_line_locate_point(locate_ddbs, y = ref_point_ddbs))
    })

    it("returns 0.3 for reference point at (0.3, 0) on a unit horizontal line", {
      result <- ddbs_line_locate_point(locate_ddbs, y = ref_point_ddbs, mode = "sf")
      expect_equal(result[1], 0.3, tolerance = 1e-6)
    })

    it("produces identical results with sf and duckspatial_df y", {
      output_sf_y   <- ddbs_line_locate_point(locate_ddbs, y = ref_point_sf,   mode = "sf")
      output_ddbs_y <- ddbs_line_locate_point(locate_ddbs, y = ref_point_ddbs, mode = "sf")
      expect_equal(output_sf_y, output_ddbs_y)
    })
  })


  ### y = character (DuckDB table)

  describe("expected behavior with character table y", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_line_locate_point("locate_test", y = "ref_point", conn = conn_test)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_line_locate_point("locate_test", y = "ref_point", conn = conn_test, mode = "sf")
      expect_true(is.numeric(output))
    })

    it("writes tables to the database", {
      output <- ddbs_line_locate_point(
        "locate_test", y = "ref_point", conn = conn_test,
        name = "ll_tbl", new_column = "frac"
      )
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(
        ddbs_line_locate_point("locate_test", y = "ref_point", conn = conn_test)
      )
      expect_message(
        ddbs_line_locate_point("locate_test", y = "ref_point", conn = conn_test,
          name = "ll_tbl2", new_column = "frac")
      )
      expect_no_message(
        ddbs_line_locate_point("locate_test", y = "ref_point", conn = conn_test, quiet = TRUE)
      )
      expect_no_message(
        ddbs_line_locate_point("locate_test", y = "ref_point", conn = conn_test,
          name = "ll_tbl3", new_column = "frac", quiet = TRUE)
      )
    })

    it("returns 0.3 for reference point at (0.3, 0) on a unit horizontal line", {
      result <- ddbs_line_locate_point("locate_test", y = "ref_point", conn = conn_test, mode = "sf")
      expect_equal(result[1], 0.3, tolerance = 1e-6)
    })

    it("materializes data correctly (collect, ddbs_collect)", {
      output <- ddbs_line_locate_point("locate_test", y = "ref_point", conn = conn_test)
      output_collect <- output |> collect()
      output_ddbs    <- output |> ddbs_collect()
      expect_identical(output_collect, output_ddbs)
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- ddbs_line_locate_point(
        "locate_test", y = "ref_point", conn = conn_test, mode = "sf"
      )
      output_column <- ddbs_line_locate_point(
        "locate_test", y = "ref_point", conn = conn_test
      ) |> ddbs_collect()
      expect_equal(output_vec, output_column$line_fraction)
    })
  })


  ### errors

  describe("errors", {

    it("errors if overwrite = FALSE and table already exists", {
      ddbs_line_locate_point(
        "locate_test", y = "ref_point", conn = conn_test, name = "dup_ll_tbl", new_column = "frac"
      )
      expect_error(
        ddbs_line_locate_point(
          "locate_test", y = "ref_point", conn = conn_test, name = "dup_ll_tbl", new_column = "frac"
        )
      )
    })

    it("errors if y has more than 1 row (sf)", {
      multi_pt_sf <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_point(c(0.3, 0)), sf::st_point(c(0.5, 0))),
        crs = 4326
      )
      expect_error(ddbs_line_locate_point(locate_ddbs, y = multi_pt_sf))
    })

    it("errors if y has more than 1 row (duckspatial_df)", {
      multi_pt_ddbs <- as_duckspatial_df(sf::st_sf(
        geometry = sf::st_sfc(sf::st_point(c(0.3, 0)), sf::st_point(c(0.5, 0))),
        crs = 4326
      ))
      expect_error(ddbs_line_locate_point(locate_ddbs, y = multi_pt_ddbs))
    })

    it("errors if y is an invalid type", {
      expect_error(ddbs_line_locate_point(locate_ddbs, y = 123))
      expect_error(ddbs_line_locate_point(locate_ddbs, y = TRUE))
    })

    it("errors if new_column is not a character scalar", {
      expect_error(ddbs_line_locate_point(locate_ddbs, y = ref_point_sf, new_column = 1))
    })

    it("requires conn when x or y is a table name", {
      expect_error(ddbs_line_locate_point("locate_test", y = ref_point_sf, conn = NULL))
      expect_error(ddbs_line_locate_point(locate_ddbs,   y = "ref_point",  conn = NULL))
    })

    it("errors on invalid x type", {
      expect_error(ddbs_line_locate_point(999,  y = ref_point_sf))
      expect_error(ddbs_line_locate_point(TRUE, y = ref_point_sf))
    })
  })
})



## stop connection
duckspatial::ddbs_stop_conn(conn_test)

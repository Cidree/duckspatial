

# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## 2D source data with a numeric column that will become the M value
src_sf <- sf::st_as_sf(
  data.frame(id = 1:2, m_val = c(50, 50)),
  geometry = sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 1, 0, 2, 0), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0, 1, 1, 1, 2, 1), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
)
src_ddbs <- duckspatial::as_duckspatial_df(src_sf)

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write 2D source to conn_test, then materialise M-geometry via ddbs_force_3d
## (writing XYM WKB directly from sf fails in DuckDB; SQL-based creation works)
duckspatial::ddbs_write_table(conn_test, src_sf, "lines_2d")
ddbs_force_3d("lines_2d", "m_val", dim = "m", conn = conn_test,
              name = "lines_m", quiet = TRUE)

## Fixtures
## - line_m_sf:   sf read back from DuckDB (M-geometry encoded by DuckDB, readable again)
## - line_m_ddbs: duckspatial_df with lazy M-geometry created by ddbs_force_3d
line_m_sf   <- duckspatial::ddbs_read_table(conn_test, "lines_m")
line_m_ddbs <- ddbs_force_3d(src_ddbs, "m_val", dim = "m")


# 1. ddbs_locate_along() -------------------------------------------------

describe("ddbs_locate_along()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_locate_along(line_m_ddbs, measure = 50)
      output_sf   <- ddbs_locate_along(line_m_sf, measure = 50)
      output_conn <- ddbs_locate_along("lines_m", measure = 50, conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_locate_along(line_m_ddbs, measure = 50, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_locate_along(line_m_ddbs, measure = 50))
      expect_message(ddbs_locate_along("lines_m", 50, conn = conn_test, name = "locate_along"))
      expect_message(ddbs_locate_along("lines_m", 50, conn = conn_test, name = "locate_along", overwrite = TRUE))
      expect_true(ddbs_locate_along("lines_m", 50, conn = conn_test, name = "locate_along2"))

      expect_no_message(ddbs_locate_along(line_m_ddbs, 50, quiet = TRUE))
      expect_no_message(ddbs_locate_along("lines_m", 50, conn = conn_test, name = "locate_along", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_locate_along(line_m_ddbs, measure = 50)
      output_tbl  <- ddbs_read_table(conn_test, "locate_along")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("returns one row per feature at the given M value", {
      output <- ddbs_locate_along(line_m_ddbs, measure = 50, mode = "sf")
      expect_equal(nrow(output), 2L)
    })

    it("filters out features with no point at the given M value", {
      output <- ddbs_locate_along(line_m_ddbs, measure = 999, mode = "sf")
      expect_equal(nrow(output), 0L)
    })

    describe("offset parameter", {

      it("works with default value (0)", {
        output <- ddbs_locate_along(line_m_ddbs, measure = 50, offset = 0)
        expect_s3_class(output, "duckspatial_df")
      })

      it("works with non-zero offset", {
        output <- ddbs_locate_along(line_m_ddbs, measure = 50, offset = 1)
        expect_s3_class(output, "duckspatial_df")
      })
    })
  })

  describe("errors", {

    it("requires numeric measure", {
      expect_error(ddbs_locate_along(line_m_ddbs, measure = "50"))
    })

    it("requires numeric offset", {
      expect_error(ddbs_locate_along(line_m_ddbs, measure = 50, offset = "1"))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_locate_along("lines_m", measure = 50, conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_locate_along(x = 999, measure = 50))
    })

    it("validates conn argument type", {
      expect_error(ddbs_locate_along(line_m_ddbs, measure = 50, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_locate_along(line_m_ddbs, measure = 50, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_locate_along(line_m_ddbs, measure = 50, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_locate_along("999", measure = 50, conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_locate_along(line_m_ddbs, 50, conn = conn_test, name = c("banana", "banana")))
    })
  })
})




# 2. ddbs_locate_between() -----------------------------------------------

describe("ddbs_locate_between()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_locate_between(line_m_ddbs, start_measure = 25, end_measure = 75)
      output_sf   <- ddbs_locate_between(line_m_sf, start_measure = 25, end_measure = 75)
      output_conn <- ddbs_locate_between("lines_m", start_measure = 25, end_measure = 75, conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_sf, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_locate_between(line_m_ddbs, 25, 75, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_locate_between(line_m_ddbs, 25, 75))
      expect_message(ddbs_locate_between("lines_m", 25, 75, conn = conn_test, name = "locate_between"))
      expect_message(ddbs_locate_between("lines_m", 25, 75, conn = conn_test, name = "locate_between", overwrite = TRUE))
      expect_true(ddbs_locate_between("lines_m", 25, 75, conn = conn_test, name = "locate_between2"))

      expect_no_message(ddbs_locate_between(line_m_ddbs, 25, 75, quiet = TRUE))
      expect_no_message(ddbs_locate_between("lines_m", 25, 75, conn = conn_test, name = "locate_between", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_locate_between(line_m_ddbs, 25, 75)
      output_tbl  <- ddbs_read_table(conn_test, "locate_between")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("returns one row per feature within the M range", {
      output <- ddbs_locate_between(line_m_ddbs, 25, 75, mode = "sf")
      expect_equal(nrow(output), 2L)
    })

    it("filters out features outside the M range", {
      output <- ddbs_locate_between(line_m_ddbs, 200, 300, mode = "sf")
      expect_equal(nrow(output), 0L)
    })

    describe("offset parameter", {

      it("works with default value (0)", {
        output <- ddbs_locate_between(line_m_ddbs, 25, 75, offset = 0)
        expect_s3_class(output, "duckspatial_df")
      })

      it("works with non-zero offset", {
        output <- ddbs_locate_between(line_m_ddbs, 25, 75, offset = 1)
        expect_s3_class(output, "duckspatial_df")
      })
    })
  })

  describe("errors", {

    it("requires numeric start_measure", {
      expect_error(ddbs_locate_between(line_m_ddbs, start_measure = "0", end_measure = 75))
    })

    it("requires numeric end_measure", {
      expect_error(ddbs_locate_between(line_m_ddbs, start_measure = 25, end_measure = "75"))
    })

    it("requires numeric offset", {
      expect_error(ddbs_locate_between(line_m_ddbs, 25, 75, offset = "1"))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_locate_between("lines_m", 25, 75, conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_locate_between(x = 999, start_measure = 25, end_measure = 75))
    })

    it("validates conn argument type", {
      expect_error(ddbs_locate_between(line_m_ddbs, 25, 75, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_locate_between(line_m_ddbs, 25, 75, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_locate_between(line_m_ddbs, 25, 75, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_locate_between("999", 25, 75, conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_locate_between(line_m_ddbs, 25, 75, conn = conn_test, name = c("banana", "banana")))
    })
  })
})


## stop connection
ddbs_stop_conn(conn_test)

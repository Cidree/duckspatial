

# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## nc_sf uses NAD27 CRS, which DuckDB returns as "EPSG:4267" — a string mismatch
## that breaks expect_equal. Reproject to EPSG:4326 so the CRS round-trips cleanly.
## nc still has the AREA and PERIMETER columns needed for var/var_z/var_m.
nc_sf_local    <- sf::st_transform(nc_sf, "EPSG:4326")
nc_ddbs_local  <- duckspatial::as_duckspatial_df(nc_sf_local)

## write data
duckspatial::ddbs_write_table(conn_test, nc_sf_local, "nc")


# 1. ddbs_force_2d() -----------------------------------------------------

describe("ddbs_force_2d()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_force_2d(nc_ddbs_local)
      output_sf   <- ddbs_force_2d(nc_sf_local)
      output_conn <- ddbs_force_2d("nc", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_force_2d(nc_ddbs_local, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_force_2d(nc_ddbs_local))
      expect_message(ddbs_force_2d("nc", conn = conn_test, name = "force_2d"))
      expect_message(ddbs_force_2d("nc", conn = conn_test, name = "force_2d", overwrite = TRUE))
      expect_true(ddbs_force_2d("nc", conn = conn_test, name = "force_2d2"))

      expect_no_message(ddbs_force_2d(nc_ddbs_local, quiet = TRUE))
      expect_no_message(ddbs_force_2d("nc", conn = conn_test, name = "force_2d", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_force_2d(nc_ddbs_local)
      output_tbl  <- ddbs_read_table(conn_test, "force_2d")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("strips Z dimension from 3D geometry", {
      nc_3d      <- ddbs_force_3d(nc_ddbs_local, "AREA")
      result     <- ddbs_force_2d(nc_3d, mode = "sf")
      first_geom <- sf::st_geometry(result)[[1]]
      expect_false("XYZ"  %in% class(first_geom))
      expect_false("XYZM" %in% class(first_geom))
    })
  })

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_force_2d("nc", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_force_2d(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_force_2d(nc_ddbs_local, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_force_2d(nc_ddbs_local, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_force_2d(nc_ddbs_local, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_force_2d(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_force_2d(nc_ddbs_local, conn = conn_test, name = c("banana", "banana")))
    })
  })
})




# 2. ddbs_force_3d() -----------------------------------------------------

describe("ddbs_force_3d()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_force_3d(nc_ddbs_local, "AREA")
      output_sf   <- ddbs_force_3d(nc_sf_local, "AREA")
      output_conn <- ddbs_force_3d("nc", "AREA", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_force_3d(nc_ddbs_local, "AREA", mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_force_3d(nc_ddbs_local, "AREA"))
      expect_message(ddbs_force_3d("nc", "AREA", conn = conn_test, name = "force_3d"))
      expect_message(ddbs_force_3d("nc", "AREA", conn = conn_test, name = "force_3d", overwrite = TRUE))
      expect_true(ddbs_force_3d("nc", "AREA", conn = conn_test, name = "force_3d2"))

      expect_no_message(ddbs_force_3d(nc_ddbs_local, "AREA", quiet = TRUE))
      expect_no_message(ddbs_force_3d("nc", "AREA", conn = conn_test, name = "force_3d", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_force_3d(nc_ddbs_local, "AREA")
      output_tbl  <- ddbs_read_table(conn_test, "force_3d")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    describe("dim parameter", {

      it("adds Z coordinate when dim = 'z' (default)", {
        result     <- ddbs_force_3d(nc_ddbs_local, "AREA", dim = "z", mode = "sf")
        first_geom <- sf::st_geometry(result)[[1]]
        expect_true("XYZ" %in% class(first_geom) || "XYZM" %in% class(first_geom))
      })

      it("adds M coordinate when dim = 'm'", {
        result     <- ddbs_force_3d(nc_ddbs_local, "AREA", dim = "m", mode = "sf")
        first_geom <- sf::st_geometry(result)[[1]]
        expect_true("XYM" %in% class(first_geom) || "XYZM" %in% class(first_geom))
      })

      it("is case-insensitive", {
        output_upper <- ddbs_force_3d(nc_ddbs_local, "AREA", dim = "Z")
        output_lower <- ddbs_force_3d(nc_ddbs_local, "AREA", dim = "z")
        expect_equal(ddbs_collect(output_upper), ddbs_collect(output_lower))
      })
    })
  })

  describe("errors", {

    it("rejects invalid dim values", {
      expect_error(ddbs_force_3d(nc_ddbs_local, "AREA", dim = "w"))
      expect_error(ddbs_force_3d(nc_ddbs_local, "AREA", dim = "xyz"))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_force_3d("nc", "AREA", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_force_3d(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_force_3d(nc_ddbs_local, "AREA", conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_force_3d(nc_ddbs_local, "AREA", overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_force_3d(nc_ddbs_local, "AREA", quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_force_3d(x = "999", "AREA", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_force_3d(nc_ddbs_local, "AREA", conn = conn_test, name = c("banana", "banana")))
    })
  })
})




# 3. ddbs_force_4d() -----------------------------------------------------

describe("ddbs_force_4d()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER")
      output_sf   <- ddbs_force_4d(nc_sf_local, "AREA", "PERIMETER")
      output_conn <- ddbs_force_4d("nc", "AREA", "PERIMETER", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER", mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER"))
      expect_message(ddbs_force_4d("nc", "AREA", "PERIMETER", conn = conn_test, name = "force_4d"))
      expect_message(ddbs_force_4d("nc", "AREA", "PERIMETER", conn = conn_test, name = "force_4d", overwrite = TRUE))
      expect_true(ddbs_force_4d("nc", "AREA", "PERIMETER", conn = conn_test, name = "force_4d2"))

      expect_no_message(ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER", quiet = TRUE))
      expect_no_message(ddbs_force_4d("nc", "AREA", "PERIMETER", conn = conn_test, name = "force_4d", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER")
      output_tbl  <- ddbs_read_table(conn_test, "force_4d")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("adds both Z and M coordinates", {
      result     <- ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER", mode = "sf")
      first_geom <- sf::st_geometry(result)[[1]]
      expect_true("XYZM" %in% class(first_geom))
    })
  })

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_force_4d("nc", "AREA", "PERIMETER", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_force_4d(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER", conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER", overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER", quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_force_4d(x = "999", "AREA", "PERIMETER", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_force_4d(nc_ddbs_local, "AREA", "PERIMETER", conn = conn_test, name = c("banana", "banana")))
    })
  })
})


## stop connection
ddbs_stop_conn(conn_test)

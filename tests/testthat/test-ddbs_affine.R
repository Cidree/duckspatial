

# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write some data
ddbs_write_table(conn_test, argentina_sf, "argentina")
ddbs_write_table(conn_test, nc_sf, "nc")

# 1. ddbs_rotate() -------------------------------------------------------

## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
describe("ddbs_rotate()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on ddbs input", {
      output_ddbs_1 <- ddbs_rotate(argentina_ddbs, 45)
      output_ddbs_2 <- ddbs_rotate(argentina_ddbs, 45, units = "radians")
      output_ddbs_3 <- ddbs_rotate(argentina_ddbs, 45, by_feature = TRUE)
      output_ddbs_4 <- ddbs_rotate(argentina_ddbs, 45, by_feature = TRUE, center_x = 0, center_y = 0)
      output_ddbs_5 <- ddbs_rotate(argentina_ddbs, 45, quiet = TRUE)

      expect_s3_class(output_ddbs_1, "duckspatial_df")
      expect_s3_class(output_ddbs_2, "duckspatial_df")
      expect_s3_class(output_ddbs_3, "duckspatial_df")
      expect_s3_class(output_ddbs_4, "duckspatial_df")
      expect_s3_class(output_ddbs_5, "duckspatial_df")
    })

    it("returns different output formats for ddbs input", {
      output_sf <- ddbs_rotate(argentina_ddbs, 45, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on sf input", {
      output_sf_1 <- ddbs_rotate(argentina_sf, 45)
      output_sf_2 <- ddbs_rotate(argentina_sf, 45, units = "radians")
      output_sf_3 <- ddbs_rotate(argentina_sf, 45, by_feature = TRUE)
      output_sf_4 <- ddbs_rotate(argentina_sf, 45, by_feature = TRUE, center_x = 0, center_y = 0)
      output_sf_5 <- ddbs_rotate(argentina_sf, 45, quiet = TRUE)

      expect_s3_class(output_sf_1, "duckspatial_df")
      expect_s3_class(output_sf_2, "duckspatial_df")
      expect_s3_class(output_sf_3, "duckspatial_df")
      expect_s3_class(output_sf_4, "duckspatial_df")
      expect_s3_class(output_sf_5, "duckspatial_df")
    })

    it("returns different output formats for sf input", {
      output_sf <- ddbs_rotate(argentina_sf, 45, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on DuckDB table input", {
      output_conn_1 <- ddbs_rotate("argentina", 45, conn = conn_test)
      output_conn_2 <- ddbs_rotate("argentina", 45, conn = conn_test, units = "radians")
      output_conn_3 <- ddbs_rotate("argentina", 45, conn = conn_test, by_feature = TRUE)
      output_conn_4 <- ddbs_rotate("argentina", 45, conn = conn_test, by_feature = TRUE, center_x = 0, center_y = 0)
      output_conn_5 <- ddbs_rotate("argentina", 45, conn = conn_test, quiet = TRUE)

      expect_s3_class(output_conn_1, "duckspatial_df")
      expect_s3_class(output_conn_2, "duckspatial_df")
      expect_s3_class(output_conn_3, "duckspatial_df")
      expect_s3_class(output_conn_4, "duckspatial_df")
      expect_s3_class(output_conn_5, "duckspatial_df")
    })

    it("returns different output formats for DuckDB table input", {
      output_sf <- ddbs_rotate("argentina", 45, conn = conn_test, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_rotate(argentina_ddbs, 50))
      expect_message(ddbs_rotate("argentina", 50, conn = conn_test, name = "rotated"))
      expect_message(ddbs_rotate("argentina", 50, conn = conn_test, name = "rotated", overwrite = TRUE))
      expect_true(ddbs_rotate("argentina", 50, conn = conn_test, name = "rotated2"))

      expect_no_message(ddbs_rotate(argentina_ddbs, 50, quiet = TRUE))
      expect_no_message(
        ddbs_rotate(
          "argentina",
          50,
          conn = conn_test,
          name = "rotated",
          overwrite = TRUE,
          quiet = TRUE
        )
      )
    })
  })

  ### EXPECTED ERRORS

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_rotate("argentina", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_rotate(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_rotate(argentina_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_rotate(argentina_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_rotate(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_rotate(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_rotate(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_rotate(argentina_ddbs, conn = conn_test, name = c("banana", "banana")))
    })

    it("validates required argument combinations", {
      expect_error(ddbs_rotate(argentina_ddbs))
      expect_error(ddbs_rotate(argentina_ddbs, by_feature = FALSE, center_x = 5, center_y = 5))
      expect_error(ddbs_rotate(argentina_ddbs, by_feature = TRUE, center_x = 5))
      expect_error(ddbs_rotate(argentina_ddbs, by_feature = TRUE, center_y = 5))
    })
  })
    
})




# 2. ddbs_rotate_3d ------------------------------------------------------

## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
describe("ddbs_rotate_3d()", {

  ### EXPECTED BEHAVIOR

  describe("expected behavior", {

    it("works on ddbs input", {
      output_ddbs_1 <- ddbs_rotate_3d(argentina_ddbs, 45)
      output_ddbs_2 <- ddbs_rotate_3d(argentina_ddbs, 45, units = "radians")
      output_ddbs_3 <- ddbs_rotate_3d(argentina_ddbs, 90, axis = "y")
      output_ddbs_4 <- ddbs_rotate_3d(argentina_ddbs, 180, axis = "z")
      output_ddbs_5 <- ddbs_rotate_3d(argentina_ddbs, 45, quiet = TRUE)

      expect_s3_class(output_ddbs_1, "duckspatial_df")
      expect_s3_class(output_ddbs_2, "duckspatial_df")
      expect_s3_class(output_ddbs_3, "duckspatial_df")
      expect_s3_class(output_ddbs_4, "duckspatial_df")
      expect_s3_class(output_ddbs_5, "duckspatial_df")
    })

    it("returns different output formats for ddbs input", {
      output_sf <- ddbs_rotate_3d(argentina_ddbs, 45, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on sf input", {
      output_sf_1 <- ddbs_rotate_3d(argentina_sf, 45)
      output_sf_2 <- ddbs_rotate_3d(argentina_sf, 45, units = "radians")
      output_sf_3 <- ddbs_rotate_3d(argentina_sf, 90, axis = "y")
      output_sf_4 <- ddbs_rotate_3d(argentina_sf, 180, axis = "z")
      output_sf_5 <- ddbs_rotate_3d(argentina_sf, 45, quiet = TRUE)

      expect_s3_class(output_sf_1, "duckspatial_df")
      expect_s3_class(output_sf_2, "duckspatial_df")
      expect_s3_class(output_sf_3, "duckspatial_df")
      expect_s3_class(output_sf_4, "duckspatial_df")
      expect_s3_class(output_sf_5, "duckspatial_df")
    })

    it("returns different output formats for sf input", {
      output_sf <- ddbs_rotate_3d(argentina_sf, 45, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on DuckDB table input", {
      output_conn_1 <- ddbs_rotate_3d("argentina", 45, conn = conn_test)
      output_conn_2 <- ddbs_rotate_3d("argentina", 45, conn = conn_test, units = "radians")
      output_conn_3 <- ddbs_rotate_3d("argentina", 90, conn = conn_test, axis = "y")
      output_conn_4 <- ddbs_rotate_3d("argentina", 180, conn = conn_test, axis = "z")
      output_conn_5 <- ddbs_rotate_3d("argentina", 45, conn = conn_test, quiet = TRUE)

      expect_s3_class(output_conn_1, "duckspatial_df")
      expect_s3_class(output_conn_2, "duckspatial_df")
      expect_s3_class(output_conn_3, "duckspatial_df")
      expect_s3_class(output_conn_4, "duckspatial_df")
      expect_s3_class(output_conn_5, "duckspatial_df")
    })

    it("returns different output formats for DuckDB table input", {
      output_sf <- ddbs_rotate_3d("argentina", 45, conn = conn_test, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_rotate_3d(argentina_ddbs, 50))
      expect_message(ddbs_rotate_3d("argentina", 50, conn = conn_test, name = "rotated_3d"))
      expect_message(ddbs_rotate_3d("argentina", 50, conn = conn_test, name = "rotated_3d", overwrite = TRUE))
      expect_true(ddbs_rotate_3d("argentina", 50, conn = conn_test, name = "rotated_3d2"))

      expect_no_message(ddbs_rotate_3d(argentina_ddbs, 50, quiet = TRUE))
      expect_no_message(
        ddbs_rotate_3d(
          "argentina",
          50,
          conn = conn_test,
          name = "rotated_3d",
          overwrite = TRUE,
          quiet = TRUE
        )
      )
    })
  })

  ### EXPECTED ERRORS

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_rotate_3d("argentina", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_rotate_3d(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_rotate_3d(argentina_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_rotate_3d(argentina_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_rotate_3d(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_rotate_3d(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_rotate_3d(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_rotate_3d(argentina_ddbs, conn = conn_test, name = c("banana", "banana")))
    })

    it("validates units argument", {
      expect_error(ddbs_rotate_3d(argentina_ddbs, units = "asdfasdf"))
    })

    it("validates axis argument", {
      expect_error(ddbs_rotate_3d(argentina_ddbs, axis = "asdfasdf"))
    })

    it("validates required argument combinations", {
      expect_error(ddbs_rotate_3d(argentina_ddbs))
    })
  })
})



# 3. ddbs_shift ------------------------------------------------------

## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
describe("ddbs_shift()", {

  ### EXPECTED BEHAVIOR

  describe("expected behavior", {

    it("works on ddbs input", {
      output_ddbs_1 <- ddbs_shift(argentina_ddbs, 10)
      output_ddbs_2 <- ddbs_shift(argentina_ddbs, 10, 20)
      output_ddbs_3 <- ddbs_shift(argentina_ddbs, dy = 10)
      output_ddbs_4 <- ddbs_shift(argentina_ddbs, 45, quiet = TRUE)

      expect_s3_class(output_ddbs_1, "duckspatial_df")
      expect_s3_class(output_ddbs_2, "duckspatial_df")
      expect_s3_class(output_ddbs_3, "duckspatial_df")
      expect_s3_class(output_ddbs_4, "duckspatial_df")
    })

    it("returns different output formats for ddbs input", {
      output_sf <- ddbs_shift(argentina_ddbs, 45, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on sf input", {
      output_sf_1 <- ddbs_shift(argentina_sf, 10)
      output_sf_2 <- ddbs_shift(argentina_sf, 10, 20)
      output_sf_3 <- ddbs_shift(argentina_sf, dy = 10)
      output_sf_4 <- ddbs_shift(argentina_sf, 45, quiet = TRUE)

      expect_s3_class(output_sf_1, "duckspatial_df")
      expect_s3_class(output_sf_2, "duckspatial_df")
      expect_s3_class(output_sf_3, "duckspatial_df")
      expect_s3_class(output_sf_4, "duckspatial_df")
    })

    it("returns different output formats for sf input", {
      output_sf <- ddbs_shift(argentina_sf, 45, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on DuckDB table input", {
      output_conn_1 <- ddbs_shift("argentina", 10, conn = conn_test)
      output_conn_2 <- ddbs_shift("argentina", 10, 20, conn = conn_test)
      output_conn_3 <- ddbs_shift("argentina", dy = 10, conn = conn_test)
      output_conn_4 <- ddbs_shift("argentina", 45, conn = conn_test, quiet = TRUE)

      expect_s3_class(output_conn_1, "duckspatial_df")
      expect_s3_class(output_conn_2, "duckspatial_df")
      expect_s3_class(output_conn_3, "duckspatial_df")
      expect_s3_class(output_conn_4, "duckspatial_df")
    })

    it("returns different output formats for DuckDB table input", {
      output_sf <- ddbs_shift("argentina", 45, conn = conn_test, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_shift(argentina_ddbs, 50))
      expect_message(ddbs_shift("argentina", 50, conn = conn_test, name = "shift"))
      expect_message(ddbs_shift("argentina", 50, conn = conn_test, name = "shift", overwrite = TRUE))
      expect_true(ddbs_shift("argentina", 50, conn = conn_test, name = "shift2"))

      expect_no_message(ddbs_shift(argentina_ddbs, 50, quiet = TRUE))
      expect_no_message(
        ddbs_shift(
          "argentina",
          50,
          conn = conn_test,
          name = "shift",
          overwrite = TRUE,
          quiet = TRUE
        )
      )
    })
  })

  ### EXPECTED ERRORS

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_shift("argentina", conn = NULL))
    })

    it("validates dx argument type", {
      expect_error(ddbs_shift(argentina_ddbs, dx = "10"))
    })

    it("validates dy argument type", {
      expect_error(ddbs_shift(argentina_ddbs, dy = "banana"))
    })

    it("validates x argument type", {
      expect_error(ddbs_shift(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_shift(argentina_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_shift(argentina_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_shift(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_shift(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_shift(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_shift(argentina_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 4. ddbs_flip ------------------------------------------------------

## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
describe("ddbs_flip()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on ddbs input", {
      output_ddbs_1 <- ddbs_flip(argentina_ddbs)
      output_ddbs_2 <- ddbs_flip(argentina_ddbs, "vertical")
      output_ddbs_3 <- ddbs_flip(nc_ddbs, by_feature = TRUE)
      output_ddbs_4 <- ddbs_flip(nc_ddbs, "vertical", by_feature = TRUE, quiet = TRUE)

      expect_s3_class(output_ddbs_1, "duckspatial_df")
      expect_s3_class(output_ddbs_2, "duckspatial_df")
      expect_s3_class(output_ddbs_3, "duckspatial_df")
      expect_s3_class(output_ddbs_4, "duckspatial_df")
    })

    it("returns different output formats for ddbs input", {
      output_sf <- ddbs_flip(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on sf input", {
      output_sf_1 <- ddbs_flip(argentina_sf)
      output_sf_2 <- ddbs_flip(argentina_sf, "vertical")
      output_sf_3 <- ddbs_flip(nc_ddbs, by_feature = TRUE)
      output_sf_4 <- ddbs_flip(nc_sf, "vertical", by_feature = TRUE, quiet = TRUE)

      expect_s3_class(output_sf_1, "duckspatial_df")
      expect_s3_class(output_sf_2, "duckspatial_df")
      expect_s3_class(output_sf_3, "duckspatial_df")
      expect_s3_class(output_sf_4, "duckspatial_df")
    })

    it("returns different output formats for sf input", {
      output_sf <- ddbs_flip(argentina_sf, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on DuckDB table input", {
      output_conn_1 <- ddbs_flip("nc", conn = conn_test)
      output_conn_2 <- ddbs_flip("nc", "vertical", conn = conn_test)
      output_conn_3 <- ddbs_flip("nc", by_feature = TRUE, conn = conn_test)
      output_conn_4 <- ddbs_flip("nc", "vertical", conn = conn_test, by_feature = TRUE, quiet = TRUE)

      expect_s3_class(output_conn_1, "duckspatial_df")
      expect_s3_class(output_conn_2, "duckspatial_df")
      expect_s3_class(output_conn_3, "duckspatial_df")
      expect_s3_class(output_conn_4, "duckspatial_df")
    })

    it("returns different output formats for DuckDB table input", {
      output_sf <- ddbs_flip("nc", conn = conn_test, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_flip(nc_ddbs))
      expect_message(ddbs_flip("nc", conn = conn_test, name = "flip"))
      expect_message(ddbs_flip("nc", conn = conn_test, name = "flip", overwrite = TRUE))
      expect_true(ddbs_flip("nc", conn = conn_test, name = "flip2"))

      expect_no_message(ddbs_flip(argentina_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_flip(
          "nc",
          conn = conn_test,
          name = "flip",
          overwrite = TRUE,
          quiet = TRUE
        )
      )
    })
  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_flip("argentina", conn = NULL))
    })

    it("validates direction argument", {
      expect_error(ddbs_flip(argentina_ddbs, direction = "uptodown"))
    })

    it("validates by_feature argument type", {
      expect_error(ddbs_flip(argentina_ddbs, by_feature = "TRUE"))
    })

    it("validates x argument type", {
      expect_error(ddbs_flip(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_flip(argentina_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_flip(argentina_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_flip(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_flip(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_flip(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_flip(argentina_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 5. ddbs_scale ------------------------------------------------------

## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
describe("ddbs_scale()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on ddbs input", {
      output_ddbs_1 <- ddbs_scale(argentina_ddbs)
      output_ddbs_2 <- ddbs_scale(argentina_ddbs, y_scale = -1)
      output_ddbs_3 <- ddbs_scale(nc_ddbs, by_feature = TRUE)

      expect_s3_class(output_ddbs_1, "duckspatial_df")
      expect_s3_class(output_ddbs_2, "duckspatial_df")
      expect_s3_class(output_ddbs_3, "duckspatial_df")
    })

    it("returns different output formats for ddbs input", {
      output_sf <- ddbs_scale(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on sf input", {
      output_sf_1 <- ddbs_scale(argentina_sf)
      output_sf_2 <- ddbs_scale(argentina_sf, y_scale = -1)
      output_sf_3 <- ddbs_scale(nc_ddbs, by_feature = TRUE)

      expect_s3_class(output_sf_1, "duckspatial_df")
      expect_s3_class(output_sf_2, "duckspatial_df")
      expect_s3_class(output_sf_3, "duckspatial_df")
    })

    it("returns different output formats for sf input", {
      output_sf <- ddbs_scale(argentina_sf, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on DuckDB table input", {
      output_conn_1 <- ddbs_scale("nc", conn = conn_test)
      output_conn_2 <- ddbs_scale("nc", y_scale = -1, conn = conn_test)
      output_conn_3 <- ddbs_scale("nc", by_feature = TRUE, conn = conn_test)

      expect_s3_class(output_conn_1, "duckspatial_df")
      expect_s3_class(output_conn_2, "duckspatial_df")
      expect_s3_class(output_conn_3, "duckspatial_df")
    })

    it("returns different output formats for DuckDB table input", {
      output_sf <- ddbs_scale("nc", conn = conn_test, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_scale(nc_ddbs))
      expect_message(ddbs_scale("nc", conn = conn_test, name = "shear"))
      expect_message(ddbs_scale("nc", conn = conn_test, name = "shear", overwrite = TRUE))
      expect_true(ddbs_scale("nc", conn = conn_test, name = "shear2"))

      expect_no_message(ddbs_scale(argentina_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_scale(
          "nc",
          conn = conn_test,
          name = "shear",
          overwrite = TRUE,
          quiet = TRUE
        )
      )
    })
  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_scale("argentina", conn = NULL))
    })

    it("validates x_scale argument type", {
      expect_error(ddbs_scale(argentina_ddbs, x_scale = "23"))
    })

    it("validates y_scale argument type", {
      expect_error(ddbs_scale(argentina_ddbs, y_scale = "five"))
    })

    it("validates by_feature argument type", {
      expect_error(ddbs_scale(argentina_ddbs, by_feature = "TRUE"))
    })

    it("validates x argument type", {
      expect_error(ddbs_scale(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_scale(argentina_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_scale(argentina_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_scale(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_scale(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_scale(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_scale(argentina_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 6. ddbs_shear ------------------------------------------------------

## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
describe("ddbs_shear()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on ddbs input", {
      output_ddbs_1 <- ddbs_shear(argentina_ddbs)
      output_ddbs_2 <- ddbs_shear(argentina_ddbs, y_shear = -1)
      output_ddbs_3 <- ddbs_shear(nc_ddbs, by_feature = TRUE)

      expect_s3_class(output_ddbs_1, "duckspatial_df")
      expect_s3_class(output_ddbs_2, "duckspatial_df")
      expect_s3_class(output_ddbs_3, "duckspatial_df")
    })

    it("returns different output formats for ddbs input", {
      output_sf <- ddbs_shear(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on sf input", {
      output_sf_1 <- ddbs_shear(argentina_sf)
      output_sf_2 <- ddbs_shear(argentina_sf, y_shear = -1)
      output_sf_3 <- ddbs_shear(nc_ddbs, by_feature = TRUE)

      expect_s3_class(output_sf_1, "duckspatial_df")
      expect_s3_class(output_sf_2, "duckspatial_df")
      expect_s3_class(output_sf_3, "duckspatial_df")
    })

    it("returns different output formats for sf input", {
      output_sf <- ddbs_shear(argentina_sf, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on DuckDB table input", {
      output_conn_1 <- ddbs_shear("nc", conn = conn_test)
      output_conn_2 <- ddbs_shear("nc", y_shear = -1, conn = conn_test)
      output_conn_3 <- ddbs_shear("nc", by_feature = TRUE, conn = conn_test)

      expect_s3_class(output_conn_1, "duckspatial_df")
      expect_s3_class(output_conn_2, "duckspatial_df")
      expect_s3_class(output_conn_3, "duckspatial_df")
    })

    it("returns different output formats for DuckDB table input", {
      output_sf <- ddbs_shear("nc", conn = conn_test, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_shear(nc_ddbs))
      expect_message(ddbs_shear("nc", conn = conn_test, name = "scale"))
      expect_message(ddbs_shear("nc", conn = conn_test, name = "scale", overwrite = TRUE))
      expect_true(ddbs_shear("nc", conn = conn_test, name = "scale2"))

      expect_no_message(ddbs_shear(argentina_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_shear(
          "nc",
          conn = conn_test,
          name = "scale",
          overwrite = TRUE,
          quiet = TRUE
        )
      )
    })
  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_shear("argentina", conn = NULL))
    })

    it("validates x_scale argument type", {
      expect_error(ddbs_shear(argentina_ddbs, x_scale = "23"))
    })

    it("validates y_shear argument type", {
      expect_error(ddbs_shear(argentina_ddbs, y_shear = "five"))
    })

    it("validates by_feature argument type", {
      expect_error(ddbs_shear(argentina_ddbs, by_feature = "TRUE"))
    })

    it("validates x argument type", {
      expect_error(ddbs_shear(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_shear(argentina_ddbs, conn = 999))
    })

    it("validates new_column argument type", {
      expect_error(ddbs_shear(argentina_ddbs, new_column = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_shear(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_shear(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_shear(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_shear(argentina_ddbs, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



# 7. ddbs_affine -------------------------------------------------------

## - CHECK 1.1: works on ddbs
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, sf)
## - CHECK 1.3: works on sf
## - CHECK 1.4: sf returns different outputs (duckspatial_df, sf)
## - CHECK 1.5: works on duckdb table
## - CHECK 1.6: duckdb table returns different outputs (duckspatial_df, sf)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
## - CHECK 1.9: 2D identity matrix preserves geometry
## - CHECK 1.10: 2D translation matrix shifts geometry correctly
## - CHECK 1.11: 2D scaling matrix scales geometry correctly
## - CHECK 1.12: 3D identity matrix preserves geometry
## - CHECK 2.1: matrix must be a matrix object
## - CHECK 2.2: matrix must be numeric
## - CHECK 2.3: matrix must be 2x3 or 3x4
## - CHECK 2.4: combination of inputs / missing arguments
## - CHECK 2.5: other errors
describe("ddbs_affine()", {

  mat_identity_2d  <- matrix(c(1, 0, 0,
                                0, 1, 0), nrow = 2, byrow = TRUE)
  mat_translate_2d <- matrix(c(1, 0, 1,
                                0, 1, 2), nrow = 2, byrow = TRUE)
  mat_scale_2d     <- matrix(c(2, 0, 0,
                                0, 2, 0), nrow = 2, byrow = TRUE)
  mat_identity_3d  <- matrix(c(1, 0, 0, 0,
                                0, 1, 0, 0,
                                0, 0, 1, 0), nrow = 3, byrow = TRUE)

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on ddbs input", {
      output_ddbs_1 <- ddbs_affine(argentina_ddbs, mat_identity_2d)
      output_ddbs_2 <- ddbs_affine(argentina_ddbs, mat_translate_2d)
      output_ddbs_3 <- ddbs_affine(argentina_ddbs, mat_scale_2d)
      output_ddbs_4 <- ddbs_affine(argentina_ddbs, mat_identity_3d)
      output_ddbs_5 <- ddbs_affine(argentina_ddbs, mat_identity_2d, quiet = TRUE)

      expect_s3_class(output_ddbs_1, "duckspatial_df")
      expect_s3_class(output_ddbs_2, "duckspatial_df")
      expect_s3_class(output_ddbs_3, "duckspatial_df")
      expect_s3_class(output_ddbs_4, "duckspatial_df")
      expect_s3_class(output_ddbs_5, "duckspatial_df")
    })

    it("returns different output formats for ddbs input", {
      output_sf <- ddbs_affine(argentina_ddbs, mat_identity_2d, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on sf input", {
      output_sf_1 <- ddbs_affine(argentina_sf, mat_identity_2d)
      output_sf_2 <- ddbs_affine(argentina_sf, mat_translate_2d)
      output_sf_3 <- ddbs_affine(argentina_sf, mat_scale_2d)
      output_sf_4 <- ddbs_affine(argentina_sf, mat_identity_3d)
      output_sf_5 <- ddbs_affine(argentina_sf, mat_identity_2d, quiet = TRUE)

      expect_s3_class(output_sf_1, "duckspatial_df")
      expect_s3_class(output_sf_2, "duckspatial_df")
      expect_s3_class(output_sf_3, "duckspatial_df")
      expect_s3_class(output_sf_4, "duckspatial_df")
      expect_s3_class(output_sf_5, "duckspatial_df")
    })

    it("returns different output formats for sf input", {
      output_sf <- ddbs_affine(argentina_sf, mat_identity_2d, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on DuckDB table input", {
      output_conn_1 <- ddbs_affine("argentina", mat_identity_2d, conn = conn_test)
      output_conn_2 <- ddbs_affine("argentina", mat_translate_2d, conn = conn_test)
      output_conn_3 <- ddbs_affine("argentina", mat_scale_2d, conn = conn_test)
      output_conn_4 <- ddbs_affine("argentina", mat_identity_3d, conn = conn_test)
      output_conn_5 <- ddbs_affine("argentina", mat_identity_2d, conn = conn_test, quiet = TRUE)

      expect_s3_class(output_conn_1, "duckspatial_df")
      expect_s3_class(output_conn_2, "duckspatial_df")
      expect_s3_class(output_conn_3, "duckspatial_df")
      expect_s3_class(output_conn_4, "duckspatial_df")
      expect_s3_class(output_conn_5, "duckspatial_df")
    })

    it("returns different output formats for DuckDB table input", {
      output_sf <- ddbs_affine("argentina", mat_identity_2d, conn = conn_test, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_affine(argentina_ddbs, mat_identity_2d))
      expect_message(ddbs_affine("argentina", mat_identity_2d, conn = conn_test, name = "affine"))
      expect_message(ddbs_affine("argentina", mat_identity_2d, conn = conn_test, name = "affine", overwrite = TRUE))
      expect_true(ddbs_affine("argentina", mat_identity_2d, conn = conn_test, name = "affine2"))

      expect_no_message(ddbs_affine(argentina_ddbs, mat_identity_2d, quiet = TRUE))
      expect_no_message(
        ddbs_affine(
          "argentina",
          mat_identity_2d,
          conn     = conn_test,
          name     = "affine",
          overwrite = TRUE,
          quiet    = TRUE
        )
      )
    })

    describe("2D matrix transformations", {

      it("identity matrix preserves geometry", {
        original <- ddbs_collect(argentina_ddbs)
        result   <- ddbs_affine(argentina_ddbs, mat_identity_2d) |> ddbs_collect()
        expect_equal(original$geometry, result$geometry)
      })

      it("translation matrix shifts coordinates by xoff and yoff", {
        original <- argentina_sf
        result   <- ddbs_affine(argentina_sf, mat_translate_2d, mode = "sf")

        coords_orig   <- sf::st_coordinates(original)[, c("X", "Y")]
        coords_result <- sf::st_coordinates(result)[, c("X", "Y")]

        expect_equal(coords_result[, "X"], coords_orig[, "X"] + 1, tolerance = 1e-6)
        expect_equal(coords_result[, "Y"], coords_orig[, "Y"] + 2, tolerance = 1e-6)
      })

      it("scaling matrix multiplies coordinates", {
        original <- argentina_sf
        result   <- ddbs_affine(argentina_sf, mat_scale_2d, mode = "sf")

        coords_orig   <- sf::st_coordinates(original)[, c("X", "Y")]
        coords_result <- sf::st_coordinates(result)[, c("X", "Y")]

        expect_equal(coords_result[, "X"], coords_orig[, "X"] * 2, tolerance = 1e-6)
        expect_equal(coords_result[, "Y"], coords_orig[, "Y"] * 2, tolerance = 1e-6)
      })
    })

    describe("3D matrix transformations", {

      it("3D identity matrix preserves geometry", {
        original <- ddbs_collect(argentina_ddbs)
        result   <- ddbs_affine(argentina_ddbs, mat_identity_3d) |> ddbs_collect()
        expect_equal(original$geometry, result$geometry)
      })

      it("3D translation matrix shifts X and Y coordinates", {
        mat_translate_3d <- matrix(c(1, 0, 0, 5,
                                     0, 1, 0, 3,
                                     0, 0, 1, 0), nrow = 3, byrow = TRUE)
        original <- argentina_sf
        result   <- ddbs_affine(argentina_sf, mat_translate_3d, mode = "sf")

        coords_orig   <- sf::st_coordinates(original)[, c("X", "Y")]
        coords_result <- sf::st_coordinates(result)[, c("X", "Y")]

        expect_equal(coords_result[, "X"], coords_orig[, "X"] + 5, tolerance = 1e-6)
        expect_equal(coords_result[, "Y"], coords_orig[, "Y"] + 3, tolerance = 1e-6)
      })
    })
  })

  ### EXPECTED ERRORS ---------------------------------------------------

  describe("errors", {

    describe("matrix argument validation", {

      it("rejects non-matrix input", {
        expect_error(ddbs_affine(argentina_ddbs, matrix = c(1, 0, 0, 0, 1, 0)))
        expect_error(ddbs_affine(argentina_ddbs, matrix = list(1, 0, 0, 0, 1, 0)))
        expect_error(ddbs_affine(argentina_ddbs, matrix = "1, 0, 0, 0, 1, 0"))
      })

      it("rejects non-numeric matrix", {
        expect_error(
          ddbs_affine(argentina_ddbs, matrix = matrix(c("a", "b", "c", "d", "e", "f"), nrow = 2))
        )
      })

      it("rejects wrong matrix dimensions", {
        expect_error(ddbs_affine(argentina_ddbs, matrix = matrix(1:4, nrow = 2)))
        expect_error(ddbs_affine(argentina_ddbs, matrix = matrix(1:6, nrow = 3)))
        expect_error(ddbs_affine(argentina_ddbs, matrix = matrix(1:9, nrow = 3)))
        expect_error(ddbs_affine(argentina_ddbs, matrix = matrix(1:12, nrow = 4)))
      })
    })

    it("requires connection when using table names", {
      expect_error(ddbs_affine("argentina", mat_identity_2d, conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_affine(x = 999, matrix = mat_identity_2d))
    })

    it("validates conn argument type", {
      expect_error(ddbs_affine(argentina_ddbs, mat_identity_2d, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_affine(argentina_ddbs, mat_identity_2d, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_affine(argentina_ddbs, mat_identity_2d, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_affine(x = "999", mat_identity_2d, conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_affine(argentina_ddbs, mat_identity_2d, conn = conn_test, name = c("banana", "banana")))
    })
  })
})



## stop connection
duckspatial::ddbs_stop_conn(conn_test)


# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## known-vertex polygon: 4 corners + ring closure = 5 points; ngeometries = 1
simple_sf <- sf::st_sf(
  id = 1L,
  geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)))
  ),
  crs = 4326
)
simple_ddbs <- as_duckspatial_df(simple_sf)

## two-part MULTIPOLYGON: ngeometries = 2
two_poly_sf <- sf::st_sf(
  id = 1L,
  geometry = sf::st_sfc(sf::st_multipolygon(list(
    list(matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)),
    list(matrix(c(2,2, 3,2, 3,3, 2,3, 2,2), ncol = 2, byrow = TRUE))
  ))),
  crs = 4326
)
two_poly_ddbs <- as_duckspatial_df(two_poly_sf)

## point (dimension 0) and linestring (dimension 1) for ddbs_dimension tests
point_sf <- sf::st_sf(
  id = 1L,
  geometry = sf::st_sfc(sf::st_point(c(0, 0))),
  crs = 4326
)
point_ddbs <- as_duckspatial_df(point_sf)

line_sf <- sf::st_sf(
  id = 1L,
  geometry = sf::st_sfc(sf::st_linestring(matrix(c(0,0, 1,1), ncol = 2, byrow = TRUE))),
  crs = 4326
)
line_ddbs <- as_duckspatial_df(line_sf)

## write data in the database
duckspatial::ddbs_write_table(conn_test, simple_sf,   "simple")
duckspatial::ddbs_write_table(conn_test, two_poly_sf, "two_poly")
duckspatial::ddbs_write_table(conn_test, point_sf,    "point_dim")
duckspatial::ddbs_write_table(conn_test, line_sf,     "line_dim")


# 1. ddbs_get_npoints ----------------------------------------------------

## expected behaviour for inherits(x, "sf")
## - CHECK 1.1: returns duckspatial_df by default
## - CHECK 1.2: returns correct output formats
## - CHECK 1.3: sf is written into the database
## - CHECK 1.4: messages shown/suppressed correctly
## - CHECK 1.5: counts vertices correctly (5 for a closed rectangle ring)
## - CHECK 1.6: materializes data correctly
## - CHECK 1.7: vector and column outputs are identical
## expected behaviour for inherits(x, "duckspatial_df")
## - CHECK 2.1: returns duckspatial_df by default
## - CHECK 2.2: returns numeric with mode sf
## - CHECK 2.3: duckspatial_df is written into the database
## - CHECK 2.4: messages shown/suppressed correctly
## - CHECK 2.5: warns when creating table from a different connection
## - CHECK 2.6: materializes data correctly
## - CHECK 2.7: vector and column outputs are identical
## expected behaviour for inherits(x, "character")
## - CHECK 3.1: returns duckspatial_df by default
## - CHECK 3.2: returns numeric with mode sf
## - CHECK 3.3: table is written into the database
## - CHECK 3.4: messages shown/suppressed correctly
## - CHECK 3.5: counts vertices correctly
## - CHECK 3.6: conn = NULL errors
## - CHECK 3.7: materializes data correctly
## - CHECK 3.8: vector and column outputs are identical
## Check that errors work
## - CHECK 4.1: if overwrite = FALSE, it won't delete an existing table
## - CHECK 4.2: incorrect x type
## - CHECK 4.3: conn required when x is a table name
describe("ddbs_get_npoints()", {

  ### EXPECTED BEHAVIOUR - SF INPUT

  describe("expected behavior on sf input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_get_npoints(simple_sf)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_get_npoints(simple_sf, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_ddbs <- ddbs_get_npoints(simple_sf, mode = NULL)
      output_sf   <- ddbs_get_npoints(simple_sf, mode = "sf")

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_true(is.numeric(output_sf) || is.integer(output_sf))
    })

    it("writes tables to the database", {
      output <- ddbs_get_npoints(simple_sf, conn = conn_test, name = "np_sf_tbl", new_column = "np")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_get_npoints(simple_sf))
      expect_message(ddbs_get_npoints(simple_sf, conn = conn_test, name = "np_sf_tbl2", new_column = "np"))

      expect_no_message(ddbs_get_npoints(simple_sf, quiet = TRUE))
      expect_no_message(ddbs_get_npoints(simple_sf, conn = conn_test, name = "np_sf_tbl3", new_column = "np", quiet = TRUE))
    })

    it("counts vertices correctly (closed rectangle ring: 5 points)", {
      result <- ddbs_get_npoints(simple_sf, mode = "sf")
      expect_equal(as.integer(result), 5L)
    })

    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_get_npoints(simple_sf)

      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_get_npoints(simple_sf, mode = "sf"))
      output_column <- ddbs_get_npoints(simple_sf) |> ddbs_collect()

      expect_equal(output_vec, output_column$npoints)
    })
  })

  ### EXPECTED BEHAVIOUR - DUCKSPATIAL_DF INPUT

  describe("expected behavior on duckspatial_df input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_get_npoints(simple_ddbs)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_get_npoints(simple_ddbs, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("writes tables to the database", {
      output <- ddbs_get_npoints(simple_ddbs, conn = conn_test, name = "np_ddbs_tbl", new_column = "np")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_get_npoints(simple_ddbs))
      expect_message(ddbs_get_npoints(simple_ddbs, conn = conn_test, name = "np_ddbs_tbl2", new_column = "np"))

      expect_no_message(ddbs_get_npoints(simple_ddbs, quiet = TRUE))
      expect_no_message(ddbs_get_npoints(simple_ddbs, conn = conn_test, name = "np_ddbs_tbl3", new_column = "np", quiet = TRUE))
    })

    it("warns when creating table from different connections", {
      expect_warning(ddbs_get_npoints(simple_ddbs, conn = conn_test, name = "np_ddbs_tbl4", new_column = "np"))
    })

    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_get_npoints(simple_ddbs)

      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_get_npoints(simple_ddbs, mode = "sf"))
      output_column <- ddbs_get_npoints(simple_ddbs) |> ddbs_collect()

      expect_equal(output_vec, output_column$npoints)
    })
  })

  ### EXPECTED BEHAVIOUR - DUCKDB TABLE INPUT

  describe("expected behavior on DuckDB table input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_get_npoints("simple", conn = conn_test)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_get_npoints("simple", conn = conn_test, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("writes tables to the database", {
      output <- ddbs_get_npoints("simple", conn = conn_test, name = "np_tbl_tbl", new_column = "np")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_get_npoints("simple", conn = conn_test))
      expect_message(ddbs_get_npoints("simple", conn = conn_test, name = "np_tbl_tbl2", new_column = "np"))

      expect_no_message(ddbs_get_npoints("simple", conn = conn_test, quiet = TRUE))
      expect_no_message(ddbs_get_npoints("simple", conn = conn_test, name = "np_tbl_tbl3", new_column = "np", quiet = TRUE))
    })

    it("counts vertices correctly (closed rectangle ring: 5 points)", {
      result <- ddbs_get_npoints("simple", conn = conn_test, mode = "sf")
      expect_equal(as.integer(result), 5L)
    })

    it("requires conn when using table names", {
      expect_error(ddbs_get_npoints("simple", conn = NULL))
    })

    it("materializes data correctly (collect, ddbs_collect)", {
      output_with_column <- ddbs_get_npoints("simple", conn = conn_test)

      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_collect, output_ddbs)
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_get_npoints("simple", conn = conn_test, mode = "sf"))
      output_column <- ddbs_get_npoints("simple", conn = conn_test) |> ddbs_collect()

      expect_equal(output_vec, output_column$npoints)
    })
  })

  ### EXPECTED ERRORS

  describe("errors", {

    it("errors if overwrite = FALSE and table already exists", {
      ddbs_get_npoints(simple_sf, conn = conn_test, name = "dup_np_tbl", new_column = "np")
      expect_error(
        ddbs_get_npoints(simple_sf, conn = conn_test, name = "dup_np_tbl", new_column = "np")
      )
    })

    it("errors on invalid x type", {
      expect_error(ddbs_get_npoints(999))
      expect_error(ddbs_get_npoints(TRUE))
    })

    it("requires conn when x is a table name", {
      expect_error(ddbs_get_npoints("simple"))
    })
  })
})


# 2. ddbs_get_ngeometries ------------------------------------------------

## expected behaviour for inherits(x, "sf")
## - CHECK 1.1: returns duckspatial_df by default
## - CHECK 1.2: returns correct output formats
## - CHECK 1.3: sf is written into the database
## - CHECK 1.4: messages shown/suppressed correctly
## - CHECK 1.5: returns 1 for a simple polygon
## - CHECK 1.6: returns 2 for a two-part MULTIPOLYGON
## - CHECK 1.7: materializes data correctly
## - CHECK 1.8: vector and column outputs are identical
## expected behaviour for inherits(x, "duckspatial_df")
## - CHECK 2.1: returns duckspatial_df by default
## - CHECK 2.2: returns numeric with mode sf
## - CHECK 2.3: duckspatial_df is written into the database
## - CHECK 2.4: messages shown/suppressed correctly
## - CHECK 2.5: warns when creating table from a different connection
## - CHECK 2.6: materializes data correctly
## - CHECK 2.7: vector and column outputs are identical
## expected behaviour for inherits(x, "character")
## - CHECK 3.1: returns duckspatial_df by default
## - CHECK 3.2: returns numeric with mode sf
## - CHECK 3.3: table is written into the database
## - CHECK 3.4: messages shown/suppressed correctly
## - CHECK 3.5: returns 2 for a two-part MULTIPOLYGON
## - CHECK 3.6: conn = NULL errors
## - CHECK 3.7: materializes data correctly
## - CHECK 3.8: vector and column outputs are identical
## Check that errors work
## - CHECK 4.1: if overwrite = FALSE, it won't delete an existing table
## - CHECK 4.2: incorrect x type
## - CHECK 4.3: conn required when x is a table name
describe("ddbs_get_ngeometries()", {

  ### EXPECTED BEHAVIOUR - SF INPUT

  describe("expected behavior on sf input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_get_ngeometries(simple_sf)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_get_ngeometries(simple_sf, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_ddbs <- ddbs_get_ngeometries(simple_sf, mode = NULL)
      output_sf   <- ddbs_get_ngeometries(simple_sf, mode = "sf")

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_true(is.numeric(output_sf) || is.integer(output_sf))
    })

    it("writes tables to the database", {
      output <- ddbs_get_ngeometries(simple_sf, conn = conn_test, name = "ng_sf_tbl", new_column = "ng")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_get_ngeometries(simple_sf))
      expect_message(ddbs_get_ngeometries(simple_sf, conn = conn_test, name = "ng_sf_tbl2", new_column = "ng"))

      expect_no_message(ddbs_get_ngeometries(simple_sf, quiet = TRUE))
      expect_no_message(ddbs_get_ngeometries(simple_sf, conn = conn_test, name = "ng_sf_tbl3", new_column = "ng", quiet = TRUE))
    })

    it("returns 1 for a simple polygon", {
      result <- ddbs_get_ngeometries(simple_sf, mode = "sf")
      expect_equal(as.integer(result), 1L)
    })

    it("returns 2 for a two-part MULTIPOLYGON", {
      result <- ddbs_get_ngeometries(two_poly_sf, mode = "sf")
      expect_equal(as.integer(result), 2L)
    })

    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_get_ngeometries(simple_sf)

      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_get_ngeometries(two_poly_sf, mode = "sf"))
      output_column <- ddbs_get_ngeometries(two_poly_sf) |> ddbs_collect()

      expect_equal(output_vec, output_column$ngeometries)
    })
  })

  ### EXPECTED BEHAVIOUR - DUCKSPATIAL_DF INPUT

  describe("expected behavior on duckspatial_df input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_get_ngeometries(simple_ddbs)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_get_ngeometries(simple_ddbs, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("writes tables to the database", {
      output <- ddbs_get_ngeometries(simple_ddbs, conn = conn_test, name = "ng_ddbs_tbl", new_column = "ng")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_get_ngeometries(simple_ddbs))
      expect_message(ddbs_get_ngeometries(simple_ddbs, conn = conn_test, name = "ng_ddbs_tbl2", new_column = "ng"))

      expect_no_message(ddbs_get_ngeometries(simple_ddbs, quiet = TRUE))
      expect_no_message(ddbs_get_ngeometries(simple_ddbs, conn = conn_test, name = "ng_ddbs_tbl3", new_column = "ng", quiet = TRUE))
    })

    it("warns when creating table from different connections", {
      expect_warning(ddbs_get_ngeometries(simple_ddbs, conn = conn_test, name = "ng_ddbs_tbl4", new_column = "ng"))
    })

    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_get_ngeometries(simple_ddbs)

      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_get_ngeometries(two_poly_ddbs, mode = "sf"))
      output_column <- ddbs_get_ngeometries(two_poly_ddbs) |> ddbs_collect()

      expect_equal(output_vec, output_column$ngeometries)
    })
  })

  ### EXPECTED BEHAVIOUR - DUCKDB TABLE INPUT

  describe("expected behavior on DuckDB table input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_get_ngeometries("two_poly", conn = conn_test)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_get_ngeometries("two_poly", conn = conn_test, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("writes tables to the database", {
      output <- ddbs_get_ngeometries("two_poly", conn = conn_test, name = "ng_tbl_tbl", new_column = "ng")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_get_ngeometries("two_poly", conn = conn_test))
      expect_message(ddbs_get_ngeometries("two_poly", conn = conn_test, name = "ng_tbl_tbl2", new_column = "ng"))

      expect_no_message(ddbs_get_ngeometries("two_poly", conn = conn_test, quiet = TRUE))
      expect_no_message(ddbs_get_ngeometries("two_poly", conn = conn_test, name = "ng_tbl_tbl3", new_column = "ng", quiet = TRUE))
    })

    it("returns 2 for a two-part MULTIPOLYGON", {
      result <- ddbs_get_ngeometries("two_poly", conn = conn_test, mode = "sf")
      expect_equal(as.integer(result), 2L)
    })

    it("requires conn when using table names", {
      expect_error(ddbs_get_ngeometries("two_poly", conn = NULL))
    })

    it("materializes data correctly (collect, ddbs_collect)", {
      output_with_column <- ddbs_get_ngeometries("two_poly", conn = conn_test)

      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_collect, output_ddbs)
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_get_ngeometries("two_poly", conn = conn_test, mode = "sf"))
      output_column <- ddbs_get_ngeometries("two_poly", conn = conn_test) |> ddbs_collect()

      expect_equal(output_vec, output_column$ngeometries)
    })
  })

  ### EXPECTED ERRORS

  describe("errors", {

    it("errors if overwrite = FALSE and table already exists", {
      ddbs_get_ngeometries(simple_sf, conn = conn_test, name = "dup_ng_tbl", new_column = "ng")
      expect_error(
        ddbs_get_ngeometries(simple_sf, conn = conn_test, name = "dup_ng_tbl", new_column = "ng")
      )
    })

    it("errors on invalid x type", {
      expect_error(ddbs_get_ngeometries(999))
      expect_error(ddbs_get_ngeometries(TRUE))
    })

    it("requires conn when x is a table name", {
      expect_error(ddbs_get_ngeometries("simple"))
    })
  })
})


# 3. ddbs_dimension ------------------------------------------------------

## expected behaviour for inherits(x, "sf")
## - CHECK 1.1: returns duckspatial_df by default
## - CHECK 1.2: returns correct output formats
## - CHECK 1.3: sf is written into the database
## - CHECK 1.4: messages shown/suppressed correctly
## - CHECK 1.5: returns 2 for a polygon
## - CHECK 1.6: returns 1 for a linestring
## - CHECK 1.7: returns 0 for a point
## - CHECK 1.8: materializes data correctly
## - CHECK 1.9: vector and column outputs are identical
## expected behaviour for inherits(x, "duckspatial_df")
## - CHECK 2.1: returns duckspatial_df by default
## - CHECK 2.2: returns numeric with mode sf
## - CHECK 2.3: duckspatial_df is written into the database
## - CHECK 2.4: messages shown/suppressed correctly
## - CHECK 2.5: warns when creating table from a different connection
## - CHECK 2.6: materializes data correctly
## - CHECK 2.7: vector and column outputs are identical
## expected behaviour for inherits(x, "character")
## - CHECK 3.1: returns duckspatial_df by default
## - CHECK 3.2: returns numeric with mode sf
## - CHECK 3.3: table is written into the database
## - CHECK 3.4: messages shown/suppressed correctly
## - CHECK 3.5: returns 2 for a polygon
## - CHECK 3.6: conn = NULL errors
## - CHECK 3.7: materializes data correctly
## - CHECK 3.8: vector and column outputs are identical
## Check that errors work
## - CHECK 4.1: if overwrite = FALSE, it won't delete an existing table
## - CHECK 4.2: incorrect x type
## - CHECK 4.3: conn required when x is a table name
describe("ddbs_dimension()", {

  ### EXPECTED BEHAVIOUR - SF INPUT

  describe("expected behavior on sf input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_dimension(simple_sf)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_dimension(simple_sf, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_ddbs <- ddbs_dimension(simple_sf, mode = NULL)
      output_sf   <- ddbs_dimension(simple_sf, mode = "sf")

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_true(is.numeric(output_sf) || is.integer(output_sf))
    })

    it("writes tables to the database", {
      output <- ddbs_dimension(simple_sf, conn = conn_test, name = "dim_sf_tbl", new_column = "dim")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_dimension(simple_sf))
      expect_message(ddbs_dimension(simple_sf, conn = conn_test, name = "dim_sf_tbl2", new_column = "dim"))

      expect_no_message(ddbs_dimension(simple_sf, quiet = TRUE))
      expect_no_message(ddbs_dimension(simple_sf, conn = conn_test, name = "dim_sf_tbl3", new_column = "dim", quiet = TRUE))
    })

    it("returns 2 for a polygon", {
      result <- ddbs_dimension(simple_sf, mode = "sf")
      expect_equal(as.integer(result), 2L)
    })

    it("returns 1 for a linestring", {
      result <- ddbs_dimension(line_sf, mode = "sf")
      expect_equal(as.integer(result), 1L)
    })

    it("returns 0 for a point", {
      result <- ddbs_dimension(point_sf, mode = "sf")
      expect_equal(as.integer(result), 0L)
    })

    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_dimension(simple_sf)

      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_dimension(simple_sf, mode = "sf"))
      output_column <- ddbs_dimension(simple_sf) |> ddbs_collect()

      expect_equal(output_vec, output_column$dimension)
    })
  })

  ### EXPECTED BEHAVIOUR - DUCKSPATIAL_DF INPUT

  describe("expected behavior on duckspatial_df input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_dimension(simple_ddbs)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_dimension(simple_ddbs, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("writes tables to the database", {
      output <- ddbs_dimension(simple_ddbs, conn = conn_test, name = "dim_ddbs_tbl", new_column = "dim")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_dimension(simple_ddbs))
      expect_message(ddbs_dimension(simple_ddbs, conn = conn_test, name = "dim_ddbs_tbl2", new_column = "dim"))

      expect_no_message(ddbs_dimension(simple_ddbs, quiet = TRUE))
      expect_no_message(ddbs_dimension(simple_ddbs, conn = conn_test, name = "dim_ddbs_tbl3", new_column = "dim", quiet = TRUE))
    })

    it("warns when creating table from different connections", {
      expect_warning(ddbs_dimension(simple_ddbs, conn = conn_test, name = "dim_ddbs_tbl4", new_column = "dim"))
    })

    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_dimension(simple_ddbs)

      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_dimension(simple_ddbs, mode = "sf"))
      output_column <- ddbs_dimension(simple_ddbs) |> ddbs_collect()

      expect_equal(output_vec, output_column$dimension)
    })
  })

  ### EXPECTED BEHAVIOUR - DUCKDB TABLE INPUT

  describe("expected behavior on DuckDB table input", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_dimension("simple", conn = conn_test)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns a numeric vector with mode sf", {
      output <- ddbs_dimension("simple", conn = conn_test, mode = "sf")
      expect_true(is.numeric(output) || is.integer(output))
    })

    it("writes tables to the database", {
      output <- ddbs_dimension("simple", conn = conn_test, name = "dim_tbl_tbl", new_column = "dim")
      expect_true(output)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_dimension("simple", conn = conn_test))
      expect_message(ddbs_dimension("simple", conn = conn_test, name = "dim_tbl_tbl2", new_column = "dim"))

      expect_no_message(ddbs_dimension("simple", conn = conn_test, quiet = TRUE))
      expect_no_message(ddbs_dimension("simple", conn = conn_test, name = "dim_tbl_tbl3", new_column = "dim", quiet = TRUE))
    })

    it("returns 2 for a polygon table", {
      result <- ddbs_dimension("simple", conn = conn_test, mode = "sf")
      expect_equal(as.integer(result), 2L)
    })

    it("requires conn when using table names", {
      expect_error(ddbs_dimension("simple", conn = NULL))
    })

    it("materializes data correctly (collect, ddbs_collect)", {
      output_with_column <- ddbs_dimension("simple", conn = conn_test)

      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()

      expect_identical(output_collect, output_ddbs)
    })

    it("produces identical results for vector and column outputs", {
      output_vec    <- as.integer(ddbs_dimension("simple", conn = conn_test, mode = "sf"))
      output_column <- ddbs_dimension("simple", conn = conn_test) |> ddbs_collect()

      expect_equal(output_vec, output_column$dimension)
    })
  })

  ### EXPECTED ERRORS

  describe("errors", {

    it("errors if overwrite = FALSE and table already exists", {
      ddbs_dimension(simple_sf, conn = conn_test, name = "dup_dim_tbl", new_column = "dim")
      expect_error(
        ddbs_dimension(simple_sf, conn = conn_test, name = "dup_dim_tbl", new_column = "dim")
      )
    })

    it("errors on invalid x type", {
      expect_error(ddbs_dimension(999))
      expect_error(ddbs_dimension(TRUE))
    })

    it("requires conn when x is a table name", {
      expect_error(ddbs_dimension("simple"))
    })
  })
})


## stop connection
ddbs_stop_conn(conn_test)

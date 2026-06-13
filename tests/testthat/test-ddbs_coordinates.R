

# 0. Set up ------------------------------------------------------------------

skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

conn_test <- duckspatial::ddbs_create_conn()
ddbs_write_table(conn_test, argentina_sf, "argentina")
ddbs_write_table(conn_test, points_sf,   "points")


# 1. ddbs_xmax() -------------------------------------------------------------

## - CHECK 1.1: works on all input types with by_feature = TRUE
## - CHECK 1.2: returns numeric vector with mode = "sf"
## - CHECK 1.3: returns single scalar with by_feature = FALSE
## - CHECK 1.4: messages shown/suppressed correctly
## - CHECK 1.5: writes table to database
## - CHECK 1.6: by_feature = FALSE equals max of by_feature = TRUE vector
## - CHECK 1.7: xmax >= xmin (sanity)
## - CHECK 2.1: standard errors
describe("ddbs_xmax()", {

  describe("expected behavior", {

    it("works on all input types with by_feature = TRUE", {
      out_ddbs <- ddbs_xmax(argentina_ddbs)
      out_sf   <- ddbs_xmax(argentina_sf)
      out_conn <- ddbs_xmax("argentina", conn = conn_test)

      expect_s3_class(out_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(out_ddbs), ddbs_collect(out_sf))
      expect_equal(ddbs_collect(out_ddbs), ddbs_collect(out_conn))
    })

    it("returns a numeric vector with mode = 'sf'", {
      out <- ddbs_xmax(argentina_ddbs, mode = "sf")
      expect_true(is.numeric(out))
    })

    it("returns a single numeric scalar with by_feature = FALSE", {
      out_ddbs <- ddbs_xmax(argentina_ddbs, by_feature = FALSE)
      out_sf   <- ddbs_xmax(argentina_sf,   by_feature = FALSE)
      out_conn <- ddbs_xmax("argentina", conn = conn_test, by_feature = FALSE)

      expect_length(out_ddbs, 1)
      expect_true(is.numeric(out_ddbs))
      expect_equal(out_ddbs, out_sf)
      expect_equal(out_ddbs, out_conn)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_xmax(argentina_ddbs))
      expect_message(ddbs_xmax("argentina", conn = conn_test, name = "xmax_tbl"))
      expect_no_message(ddbs_xmax(argentina_ddbs, quiet = TRUE))
      expect_no_message(
        ddbs_xmax("argentina", conn = conn_test, name = "xmax_tbl_q", quiet = TRUE)
      )
    })

    it("writes table to database", {
      out <- ddbs_xmax("argentina", conn = conn_test, name = "xmax_tbl2", new_column = "x_hi")
      expect_true(out)
    })

    it("by_feature = FALSE equals max of by_feature = TRUE vector", {
      global_max   <- ddbs_xmax(argentina_ddbs, by_feature = FALSE)
      per_feat_max <- ddbs_xmax(argentina_ddbs, mode = "sf")
      expect_equal(global_max, max(per_feat_max))
    })

    it("xmax >= xmin for the same dataset", {
      expect_gte(
        ddbs_xmax(argentina_ddbs, by_feature = FALSE),
        ddbs_xmin(argentina_ddbs, by_feature = FALSE)
      )
    })
  })

  describe("errors", {

    it("errors if overwrite = FALSE and table already exists", {
      ddbs_xmax("argentina", conn = conn_test, name = "dup_xmax")
      expect_error(ddbs_xmax("argentina", conn = conn_test, name = "dup_xmax"))
    })

    it("requires conn when x is a table name", {
      expect_error(ddbs_xmax("argentina", conn = NULL))
    })

    it("errors on invalid x type", {
      expect_error(ddbs_xmax(999))
      expect_error(ddbs_xmax(TRUE))
    })
  })
})


# 2. ddbs_xmin() -------------------------------------------------------------

## - CHECK 1.1: works on all input types
## - CHECK 1.2: returns scalar with by_feature = FALSE
## - CHECK 1.3: by_feature = FALSE equals min of by_feature = TRUE
describe("ddbs_xmin()", {

  describe("expected behavior", {

    it("works on all input types with by_feature = TRUE", {
      out_ddbs <- ddbs_xmin(argentina_ddbs)
      out_sf   <- ddbs_xmin(argentina_sf)
      out_conn <- ddbs_xmin("argentina", conn = conn_test)

      expect_s3_class(out_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(out_ddbs), ddbs_collect(out_sf))
      expect_equal(ddbs_collect(out_ddbs), ddbs_collect(out_conn))
    })

    it("returns a single numeric scalar with by_feature = FALSE", {
      out <- ddbs_xmin(argentina_ddbs, by_feature = FALSE)
      expect_length(out, 1)
      expect_true(is.numeric(out))
    })

    it("by_feature = FALSE equals min of by_feature = TRUE vector", {
      global_min   <- ddbs_xmin(argentina_ddbs, by_feature = FALSE)
      per_feat_min <- ddbs_xmin(argentina_ddbs, mode = "sf")
      expect_equal(global_min, min(per_feat_min))
    })
  })
})


# 3. ddbs_ymax() / ddbs_ymin() -----------------------------------------------

## - CHECK 1.1: return duckspatial_df by default
## - CHECK 1.2: return scalar with by_feature = FALSE, ymax >= ymin
## - CHECK 1.3: by_feature = FALSE equals max/min of by_feature = TRUE
describe("ddbs_ymax() and ddbs_ymin()", {

  it("return duckspatial_df by default", {
    expect_s3_class(ddbs_ymax(argentina_ddbs), "duckspatial_df")
    expect_s3_class(ddbs_ymin(argentina_ddbs), "duckspatial_df")
  })

  it("return a single numeric scalar with by_feature = FALSE, ymax >= ymin", {
    out_max <- ddbs_ymax(argentina_ddbs, by_feature = FALSE)
    out_min <- ddbs_ymin(argentina_ddbs, by_feature = FALSE)
    expect_length(out_max, 1)
    expect_length(out_min, 1)
    expect_true(is.numeric(out_max))
    expect_gte(out_max, out_min)
  })

  it("by_feature = FALSE equals max/min of by_feature = TRUE vector", {
    expect_equal(
      ddbs_ymax(argentina_ddbs, by_feature = FALSE),
      max(ddbs_ymax(argentina_ddbs, mode = "sf"))
    )
    expect_equal(
      ddbs_ymin(argentina_ddbs, by_feature = FALSE),
      min(ddbs_ymin(argentina_ddbs, mode = "sf"))
    )
  })
})


# 4. ddbs_zmax() / ddbs_zmin() / ddbs_mmax() / ddbs_mmin() ------------------

## - CHECK 1.1: all return duckspatial_df by default
## - CHECK 1.2: all return a single numeric scalar with by_feature = FALSE
describe("ddbs_zmax(), ddbs_zmin(), ddbs_mmax(), ddbs_mmin()", {

  it("all return duckspatial_df by default", {
    expect_s3_class(ddbs_zmax(argentina_ddbs), "duckspatial_df")
    expect_s3_class(ddbs_zmin(argentina_ddbs), "duckspatial_df")
    expect_s3_class(ddbs_mmax(argentina_ddbs), "duckspatial_df")
    expect_s3_class(ddbs_mmin(argentina_ddbs), "duckspatial_df")
  })

  it("all return a single numeric scalar with by_feature = FALSE", {
    expect_length(ddbs_zmax(argentina_ddbs, by_feature = FALSE), 1)
    expect_length(ddbs_zmin(argentina_ddbs, by_feature = FALSE), 1)
    expect_length(ddbs_mmax(argentina_ddbs, by_feature = FALSE), 1)
    expect_length(ddbs_mmin(argentina_ddbs, by_feature = FALSE), 1)
    expect_true(is.numeric(ddbs_zmax(argentina_ddbs, by_feature = FALSE)))
    expect_true(is.numeric(ddbs_mmin(argentina_ddbs, by_feature = FALSE)))
  })
})

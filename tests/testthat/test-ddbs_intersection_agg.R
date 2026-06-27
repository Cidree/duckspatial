
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## overlapping polygons in two groups:
## - group "a": 3 squares (0-3, 1-4, 2-5) -> common area is the (2,2)-(3,3) square
## - group "b": 2 squares (0-2, 1-3)      -> common area is the (1,1)-(2,2) square
sq <- function(xmin, ymin, side) {
  sf::st_polygon(list(matrix(
    c(xmin, ymin, xmin + side, ymin, xmin + side, ymin + side, xmin, ymin + side, xmin, ymin),
    ncol = 2, byrow = TRUE
  )))
}
inter_sf <- sf::st_sf(
  grp = c("a", "a", "a", "b", "b"),
  geometry = sf::st_sfc(sq(0, 0, 3), sq(1, 1, 3), sq(2, 2, 3), sq(0, 0, 2), sq(1, 1, 2), crs = 4326)
)
inter_ddbs <- as_duckspatial_df(inter_sf)
duckspatial::ddbs_write_table(conn_test, inter_sf, "inter")


# 1. ddbs_intersection_agg() ---------------------------------------------

## - CHECK 1.1: works on all input formats and they agree
## - CHECK 1.2: ddbs / sf output modes
## - CHECK 1.3: no `by` -> single common geometry (here empty: no area common to all 5)
## - CHECK 1.4: `by` -> one row per group with the correct common area
## - CHECK 1.5: messages and table writing
## - CHECK 2.x: errors
describe("ddbs_intersection_agg()", {

  describe("expected behavior", {

    it("works on all input formats and they agree", {
      out_ddbs <- ddbs_intersection_agg(inter_ddbs, by = "grp")
      out_sf   <- ddbs_intersection_agg(inter_sf, by = "grp")
      out_conn <- ddbs_intersection_agg("inter", by = "grp", conn = conn_test)

      expect_s3_class(out_ddbs, "duckspatial_df")
      expect_equal(
        ddbs_collect(out_ddbs) |> dplyr::arrange(grp),
        ddbs_collect(out_sf)   |> dplyr::arrange(grp)
      )
      expect_equal(
        ddbs_collect(out_ddbs) |> dplyr::arrange(grp),
        ddbs_collect(out_conn) |> dplyr::arrange(grp)
      )
    })

    it("returns an sf object with mode = 'sf'", {
      expect_s3_class(ddbs_intersection_agg(inter_ddbs, by = "grp", mode = "sf"), "sf")
    })

    it("intersects within each group correctly", {
      out <- ddbs_intersection_agg(inter_ddbs, by = "grp", mode = "sf") |>
        dplyr::arrange(grp)

      expect_equal(nrow(out), 2L)
      ## both groups' common area is a 1x1 square; drop CRS so st_area is planar
      planar <- sf::st_set_crs(out, NA)
      expect_equal(as.numeric(sf::st_area(planar)), c(1, 1))
    })

    it("aggregates all geometries into one when `by` is NULL", {
      ## three fully overlapping squares -> common (2,2)-(3,3) 1x1 square
      overlap_ddbs <- as_duckspatial_df(sf::st_sf(
        geometry = sf::st_sfc(sq(0, 0, 3), sq(1, 1, 3), sq(2, 2, 3), crs = 4326)
      ))
      out <- ddbs_intersection_agg(overlap_ddbs, mode = "sf")
      expect_equal(nrow(out), 1L)
      planar <- sf::st_set_crs(out, NA)
      expect_equal(as.numeric(sf::st_area(planar)), 1)
    })

    it("preserves the CRS", {
      out <- ddbs_intersection_agg(inter_ddbs, by = "grp", mode = "sf")
      expect_equal(sf::st_crs(out), sf::st_crs(4326))
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_intersection_agg(inter_ddbs, by = "grp"))
      expect_message(ddbs_intersection_agg("inter", by = "grp", conn = conn_test, name = "iagg"))
      expect_no_message(
        ddbs_intersection_agg("inter", by = "grp", conn = conn_test, name = "iagg", overwrite = TRUE, quiet = TRUE)
      )
    })

    it("writes a table when name is provided", {
      expect_true(ddbs_intersection_agg("inter", by = "grp", conn = conn_test, name = "iagg2"))
      expect_true(DBI::dbExistsTable(conn_test, "iagg2"))
    })
  })

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_intersection_agg("inter", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_intersection_agg(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_intersection_agg(inter_ddbs, conn = 999))
    })

    it("validates `by` argument type", {
      expect_error(ddbs_intersection_agg(inter_ddbs, by = 1))
    })

    it("validates table name exists", {
      expect_error(ddbs_intersection_agg(x = "999", conn = conn_test))
    })
  })
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)

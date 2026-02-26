
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## create data
cities_tbl <- data.frame(
  city = c("Buenos Aires", "Córdoba", "Rosario"),
  lon = c(-58.3816, -64.1811, -60.6393),
  lat = c(-34.6037, -31.4201, -32.9468),
  population = c(3075000, 1391000, 1193605)
)

## write data
DBI::dbWriteTable(conn_test, "cities", cities_tbl)


# 1. ddbs_as_spatial() -------------------------------------------------------

## - CHECK 1.1: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.2: messages work
## - CHECK 1.3: writting a table works
## - CHECK 1.4: different column names, and CRS
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_as_spatial()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("returns different output formats", {
      output_ddbs_fmt     <- ddbs_as_spatial(cities_tbl)
      output_geoarrow_fmt <- ddbs_as_spatial(cities_tbl, output = "geoarrow")
      output_sf_fmt       <- ddbs_as_spatial(cities_tbl, output = "sf")
      output_raw_fmt      <- ddbs_as_spatial(cities_tbl, output = "raw")

      expect_s3_class(output_ddbs_fmt, "duckspatial_df")
      expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf_fmt, "sf")
      expect_s3_class(output_raw_fmt, "tbl_df")
    })

    it("shows and suppresses messages correctly", {
      expect_message(ddbs_as_spatial(cities_tbl))
      expect_message(ddbs_as_spatial("cities", conn = conn_test, name = "as_spatial"))
      expect_message(ddbs_as_spatial("cities", conn = conn_test, name = "as_spatial", overwrite = TRUE))
      expect_true(ddbs_as_spatial("cities", conn = conn_test, name = "as_spatial2"))

      expect_no_message(ddbs_as_spatial(cities_tbl, quiet = TRUE))
      expect_no_message(ddbs_as_spatial("cities", conn = conn_test, name = "as_spatial", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables correctly to DuckDB", {
      output_tbl <- ddbs_read_vector(conn_test, "as_spatial")
      expect_equal(
        ddbs_collect(ddbs_as_spatial(cities_tbl))$geometry,
        output_tbl$geometry
      )
    })

    it("handles different column names and CRS correctly", {
      output_tbl <- ddbs_read_vector(conn_test, "as_spatial")
      cities_3857 <- output_tbl |> 
        sf::st_transform("EPSG:3847") %>% 
        dplyr::mutate(
          xx = sf::st_coordinates(.)[, 1],
          yy = sf::st_coordinates(.)[, 2]
        ) |> 
        sf::st_drop_geometry()

      output_3857 <- ddbs_as_spatial(
        cities_3857,
        coords = c("xx", "yy"),
        crs = "EPSG:3857"
      )

      expect_equal(st_crs(output_3857), st_crs("EPSG:3857"))
    })
  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("validates coords argument", {
      expect_error(ddbs_as_spatial(cities_tbl, coords = c("longitude", "latitude")))
      expect_error(ddbs_as_spatial(cities_tbl, coords = c("longitude", "latitude", "z")))
    })

    it("validates CRS argument", {
      expect_error(ddbs_as_spatial(cities_tbl, crs = "NICE_CRS"))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_as_spatial("cities", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_as_spatial(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_as_spatial(cities_tbl, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_as_spatial(cities_tbl, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_as_spatial(cities_tbl, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_as_spatial(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_as_spatial(cities_tbl, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})



## stop connection
duckspatial::ddbs_stop_conn(conn_test)

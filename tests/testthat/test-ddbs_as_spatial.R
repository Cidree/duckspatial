
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


# 1. ddbs_as_points() -------------------------------------------------------

## - CHECK 1.1: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.2: messages work
## - CHECK 1.3: writting a table works
## - CHECK 1.4: different column names, and CRS
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_as_points()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("returns different output formats", {
      output_ddbs_fmt <- ddbs_as_points(cities_tbl)
      output_sf_fmt <- ddbs_as_points(cities_tbl, mode = "sf")

      expect_s3_class(output_ddbs_fmt, "duckspatial_df")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_as_points(cities_tbl))
      expect_message(ddbs_as_points("cities", conn = conn_test, name = "as_spatial"))
      expect_message(ddbs_as_points("cities", conn = conn_test, name = "as_spatial", overwrite = TRUE))
      expect_true(ddbs_as_points("cities", conn = conn_test, name = "as_spatial2"))

      expect_no_message(ddbs_as_points(cities_tbl, quiet = TRUE))
      expect_no_message(ddbs_as_points("cities", conn = conn_test, name = "as_spatial", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables correctly to DuckDB", {
      output_tbl <- ddbs_read_table(conn_test, "as_spatial")
      expect_equal(
        ddbs_collect(ddbs_as_points(cities_tbl))$geometry,
        output_tbl$geometry
      )
    })

    it("handles different column names and CRS correctly", {
      output_tbl <- ddbs_read_table(conn_test, "as_spatial")
      cities_3857 <- output_tbl |> 
        sf::st_transform("EPSG:3847") %>% 
        dplyr::mutate(
          xx = sf::st_coordinates(.)[, 1],
          yy = sf::st_coordinates(.)[, 2]
        ) |> 
        sf::st_drop_geometry()

      output_3857 <- ddbs_as_points(
        cities_3857,
        coords = c("xx", "yy"),
        crs = "EPSG:3857"
      )

      expect_equal(sf::st_crs(output_3857), sf::st_crs("EPSG:3857"))
    })

    it("supports remove and na.fail arguments", {
      # remove = TRUE (default)
      out_remove <- ddbs_as_points(cities_tbl, coords = c("lon", "lat"), remove = TRUE)
      expect_false(any(c("lon", "lat") %in% colnames(out_remove)))
      
      # remove = FALSE
      out_keep <- ddbs_as_points(cities_tbl, coords = c("lon", "lat"), remove = FALSE)
      expect_true(all(c("lon", "lat") %in% colnames(out_keep)))
      
      # na.fail = TRUE (default)
      cities_na <- cities_tbl
      cities_na$lon[1] <- NA
      expect_error(ddbs_as_points(cities_na), "Missing values found")
      
      # na.fail = FALSE
      out_na <- ddbs_as_points(cities_na, na.fail = FALSE)
      expect_s3_class(out_na, "duckspatial_df")
    })
  })

  ### ERRORS ------------------------------------------------------------

  describe("errors", {

    it("validates coords argument", {
      expect_error(ddbs_as_points(cities_tbl, coords = c("longitude", "latitude")))
      expect_error(ddbs_as_points(cities_tbl, coords = c("longitude", "latitude", "z")))
    })

    it("validates CRS argument", {
      expect_error(ddbs_as_points(cities_tbl, crs = "NICE_CRS"))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_as_points("cities", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_as_points(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_as_points(cities_tbl, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_as_points(cities_tbl, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_as_points(cities_tbl, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_as_points(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_as_points(cities_tbl, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})



# 2. ddbs_point() -----------------------------------------------------------

## - CHECK 1.1: 2D, 3D, and 4D output classes
## - CHECK 1.2: returns sf when mode = "sf"
## - CHECK 1.3: CRS is assigned correctly
## - CHECK 1.4: extra columns appear in output
## - CHECK 1.5: geom_col renames the geometry column
## - CHECK 1.6: messages shown/suppressed correctly
## - CHECK 1.7: writes table to database
## - CHECK 2.1: standard errors (x/y type, length mismatch, m without z,
##              named ..., conflicting names, length mismatch in ...,
##              name without conn)
describe("ddbs_point()", {

  x_vec <- c(-58.38, -64.18, -60.64)
  y_vec <- c(-34.60, -31.42, -32.95)
  z_vec <- c(100, 200, 300)
  m_vec <- c(1.0, 2.0, 3.0)

  describe("expected behavior", {

    it("returns duckspatial_df by default for 2D, 3D, and 4D", {
      expect_s3_class(ddbs_point(x_vec, y_vec),                                        "duckspatial_df")
      expect_s3_class(ddbs_point(x_vec, y_vec, z = z_vec),                             "duckspatial_df")
      expect_s3_class(ddbs_point(x_vec, y_vec, z = z_vec, m = m_vec),                  "duckspatial_df")
    })

    it("returns sf when mode = 'sf'", {
      out <- ddbs_point(x_vec, y_vec, crs = 4326, mode = "sf")
      expect_s3_class(out, "sf")
    })

    it("assigns CRS correctly", {
      out <- ddbs_point(x_vec, y_vec, crs = 4326, mode = "sf")
      expect_equal(sf::st_crs(out), sf::st_crs(4326))
    })

    it("includes extra columns in output", {
      out <- ddbs_point(x_vec, y_vec, id = 1:3, label = c("a", "b", "c"), mode = "sf")
      expect_true("id"    %in% colnames(out))
      expect_true("label" %in% colnames(out))
    })

    it("renames geometry column when geom_col is set", {
      out <- ddbs_point(x_vec, y_vec, geom_col = "geom", mode = "sf")
      expect_true("geom" %in% colnames(out))
      expect_false("geometry" %in% colnames(out))
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_point(x_vec, y_vec))
      expect_message(
        ddbs_point(x_vec, y_vec, conn = conn_test, name = "pts_tbl")
      )
      expect_no_message(
        ddbs_point(x_vec, y_vec, conn = conn_test, name = "pts_tbl_q",
                   quiet = TRUE)
      )
    })

    it("writes table to database and returns TRUE invisibly", {
      out <- ddbs_point(x_vec, y_vec, crs = 4326, conn = conn_test,
                        name = "pts_tbl2")
      expect_true(out)
      expect_true(DBI::dbExistsTable(conn_test, "pts_tbl2"))
    })

    it("coordinates round-trip correctly", {
      out <- ddbs_point(x_vec, y_vec, crs = 4326, mode = "sf")
      coords <- sf::st_coordinates(out)
      expect_equal(coords[, "X"], x_vec, tolerance = 1e-6)
      expect_equal(coords[, "Y"], y_vec, tolerance = 1e-6)
    })
  })

  describe("errors", {

    it("errors if x is not numeric", {
      expect_error(ddbs_point("a", y_vec))
    })

    it("errors if y is not numeric", {
      expect_error(ddbs_point(x_vec, "b"))
    })

    it("errors if x and y have different lengths", {
      expect_error(ddbs_point(x_vec, y_vec[-1]))
    })

    it("errors if m is provided without z", {
      expect_error(ddbs_point(x_vec, y_vec, m = m_vec))
    })

    it("errors if z is not numeric", {
      expect_error(ddbs_point(x_vec, y_vec, z = c("a", "b", "c")))
    })

    it("errors if z has wrong length", {
      expect_error(ddbs_point(x_vec, y_vec, z = z_vec[-1]))
    })

    it("errors if ... contains unnamed arguments", {
      expect_error(ddbs_point(x_vec, y_vec, z_vec, m_vec, 1:3))
    })

    it("errors if ... names conflict with reserved column names", {
      expect_error(ddbs_point(x_vec, y_vec, geometry = 1:3))
    })

    it("errors if ... columns have wrong length", {
      expect_error(ddbs_point(x_vec, y_vec, id = 1:2))
    })

    it("errors if name is provided without conn", {
      expect_error(ddbs_point(x_vec, y_vec, name = "pts_err"))
    })

    it("errors if overwrite is not logical", {
      expect_error(ddbs_point(x_vec, y_vec, overwrite = 1))
    })

    it("errors if quiet is not logical", {
      expect_error(ddbs_point(x_vec, y_vec, quiet = 1))
    })
  })
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)


# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## create a random variable for quad keys
rand_sf <- sf::st_sample(argentina_sf, 100) |> sf::st_as_sf()
rand_sf["var"] <- runif(100)
rand_ddbs <- as_duckspatial_df(rand_sf)

## write data
duckspatial::ddbs_write_vector(conn_test, rand_sf, "points")


# 1. ddbs_quadkey() --------------------------------------------------------

## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all input formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, tbl, raster)
## - CHECK 1.3: messages work
## - CHECK 1.4: "var" argument works
## - CHECK 1.5: "fun" argument works
## - CHECK 1.6: "background" argument works
describe("ddbs_quadkey()", {
  
  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_quadkey(rand_ddbs, level = 1)
      output_sf   <- ddbs_quadkey(rand_sf, level = 1)
      output_conn <- ddbs_quadkey("points", conn = conn_test, level = 1)
      
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, tilexy, raster)", {
      output_ddbs   <- ddbs_quadkey(rand_ddbs, level = 5)
      output_tilexy <- ddbs_quadkey(rand_ddbs, level = 5, output = "tilexy")
      output_raster <- ddbs_quadkey(rand_ddbs, level = 5, output = "raster")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_tilexy, "tbl_df")
      expect_s4_class(output_raster, "SpatRaster")
      expect_true(all(terra::values(output_raster, mat = FALSE, na.rm = TRUE) == 1))
    })
    
    it("writes tables to the database", {
      output <- ddbs_quadkey("points", conn = conn_test, name = "quadkey2")
      expect_true(output)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_quadkey(rand_ddbs, level = 1))
      expect_message(ddbs_quadkey("points", conn = conn_test, name = "quadkey"))
      expect_message(ddbs_quadkey("points", conn = conn_test, name = "quadkey", overwrite = TRUE))
      
      expect_no_message(ddbs_quadkey(rand_ddbs, quiet = TRUE))
      expect_no_message(ddbs_quadkey("points", conn = conn_test, name = "quadkey", overwrite = TRUE, quiet = TRUE))
    })
    
    it("field argument aggregates variable values correctly", {
      output_raster <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", output = "raster")
      
      expect_false(all(terra::values(output_raster, mat = FALSE, na.rm = TRUE) == 1))
    })
    
    it("fun argument controls aggregation function (min)", {
      output_raster_min <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", fun = "min", output = "raster")
      output_raster_max <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", fun = "max", output = "raster")
      
      expect_gt(terra::minmax(output_raster_max)[2], terra::minmax(output_raster_min)[2])
    })
    
    it("fun argument supports mean aggregation", {
      output_raster_mean <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", fun = "mean", output = "raster")
      
      expect_s4_class(output_raster_mean, "SpatRaster")
      expect_false(all(terra::values(output_raster_mean, mat = FALSE, na.rm = TRUE) == 1))
    })
    
    it("fun argument supports sum aggregation", {
      output_raster_sum <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", fun = "sum", output = "raster")
      
      expect_s4_class(output_raster_sum, "SpatRaster")
      expect_false(all(terra::values(output_raster_sum, mat = FALSE, na.rm = TRUE) == 1))
    })
    
    it("background argument fills empty cells with specified value", {
      output_raster_bg <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", background = 0, output = "raster")
      
      expect_equal(terra::minmax(output_raster_bg)[1], 0)
      expect_false(NA %in% terra::values(output_raster_bg, mat = FALSE))
    })
    
    it("background argument supports different values", {
      output_raster_bg_neg <- ddbs_quadkey(rand_ddbs, level = 5, field = "var", background = -999, output = "raster")
      
      expect_equal(terra::minmax(output_raster_bg_neg)[1], -999)
    })
    
    it("level argument controls quadkey zoom level", {
      output_level_1 <- ddbs_quadkey(rand_ddbs, level = 1, field = "var")
      output_level_5 <- ddbs_quadkey(rand_ddbs, level = 5, field = "var")
      
      expect_lt(nrow(ddbs_collect(output_level_1)), nrow(ddbs_collect(output_level_5)))
    })
  })
  
  ### EXPECTED ERRORS
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_quadkey(x = "999", conn = NULL))
    })
    
    it("only works with point geometries", {
      expect_error(ddbs_quadkey(argentina_ddbs))
      expect_error(ddbs_quadkey(rivers_ddbs))
    })
    
    it("validates level argument range (1-23)", {
      expect_error(ddbs_quadkey(points_ddbs, level = 0))
      expect_error(ddbs_quadkey(points_ddbs, level = 100))
    })
    
    it("validates level argument type", {
      expect_error(ddbs_quadkey(points_ddbs, level = "10"))
      expect_error(ddbs_quadkey(points_ddbs, level = FALSE))
    })
    
    it("validates field argument type", {
      expect_error(ddbs_quadkey(points_ddbs, field = 2))
      expect_error(ddbs_quadkey(points_ddbs, field = TRUE))
    })
    
    it("validates fun argument type", {
      expect_error(ddbs_quadkey(points_ddbs, fun = TRUE, output = "raster"))
      expect_error(ddbs_quadkey(points_ddbs, fun = 27, output = "raster"))
    })
    
    it("validates output argument value", {
      expect_error(ddbs_quadkey(points_ddbs, fun = "mean", output = "banana"))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_quadkey(x = 999))
      expect_error(ddbs_quadkey(x = "999", conn = conn_test))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_quadkey(points_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_quadkey(points_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_quadkey(points_ddbs, quiet = 999))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_quadkey(points_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})

## stop connection
ddbs_stop_conn(conn_test)
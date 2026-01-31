
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## transform nc
nc_4326_sf <- ddbs_transform(nc_sf, "EPSG:4326")
nc_ddbs <- as_duckspatial_df(nc_4326_sf)

## write some data
duckspatial::ddbs_write_vector(conn_test, countries_sf, "countries")
duckspatial::ddbs_write_vector(conn_test, nc_ddbs, "nc")
duckspatial::ddbs_write_vector(conn_test, nc_ddbs, "rivers")
duckspatial::ddbs_write_vector(conn_test, points_ddbs, "points")


# 1. ddbs_area -----------------------------------------------------------

## expected behaviour for inherits(x, "sf")
## - CHECK 1.1: returns a vector by default
## - CHECK 1.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: sf is written into the database
## - CHECK 1.4: message is shown with quiet = FALSE
## - CHECK 1.5: no message is shown with quiet = TRUE
## - CHECK 1.6: area is calculated properly
## - CHECK 1.7: materialize data, same output
## - CHECK 1.8: area calculated as vector, and added as column must be the same
## expected behaviour for inherits(x, "duckspatial_df")
## - CHECK 2.1: returns a vector by default
## - CHECK 2.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 2.3: duckspatial is written into the database
## - CHECK 2.4: message is shown with quiet = FALSE
## - CHECK 2.5: no message is shown with quiet = TRUE
## - CHECK 2.6: area is calculated properly
## - CHECK 2.7: when creating a new duckdb table, it shows warning if they come from 
##   different connections
## - CHECK 2.8 - materialize data, same output
## - CHECK 2.9 - area calculated as vector, and added as column must be the same
## expected behaviour for inherits(x, "character")
## - CHECK 3.1: returns a vector by default
## - CHECK 3.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 3.3: duckspatial is written into the database
## - CHECK 3.4: message is shown with quiet = FALSE
## - CHECK 3.5: no message is shown with quiet = TRUE
## - CHECK 3.6: area is calculated properly
## - CHECK 3.7: materialize data, same output
## - CHECK 3.8: area calculated as vector, and added as column must be the same
## - CHECK 3.9: error if conn = NULL, and x = duckdb table
## Check that errors work
## - CHECK 4.1: if name is specified, new_column cannot be NULL
## - CHECK 4.2: if overwrite = FALSE, it won't delete an existing table
## - CHECK 4.3: incorrect inputs
describe("ddbs_area()", {
  
  ### EXPECTED BEHAVIOUR - SF INPUT
  
  describe("expected behavior on sf input", {
    
    it("returns a units vector by default", {
      output <- ddbs_area(nc_4326_sf)
      expect_s3_class(output, "units")
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_area(nc_4326_sf, new_column = "area_calc", output = NULL)
      output_geoarrow <- ddbs_area(nc_4326_sf, new_column = "area_calc", output = "geoarrow")
      output_sf       <- ddbs_area(nc_4326_sf, new_column = "area_calc", output = "sf")
      output_raw      <- ddbs_area(nc_4326_sf, new_column = "area_calc", output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("writes tables to the database", {
      output <- ddbs_area(nc_4326_sf, conn = conn_test, name = "area_tbl", new_column = "area_calc")
      expect_true(output)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_area(nc_4326_sf, new_column = "area_calc"))
      expect_message(ddbs_area(nc_4326_sf, conn = conn_test, name = "area_tbl2", new_column = "area_calc"))
      
      expect_no_message(ddbs_area(nc_4326_sf, new_column = "area_calc", quiet = TRUE))
      # expect_no_message(ddbs_area(nc_4326_sf, conn = conn_test, name = "area_tbl3", new_column = "area_calc", quiet = TRUE))
    })
    
    it("calculates area correctly on projected CRS", {
      argentina_3857_sf <- sf::st_transform(argentina_sf, "EPSG:3857")
      area_ddbs <- ddbs_area(argentina_3857_sf)
      area_sf   <- sf::st_area(argentina_3857_sf)
      
      expect_equal(area_ddbs, area_sf, tolerance = 0.001)
    })

    it("calculates area correctly on geographic CRS", {
      area_ddbs <- ddbs_area(argentina_sf)
      area_sf   <- sf::st_area(argentina_sf)
      
      expect_equal(area_ddbs, area_sf, tolerance = 0.001)
    })
    
    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_area(nc_4326_sf, new_column = "area_calc", output = NULL)
      
      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()
      
      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })
    
    it("produces identical results for vector and column outputs", {
      output_vector <- ddbs_area(nc_4326_sf) |> as.numeric()
      output_column <- ddbs_area(nc_4326_sf, new_column = "area_calc", output = NULL) |> st_as_sf()
      
      expect_identical(output_vector, output_column$area_calc)
    })
  })
  
  ### EXPECTED BEHAVIOUR - DUCKSPATIAL_DF INPUT
  
  describe("expected behavior on duckspatial_df input", {
    
    it("returns a units vector by default", {
      output <- ddbs_area(nc_ddbs)
      expect_s3_class(output, "units")
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_area(nc_ddbs, new_column = "area_calc", output = NULL)
      output_geoarrow <- ddbs_area(nc_ddbs, new_column = "area_calc", output = "geoarrow")
      output_sf       <- ddbs_area(nc_ddbs, new_column = "area_calc", output = "sf")
      output_raw      <- ddbs_area(nc_ddbs, new_column = "area_calc", output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("writes tables to the database", {
      output <- ddbs_area(nc_ddbs, conn = conn_test, name = "ddbs_area_tbl", new_column = "area_calc") |> 
        suppressWarnings()
      expect_true(output)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_area(nc_ddbs, new_column = "area_calc"))
      expect_message(ddbs_area(nc_ddbs, conn = conn_test, name = "ddbs_area_tbl2", new_column = "area_calc") |> suppressWarnings())
      expect_message(ddbs_area(nc_ddbs, conn = conn_test, name = "ddbs_area_tbl3", new_column = "area_calc", quiet = TRUE) |> suppressWarnings())
      
      expect_no_message(ddbs_area(nc_ddbs, new_column = "area_calc", quiet = TRUE))
    })

    it("calculates area correctly on geographic CRS", {
      area_ddbs <- ddbs_area(argentina_sf)
      area_sf   <- sf::st_area(argentina_sf)
      
      expect_equal(area_ddbs, area_sf, tolerance = 0.001)
    })
    
    it("calculates area correctly on projected CRS", {
      argentina_3857_sf <- sf::st_transform(argentina_sf, "EPSG:3857")
      area_ddbs <- ddbs_area(argentina_3857_sf)
      area_sf   <- sf::st_area(argentina_3857_sf)
      
      expect_equal(area_ddbs, area_sf, tolerance = 0.001)
    })
    
    it("warns when creating table from different connections", {
      expect_warning(ddbs_area(nc_ddbs, conn = conn_test, name = "ddbs_area_tbl4", new_column = "area_calc"))
    })
    
    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_area(nc_ddbs, new_column = "area_calc", output = NULL)
      
      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()
      
      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })
    
    it("produces identical results for vector and column outputs", {
      output_vector <- ddbs_area(nc_ddbs) |> as.numeric()
      output_column <- ddbs_area(nc_ddbs, new_column = "area_calc", output = NULL) |> st_as_sf()
      
      expect_identical(output_vector, output_column$area_calc)
    })
  })
  
  ### EXPECTED BEHAVIOUR - DUCKDB TABLE INPUT
  
  describe("expected behavior on DuckDB table input", {
    
    it("returns a units vector by default", {
      output <- ddbs_area("nc", conn = conn_test)
      expect_s3_class(output, "units")
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_area("nc", conn = conn_test, new_column = "area_calc", output = NULL)
      output_geoarrow <- ddbs_area("nc", conn = conn_test, new_column = "area_calc", output = "geoarrow")
      output_sf       <- ddbs_area("nc", conn = conn_test, new_column = "area_calc", output = "sf")
      output_raw      <- ddbs_area("nc", conn = conn_test, new_column = "area_calc", output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("writes tables to the database", {
      output <- ddbs_area("nc", conn = conn_test, name = "conn_area_tbl", new_column = "area_calc")
      expect_true(output)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_area("nc", conn = conn_test, new_column = "area_calc"))
      expect_message(ddbs_area("nc", conn = conn_test, name = "conn_area_tbl2", new_column = "area_calc"))
      
      expect_no_message(ddbs_area("nc", conn = conn_test, new_column = "area_calc", quiet = TRUE))
      expect_no_message(ddbs_area("nc", conn = conn_test, name = "conn_area_tbl3", new_column = "area_calc", quiet = TRUE))
    })

    it("calculates area correctly on geographic CRS", {
      duckspatial::ddbs_write_vector(conn_test, argentina_sf, "argentina", overwrite = TRUE)
      area_ddbs <- ddbs_area("argentina", conn_test)
      area_sf   <- sf::st_area(argentina_sf)
      
      expect_equal(area_ddbs, area_sf, tolerance = 0.001)
    })
    
    it("calculates area correctly on projected CRS", {
      argentina_3857_sf <- sf::st_transform(argentina_sf, "EPSG:3857")
      duckspatial::ddbs_write_vector(conn_test, argentina_3857_sf, "argentina", overwrite = TRUE)
      area_ddbs <- ddbs_area("argentina", conn_test)
      area_sf   <- sf::st_area(argentina_3857_sf)
      
      expect_equal(area_ddbs, area_sf, tolerance = 0.001)
    })
    
    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_area("nc", conn = conn_test, new_column = "area_calc", output = NULL)
      
      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()
      
      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })
    
    it("produces identical results for vector and column outputs", {
      output_vector <- ddbs_area("nc", conn = conn_test) |> as.numeric()
      output_column <- ddbs_area("nc", conn = conn_test, new_column = "area_calc", output = NULL) |> st_as_sf()
      
      expect_identical(output_vector, output_column$area_calc)
    })
  })
  
  ### EXPECTED ERRORS
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_area(x = "nc", conn = NULL))
    })
    
    it("requires new_column when name is specified", {
      expect_error(ddbs_area(nc_4326_sf, name = "new_tbl"))
    })
    
    it("prevents overwriting existing tables without overwrite = TRUE", {
      expect_error(ddbs_area(nc_4326_sf, conn = conn_test, name = "countries", new_column = "area_calc"))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_area(x = 999))
      expect_error(ddbs_area(x = "999", conn = conn_test))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_area(nc_4326_sf, conn = 999))
    })
    
    it("validates new_column argument type", {
      expect_error(ddbs_area(nc_4326_sf, new_column = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_area(nc_4326_sf, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_area(nc_4326_sf, quiet = 999))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_area(nc_4326_sf, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})


# 2. ddbs_length ---------------------------------------------------------

## expected behaviour for inherits(x, "sf")
## - CHECK 1.1: returns a vector by default
## - CHECK 1.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: sf is written into the database
## - CHECK 1.4: message is shown with quiet = FALSE
## - CHECK 1.5: no message is shown with quiet = TRUE
## - CHECK 1.6: length is calculated properly
## - CHECK 1.7: materialize data, same output
## - CHECK 1.8: length calculated as vector, and added as column must be the same
## - CHECK 1.9: length on polygons or points is equal to 0
## expected behaviour for inherits(x, "duckspatial_df")
## - CHECK 2.1: returns a vector by default
## - CHECK 2.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 2.3: duckspatial is written into the database
## - CHECK 2.4: message is shown with quiet = FALSE
## - CHECK 2.5: no message is shown with quiet = TRUE
## - CHECK 2.6: length is calculated properly
## - CHECK 2.7: when creating a new duckdb table, it shows warning if they come from 
##   different connections
## - CHECK 2.8 - materialize data, same output
## - CHECK 2.9 - length calculated as vector, and added as column must be the same
## expected behaviour for inherits(x, "character")
## - CHECK 3.1: returns a vector by default
## - CHECK 3.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 3.3: duckspatial is written into the database
## - CHECK 3.4: message is shown with quiet = FALSE
## - CHECK 3.5: no message is shown with quiet = TRUE
## - CHECK 3.6: length is calculated properly
## - CHECK 3.7: materialize data, same output
## - CHECK 3.8: length calculated as vector, and added as column must be the same
## - CHECK 3.9: error if conn = NULL, and x = duckdb table
## Check that errors work
## - CHECK 4.1: if name is specified, new_column cannot be NULL
## - CHECK 4.2: if overwrite = FALSE, it won't delete an existing table
## - CHECK 4.3: incorrect inputs
describe("ddbs_length()", {
  
  ### EXPECTED BEHAVIOUR - SF INPUT
  
  describe("expected behavior on sf input", {
    
    it("returns a units vector by default", {
      output <- ddbs_length(rivers_sf)
      expect_s3_class(output, "units")
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_length(rivers_sf, new_column = "length_calc", output = NULL)
      output_geoarrow <- ddbs_length(rivers_sf, new_column = "length_calc", output = "geoarrow")
      output_sf       <- ddbs_length(rivers_sf, new_column = "length_calc", output = "sf")
      output_raw      <- ddbs_length(rivers_sf, new_column = "length_calc", output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("writes tables to the database", {
      output <- ddbs_length(rivers_sf, conn = conn_test, name = "length_tbl", new_column = "length_calc")
      expect_true(output)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_length(rivers_sf, new_column = "length_calc"))
      expect_message(ddbs_length(rivers_sf, conn = conn_test, name = "length_tbl2", new_column = "length_calc"))
      
      expect_no_message(ddbs_length(rivers_sf, new_column = "length_calc", quiet = TRUE))
      expect_no_message(ddbs_length(rivers_sf, conn = conn_test, name = "length_tbl3", new_column = "length_calc", quiet = TRUE))
    })

    it("calculates length correctly on geographic CRS", {
      length_ddbs <- ddbs_length(rivers_sf)
      length_sf   <- sf::st_length(rivers_sf)
      
      expect_equal(length_ddbs, length_sf)
    })
    
    it("calculates length correctly on projected CRS", {
      rivers_3857_sf <- sf::st_transform(rivers_sf, "EPSG:3857")
      length_ddbs <- ddbs_length(rivers_3857_sf)
      length_sf   <- sf::st_length(rivers_3857_sf)
      
      expect_equal(length_ddbs, length_sf)
    })
    
    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_length(rivers_sf, new_column = "length_calc", output = NULL)
      
      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()
      
      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })
    
    it("produces identical results for vector and column outputs", {
      output_vector <- ddbs_length(rivers_sf) |> as.numeric()
      output_column <- ddbs_length(rivers_sf, new_column = "length_calc", output = NULL) |> st_as_sf()
      
      expect_identical(output_vector, output_column$length_calc)
    })
    
    it("returns 0 for polygons and points", {
      output_polygons <- ddbs_length(countries_sf) |> as.numeric()
      output_points   <- ddbs_length(points_sf) |> as.numeric()
      
      expect_true(all(output_polygons == 0))
      expect_true(all(output_points == 0))
    })
  })
  
  ### EXPECTED BEHAVIOUR - DUCKSPATIAL_DF INPUT
  
  describe("expected behavior on duckspatial_df input", {
    
    it("returns a units vector by default", {
      output <- ddbs_length(rivers_ddbs)
      expect_s3_class(output, "units")
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_length(rivers_ddbs, new_column = "length_calc", output = NULL)
      output_geoarrow <- ddbs_length(rivers_ddbs, new_column = "length_calc", output = "geoarrow")
      output_sf       <- ddbs_length(rivers_ddbs, new_column = "length_calc", output = "sf")
      output_raw      <- ddbs_length(rivers_ddbs, new_column = "length_calc", output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geom, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("writes tables to the database", {
      output <- ddbs_length(rivers_ddbs, conn = conn_test, name = "ddbs_length_tbl", new_column = "length_calc") |> 
        suppressWarnings()
      expect_true(output)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_length(rivers_ddbs, new_column = "length_calc"))
      expect_message(ddbs_length(rivers_ddbs, conn = conn_test, name = "ddbs_length_tbl2", new_column = "length_calc") |> suppressWarnings())
      expect_message(ddbs_length(rivers_ddbs, conn = conn_test, name = "ddbs_length_tbl3", new_column = "length_calc", quiet = TRUE) |> suppressWarnings())
      
      expect_no_message(ddbs_length(rivers_ddbs, new_column = "length_calc", quiet = TRUE))
    })

    it("calculates length correctly on geographic CRS", {
      length_ddbs <- ddbs_length(rivers_sf)
      length_sf   <- sf::st_length(rivers_sf)
      
      expect_equal(length_ddbs, length_sf)
    })
    
    it("calculates length correctly on projected CRS", {
      rivers_3857_sf <- sf::st_transform(rivers_sf, "EPSG:3857")
      length_ddbs <- ddbs_length(rivers_3857_sf)
      length_sf   <- sf::st_length(rivers_3857_sf)
      
      expect_equal(length_ddbs, length_sf)
    })
    
    it("warns when creating table from different connections", {
      expect_warning(ddbs_length(rivers_ddbs, conn = conn_test, name = "ddbs_length_tbl4", new_column = "length_calc"))
    })
    
    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_length(rivers_ddbs, new_column = "length_calc", output = NULL)
      
      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()
      
      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })
    
    it("produces identical results for vector and column outputs", {
      output_vector <- ddbs_length(rivers_ddbs) |> as.numeric()
      output_column <- ddbs_length(rivers_ddbs, new_column = "length_calc", output = NULL) |> st_as_sf()
      
      expect_identical(output_vector, output_column$length_calc)
    })
  })
  
  ### EXPECTED BEHAVIOUR - DUCKDB TABLE INPUT
  
  describe("expected behavior on DuckDB table input", {
    
    it("returns a units vector by default", {
      output <- ddbs_length("rivers", conn = conn_test)
      expect_s3_class(output, "units")
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_length("rivers", conn = conn_test, new_column = "length_calc", output = NULL)
      output_geoarrow <- ddbs_length("rivers", conn = conn_test, new_column = "length_calc", output = "geoarrow")
      output_sf       <- ddbs_length("rivers", conn = conn_test, new_column = "length_calc", output = "sf")
      output_raw      <- ddbs_length("rivers", conn = conn_test, new_column = "length_calc", output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("writes tables to the database", {
      output <- ddbs_length("rivers", conn = conn_test, name = "conn_length_tbl", new_column = "length_calc")
      expect_true(output)
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_length("rivers", conn = conn_test, new_column = "length_calc"))
      expect_message(ddbs_length("rivers", conn = conn_test, name = "conn_length_tbl2", new_column = "length_calc"))
      
      expect_no_message(ddbs_length("rivers", conn = conn_test, new_column = "length_calc", quiet = TRUE))
      expect_no_message(ddbs_length("rivers", conn = conn_test, name = "conn_length_tbl3", new_column = "length_calc", quiet = TRUE))
    })

    it("calculates length correctly on geographic CRS", {
      duckspatial::ddbs_write_vector(conn_test, rivers_sf, "rivers_4326")
      length_ddbs <- ddbs_length("rivers_4326", conn_test)
      length_sf   <- sf::st_length(rivers_sf)
      
      expect_equal(length_ddbs, length_sf)
    })
    
    it("calculates length correctly on projected CRS", {
      rivers_3857_sf <- sf::st_transform(rivers_sf, "EPSG:3857")
      duckspatial::ddbs_write_vector(conn_test, rivers_3857_sf, "rivers_3857")
      length_ddbs <- ddbs_length("rivers_3857", conn_test)
      length_sf   <- sf::st_length(rivers_3857_sf)
      
      expect_equal(length_ddbs, length_sf)
    })
    
    it("materializes data correctly (st_as_sf, collect, ddbs_collect)", {
      output_with_column <- ddbs_length("rivers", conn = conn_test, new_column = "length_calc", output = NULL)
      
      output_sf      <- output_with_column |> st_as_sf()
      output_collect <- output_with_column |> collect()
      output_ddbs    <- output_with_column |> ddbs_collect()
      
      expect_identical(output_sf, output_collect)
      expect_identical(output_collect, output_ddbs)
      expect_s3_class(output_sf, "sf")
    })
    
    it("produces identical results for vector and column outputs", {
      output_vector <- ddbs_length("rivers", conn = conn_test) |> as.numeric()
      output_column <- ddbs_length("rivers", conn = conn_test, new_column = "length_calc", output = NULL) |> st_as_sf()
      
      expect_identical(output_vector, output_column$length_calc)
    })
  })
  
  ### EXPECTED ERRORS
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_length(x = "nc", conn = NULL))
    })
    
    it("requires new_column when name is specified", {
      expect_error(ddbs_length(rivers_sf, name = "new_tbl"))
    })
    
    it("prevents overwriting existing tables without overwrite = TRUE", {
      expect_error(ddbs_length(rivers_sf, conn = conn_test, name = "countries", new_column = "length_calc"))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_length(x = 999))
      expect_error(ddbs_length(x = "999", conn = conn_test))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_length(rivers_sf, conn = 999))
    })
    
    it("validates new_column argument type", {
      expect_error(ddbs_length(rivers_sf, new_column = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_length(rivers_sf, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_length(rivers_sf, quiet = 999))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_length(rivers_sf, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})


# 3. ddbs_distance -------------------------------------------------------

## 3.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns a matrix
## - CHECK 1.3: messages work
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_distance()", {
  
  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_sf_ddbs   <- ddbs_distance(points_sf, points_ddbs)
      output_ddbs_sf   <- ddbs_distance(points_ddbs, points_sf)
      output_sf_sf     <- ddbs_distance(points_sf, points_sf)
      output_ddbs_ddbs <- ddbs_distance(points_ddbs, points_ddbs)
      output_conn_sf   <- ddbs_distance("points", points_sf, conn = conn_test)
      output_sf_conn   <- ddbs_distance(points_sf, "points", conn = conn_test)
      
      expect_equal(output_sf_ddbs, output_ddbs_sf)
      expect_equal(output_sf_ddbs, output_sf_sf)
      expect_equal(output_sf_ddbs, output_ddbs_ddbs)
      expect_equal(output_sf_ddbs, output_conn_sf)
      expect_equal(output_sf_ddbs, output_sf_conn)
    })
    
    it("warns when mixing DuckDB table with duckspatial_df from different connections", {
      expect_warning(ddbs_distance("points", points_ddbs, conn = conn_test))
      expect_warning(ddbs_distance(points_ddbs, "points", conn = conn_test))
    })

    it("warns when using a geographic CRS different than WGS84", {
      points_nad83 <- ddbs_transform(points_ddbs, "EPSG:4269") |> head()
      expect_warning(ddbs_distance(points_nad83, points_nad83))
    })
    
    it("returns a units matrix", {
      output <- ddbs_distance(points_sf, points_ddbs)
      expect_s3_class(output, "units")
      expect_equal(
        class(units::drop_units(output)), 
        c("matrix", "array")
      )
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_distance(points_sf, points_ddbs))
      expect_no_message(ddbs_distance(points_sf, points_ddbs, quiet = TRUE))
    })

    it("works with dist_type = harvesine", {
      haversine_res <- ddbs_distance(points_ddbs, points_ddbs, dist_type = "haversine")
      expect_s3_class(haversine_res, "units")
    })

    it("works with dist_type = spheroid", {
      spheroid_res <- ddbs_distance(points_ddbs, points_ddbs, dist_type = "spheroid")
      expect_s3_class(spheroid_res, "units")
    })

    it("works with dist_type = planar", {
      points_3857_ddbs <- ddbs_transform(points_ddbs, "EPSG:3857")
      planar_res <- ddbs_distance(points_3857_ddbs, points_3857_ddbs, dist_type = "planar")
      expect_s3_class(planar_res, "units")
    })

    it("works with dist_type = geos", {
      points_3857_ddbs <- ddbs_transform(points_ddbs, "EPSG:3857")
      geos_res <- ddbs_distance(points_3857_ddbs, points_3857_ddbs, dist_type = "geos")
      expect_s3_class(geos_res, "units")
    })

    it("works with default dist_type for geographic", {
      spheroid_res <- ddbs_distance(points_ddbs, points_ddbs)
      expect_s3_class(spheroid_res, "units")
    })

    it("works with default dist_type for projected", {
      points_3857_ddbs <- ddbs_transform(points_ddbs, "EPSG:3857")
      spheroid_res <- ddbs_distance(points_3857_ddbs, points_3857_ddbs)
      expect_s3_class(spheroid_res, "units")
    })

  })

  
  ### EXPECTED ERRORS
  
  describe("errors", {

    it("validates dist_type argument", {
      expect_error(ddbs_distance(points_sf, points_ddbs, dist_type = "best_dist"))
      expect_error(ddbs_distance(points_sf, points_ddbs, dist_type = TRUE))
      expect_error(ddbs_distance(points_sf, points_ddbs, dist_type = 5))
    })

    it("error when using planar/geos with geographic coords", {
      expect_error(ddbs_distance(points_ddbs, points_ddbs, dist_type = "geos"))
      expect_error(ddbs_distance(points_ddbs, points_ddbs, dist_type = "planar"))
    })

    it("error when using geogrpahic coords in geometry different than point", {
      expect_error(ddbs_distance(points_ddbs, nc_4326_sf, dist_type = "haversine"))
      expect_error(ddbs_distance(points_ddbs, nc_4326_sf, dist_type = "spheroid"))
    })

    it("error when using haversine/spheroid in projected CRS", {
      points_3857_ddbs <- ddbs_transform(points_ddbs, "EPSG:3857")
      expect_error(ddbs_distance(points_3857_ddbs, points_3857_ddbs, dist_type = "haversine"))
      expect_error(ddbs_distance(points_3857_ddbs, points_3857_ddbs, dist_type = "spheroid"))
    })
    
    it("requires both x and y arguments", {
      expect_error(ddbs_distance(x = points_ddbs))
      expect_error(ddbs_distance(y = points_ddbs))
    })
    
    it("requires connection when using table names", {
      expect_error(ddbs_distance("points", "points", conn = NULL))
    })
    
    it("requires matching CRS between x and y", {
      points_3857_sf <- sf::st_transform(points_sf, "EPSG:3857")
      
      expect_error(ddbs_distance(points_sf, points_3857_sf))
      expect_error(ddbs_distance(points_3857_sf, points_ddbs))
    })
    
    it("requires matching geometry types", {
      expect_error(ddbs_distance(argentina_sf, points_ddbs))
      expect_error(ddbs_distance(points_ddbs, argentina_sf))
      expect_error(ddbs_distance(points_sf, sf::st_transform(rivers_sf, sf::st_crs(points_sf))))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_distance(x = 999))
      expect_error(ddbs_distance(x = "999", points_ddbs, conn = conn_test))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_distance(points_ddbs, points_ddbs, conn = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_distance(points_ddbs, points_ddbs, quiet = 999))
    })
  })
})

## stop connection
ddbs_stop_conn(conn_test)

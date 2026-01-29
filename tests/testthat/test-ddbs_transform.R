

# skip tests on CRAN
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- ddbs_create_conn()

## store countries
ddbs_write_vector(conn_test, countries_sf, "countries")

## store countries with different CRS
countries_3857_sf <- sf::st_transform(countries_sf, "EPSG:3857")
ddbs_write_vector(conn_test, countries_3857_sf, "countries_3857_test")


# 1. ddbs_transform() ----------------------------------------------------

describe("ddbs_transform()", {
  
  ### EXPECTED BEHAVIOUR - AUTH:CODE CRS
  
  describe("expected behavior with AUTH:CODE CRS", {
    
    it("transforms sf to specified CRS code", {
      output <- ddbs_transform(x = countries_sf, y = "EPSG:3857")
      
      expect_equal(sf::st_crs(output), sf::st_crs("EPSG:3857"))
    })
    
    it("transforms DuckDB table to specified CRS code", {
      output <- ddbs_transform(x = "countries", y = "EPSG:3857", conn = conn_test)
      
      expect_equal(sf::st_crs(output), sf::st_crs("EPSG:3857"))
    })
    
    it("writes transformed DuckDB table to database", {
      output <- ddbs_transform(
        x    = "countries",
        y    = "EPSG:3857",
        conn = conn_test,
        name = "countries_3857"
      )
      
      expect_true(output)
    })
    
    it("overwrites existing table when overwrite = TRUE", {
      output <- ddbs_transform(
        x         = "countries",
        y         = "EPSG:3857",
        conn      = conn_test,
        name      = "countries_3857",
        overwrite = TRUE,
        quiet     = TRUE
      )
      
      expect_true(output)
    })
  })
  
  ### EXPECTED BEHAVIOUR - SF OBJECT CRS
  
  describe("expected behavior with sf object CRS", {
    
    it("transforms sf to CRS from another sf object", {
      countries_3857_sf <- ddbs_transform(x = countries_sf, y = "EPSG:3857", quiet = TRUE)
      output <- ddbs_transform(x = countries_sf, y = countries_3857_sf)
      
      expect_equal(sf::st_crs(output), sf::st_crs("EPSG:3857"))
    })
    
    it("transforms DuckDB table to CRS from sf object", {
      countries_3857_sf <- ddbs_transform(x = countries_sf, y = "EPSG:3857", quiet = TRUE)
      output <- ddbs_transform(x = "countries", y = countries_3857_sf, conn = conn_test)
      
      expect_equal(sf::st_crs(output), sf::st_crs("EPSG:3857"))
    })
    
    it("writes transformed DuckDB table to database with sf CRS", {
      countries_3857_sf <- ddbs_transform(x = countries_sf, y = "EPSG:3857", quiet = TRUE)
      output <- ddbs_transform(
        x    = "countries",
        y    = countries_3857_sf,
        conn = conn_test,
        name = "countries_3857_sf"
      )
      
      expect_true(output)
    })
    
    it("overwrites existing table when overwrite = TRUE with sf CRS", {
      countries_3857_sf <- ddbs_transform(x = countries_sf, y = "EPSG:3857", quiet = TRUE)
      output <- ddbs_transform(
        x         = "countries",
        y         = countries_3857_sf,
        conn      = conn_test,
        name      = "countries_3857_sf",
        overwrite = TRUE,
        quiet     = TRUE
      )
      
      expect_true(output)
    })
  })
  
  ### EXPECTED BEHAVIOUR - DUCKDB TABLE CRS
  
  describe("expected behavior with DuckDB table CRS", {
    
    it("transforms sf to CRS from DuckDB table", {
      output <- ddbs_transform(
        x    = countries_sf,
        y    = "countries_3857_test",
        conn = conn_test
      )
      
      expect_equal(sf::st_crs(output), sf::st_crs("EPSG:3857"))
    })
    
    it("transforms DuckDB table to CRS from another DuckDB table", {
      countries_3857_sf <- ddbs_transform(x = countries_sf, y = "EPSG:3857", quiet = TRUE)
      output <- ddbs_transform(
        x    = "countries",
        y    = countries_3857_sf,
        conn = conn_test
      )
      
      expect_equal(sf::st_crs(output), sf::st_crs("EPSG:3857"))
    })
    
    it("writes transformed DuckDB table to database with DuckDB table CRS", {
      output <- ddbs_transform(
        x    = "countries",
        y    = "countries_3857_test",
        conn = conn_test,
        name = "countries_3857_table"
      )
      
      expect_true(output)
    })
    
    it("overwrites existing table when overwrite = TRUE with DuckDB table CRS", {
      output <- ddbs_transform(
        x         = "countries",
        y         = "countries_3857_test",
        conn      = conn_test,
        name      = "countries_3857_table",
        overwrite = TRUE,
        quiet     = TRUE
      )
      
      expect_true(output)
    })
  })
  
  ### EXPECTED BEHAVIOUR - CRS OBJECT
  
  describe("expected behavior with CRS object", {
    
    it("transforms sf to specified CRS object", {
      output <- ddbs_transform(x = rivers_sf, y = sf::st_crs(countries_sf))
      
      expect_equal(ddbs_crs(output), sf::st_crs(countries_sf))
    })
    
    it("transforms DuckDB table to specified CRS object", {
      output <- ddbs_transform(
        x    = "countries",
        y    = sf::st_crs(rivers_sf),
        conn = conn_test
      )
      
      expect_equal(ddbs_crs(output), sf::st_crs(rivers_sf))
    })
    
    it("writes transformed DuckDB table to database with CRS object", {
      output <- ddbs_transform(
        x    = "countries",
        y    = sf::st_crs(rivers_sf),
        conn = conn_test,
        name = "countries_3035_table"
      )
      
      expect_true(output)
    })
    
    it("overwrites existing table when overwrite = TRUE with CRS object", {
      output <- ddbs_transform(
        x         = "countries",
        y         = sf::st_crs(rivers_sf),
        conn      = conn_test,
        name      = "countries_3035_table",
        overwrite = TRUE,
        quiet     = TRUE
      )
      
      expect_true(output)
    })
  })
  
  ### EXPECTED WARNINGS
  
  describe("warnings", {
    
    it("warns when transforming sf to same CRS (DuckDB table)", {
      expect_warning(ddbs_transform(x = countries_sf, y = "countries", conn = conn_test))
    })
    
    it("warns when transforming DuckDB table to same CRS (sf)", {
      expect_warning(ddbs_transform(x = "countries", y = countries_sf, conn = conn_test))
    })
    
    it("warns when creating new table with same CRS", {
      expect_warning(
        ddbs_transform(
          x    = "countries",
          y    = countries_sf,
          conn = conn_test,
          name = "countries_3857_table_warn"
        )
      )
    })
    
    it("warns when overwriting table with same CRS", {
      expect_warning(
        ddbs_transform(
          x         = "countries",
          y         = countries_sf,
          conn      = conn_test,
          name      = "countries_3857_table",
          overwrite = TRUE,
          quiet     = TRUE
        )
      )
    })
  })
  
  ### EXPECTED ERRORS
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_transform(x = "999", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_transform(x = 999))
      expect_error(ddbs_transform(x = "999", conn = conn_test))
    })
    
    it("validates y argument type", {
      expect_error(ddbs_transform(y = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_transform(conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_transform(overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_transform(quiet = 999))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_transform(conn = conn_test, name = c('banana', 'banana')))
    })
  })
})



## stop connection
ddbs_stop_conn(conn_test)

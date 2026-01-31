
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()
conn_test_2 <- duckspatial::ddbs_create_conn()

## write data in the database
ddbs_write_vector(conn_test, points_sf, "points")
ddbs_write_vector(conn_test, argentina_ddbs, "argentina")
ddbs_write_vector(conn_test_2, argentina_ddbs, "argentina")


# 1. ddbs_predicate ------------------------------------------------------

## 1.1. Expected behaviour ----------

## expected behaviour
## - CHECK 1.1: combination of sf/ddbs/duckdb table work
## - CHECK 1.2: all predicates work
## - CHECK 1.3: conn_x and conn_y work
## - CHECK 1.4: sparse returns a matrix
## - CHECK 1.5: returns same as SF
## - CHECK 1.6: id_x and id_y work
## - CHECK 2.1: Combination of inputs / missing arguments
## - CHECK 2.2: other errors
describe("ddbs_predicate()", {
  
  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("works on all format combinations (sf, duckspatial_df, DuckDB table)", {
      output_sf_sf       <- ddbs_predicate(points_sf, argentina_sf)
      output_ddbs_sf     <- ddbs_predicate(points_ddbs, argentina_sf)
      output_sf_ddbs     <- ddbs_predicate(points_sf, argentina_ddbs)
      output_ddbs_ddbs   <- ddbs_predicate(points_ddbs, argentina_ddbs)
      output_conn_sf     <- ddbs_predicate("points", argentina_sf, conn = conn_test)
      output_sf_conn     <- ddbs_predicate(points_sf, "argentina", conn = conn_test)
      output_conn_conn   <- ddbs_predicate("points", "argentina", conn = conn_test)
      
      expect_type(output_sf_sf, "list")
      expect_equal(output_sf_sf, output_ddbs_sf)
      expect_equal(output_sf_sf, output_sf_ddbs)
      expect_equal(output_sf_sf, output_ddbs_ddbs)
      expect_equal(output_sf_sf, output_conn_sf)
      expect_equal(output_sf_sf, output_sf_conn)
      expect_equal(output_sf_sf, output_conn_conn)
    })
    
    it("warns when mixing DuckDB table with duckspatial_df from different connections", {
      expect_warning(ddbs_predicate("points", argentina_ddbs, conn = conn_test))
      expect_warning(ddbs_predicate(points_ddbs, "argentina", conn = conn_test))
    })
    
    it("works with intersects predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "intersects")
      expect_type(output_predicate, "list")
    })
    
    it("works with covers predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "covers")
      output_function  <- ddbs_covers(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with touches predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "touches")
      output_function  <- ddbs_touches(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with disjoint predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "disjoint")
      output_function  <- ddbs_disjoint(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with within predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "within")
      output_function  <- ddbs_within(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with contains predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "contains")
      output_function  <- ddbs_contains(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with overlaps predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "overlaps")
      output_function  <- ddbs_overlaps(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with covered_by predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "covered_by")
      output_function  <- ddbs_covered_by(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with intersects_extent predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "intersects_extent")
      output_function  <- ddbs_intersects_extent(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with contains_properly predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "contains_properly")
      output_function  <- ddbs_contains_properly(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with within_properly predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "within_properly")
      output_function  <- ddbs_within_properly(countries_sf, argentina_sf)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("works with dwithin predicate", {
      output_predicate <- ddbs_predicate(countries_sf, argentina_sf, predicate = "dwithin", distance = 100)
      output_function  <- ddbs_is_within_distance(countries_sf, argentina_sf, 10)
      
      expect_equal(output_predicate, output_function)
    })
    
    it("supports conn_x and conn_y for different connections", {
      expect_warning(ddbs_predicate("points", "argentina", conn_x = conn_test, conn_y = conn_test_2))
      
      output_different_conn <- suppressWarnings(ddbs_predicate("points", "argentina", conn_x = conn_test, conn_y = conn_test_2))
      output_same_result    <- ddbs_predicate(points_sf, argentina_sf)
      
      expect_equal(output_different_conn, output_same_result)
    })
    
    it("returns matrix when sparse = FALSE", {
      output_sparse <- ddbs_predicate(points_ddbs, argentina_ddbs, sparse = FALSE)
      
      expect_true(inherits(output_sparse, "matrix"))
    })
    
    it("returns same results as sf when sparse = FALSE for covers", {
      output_ddbs <- ddbs_covers(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_covers(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("returns same results as sf when sparse = FALSE for touches", {
      output_ddbs <- ddbs_touches(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_touches(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("returns same results as sf when sparse = FALSE for disjoint", {
      output_ddbs <- ddbs_disjoint(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_disjoint(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("returns same results as sf when sparse = FALSE for within", {
      output_ddbs <- ddbs_within(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_within(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("returns same results as sf when sparse = FALSE for contains", {
      output_ddbs <- ddbs_contains(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_contains(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("returns same results as sf when sparse = FALSE for overlaps", {
      output_ddbs <- ddbs_overlaps(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_overlaps(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("returns same results as sf when sparse = FALSE for covered_by", {
      output_ddbs <- ddbs_covered_by(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_covered_by(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("returns same results as sf when sparse = FALSE for intersects_extent", {
      output_ddbs <- ddbs_intersects_extent(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_intersects(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("returns same results as sf when sparse = FALSE for contains_properly", {
      output_ddbs <- ddbs_contains_properly(countries_sf, argentina_sf, sparse = FALSE)
      output_sf   <- sf::st_contains_properly(countries_sf, argentina_sf, sparse = FALSE)
      
      expect_equal(output_ddbs, output_sf)
    })
    
    it("supports id_x parameter to name output list elements", {
      output <- ddbs_predicate(countries_sf, argentina_sf, "touches", id_x = "CNTR_ID")
      
      expect_equal(names(output), countries_sf$CNTR_ID)
      expect_equal(output[[2]], 1)
    })
    
    it("supports id_y parameter to use custom IDs in results", {
      output <- ddbs_predicate(countries_sf, argentina_sf, "touches", id_y = "CNTR_ID")
      
      expect_null(names(output))
      expect_equal(output[[2]], "AR")
    })
    
    it("supports both id_x and id_y parameters together", {
      output <- ddbs_predicate(countries_sf, argentina_sf, "touches", id_x = "CNTR_ID", id_y = "CNTR_ID")
      
      expect_equal(names(output), countries_sf$CNTR_ID)
      expect_equal(output[[2]], "AR")
    })
  })
  
  ### EXPECTED ERRORS
  
  describe("errors", {
    
    it("requires both x and y arguments", {
      expect_error(ddbs_predicate(argentina_ddbs))
      expect_error(ddbs_predicate(y = argentina_ddbs))
    })
    
    it("requires connection when using table names", {
      expect_error(ddbs_predicate("argentina", conn = NULL))
    })
    
    it("validates predicate argument", {
      expect_error(ddbs_predicate(argentina_ddbs, points_sf, predicate = "intersect_this"))
    })
    
    it("validates sparse argument type", {
      expect_error(ddbs_predicate(argentina_ddbs, points_sf, sparse = "TRUE"))
    })
    
    it("validates distance argument type for dwithin", {
      expect_error(ddbs_is_within_distance(argentina_ddbs, distance = "many kilometers"))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_predicate(x = 999))
      expect_error(ddbs_predicate(x = "999", points_sf, conn = conn_test))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_predicate(argentina_ddbs, points_sf, conn = 999))
    })

    it("validates conn_x argument type", {
      expect_error(ddbs_predicate(argentina_ddbs, "points", conn_y = 999))
    })

    it("validates conn_y argument type", {
      expect_error(ddbs_predicate("argentina", points_sf, conn_x = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_predicate(argentina_ddbs, points_sf, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_predicate(argentina_ddbs, points_sf, quiet = 999))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_predicate(argentina_ddbs, points_sf, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})

## stop connection
ddbs_stop_conn(conn_test)

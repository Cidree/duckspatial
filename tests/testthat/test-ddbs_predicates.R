
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
testthat::test_that("ddbs_predicate(): expected behavior", {
  
  ## CHECK 1.1
  output_1 <- ddbs_predicate(points_sf, argentina_sf)
  output_2 <- ddbs_predicate(points_ddbs, argentina_sf)
  output_3 <- ddbs_predicate(points_sf, argentina_ddbs)
  output_4 <- ddbs_predicate(points_ddbs, argentina_ddbs)  
  testthat::expect_warning(ddbs_predicate("points", argentina_ddbs, conn = conn_test))
  output_6 <- ddbs_predicate("points", argentina_sf, conn = conn_test)
  output_7 <- ddbs_predicate(points_sf, "argentina", conn = conn_test)
  testthat::expect_warning(ddbs_predicate(points_ddbs, "argentina", conn = conn_test))
  output_9 <- ddbs_predicate("points", "argentina", conn = conn_test)

  testthat::expect_type(output_1, "list")

  testthat::expect_equal(output_1, output_2)
  testthat::expect_equal(output_1, output_3)
  testthat::expect_equal(output_1, output_4)
  testthat::expect_equal(output_1, output_6)
  testthat::expect_equal(output_1, output_7)
  testthat::expect_equal(output_1, output_9)

  ## CHECK 1.2
  intersects_ddbs <- ddbs_predicate(countries_sf, argentina_sf)
  covers_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "covers")
  touches_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "touches")
  disjoint_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "disjoint")
  within_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "within")
  contains_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "contains")
  overlaps_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "overlaps")
  covered_by_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "covered_by")
  intersects_extent_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "intersects_extent")
  contains_properly_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "contains_properly")
  within_properly_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "within_properly")
  dwithin_ddbs <- ddbs_predicate(countries_sf, argentina_sf, predicate = "dwithin", distance = 100)

  covers_ddbs_2 <- ddbs_covers(countries_sf, argentina_sf)
  touches_ddbs_2 <- ddbs_touches(countries_sf, argentina_sf)
  disjoint_ddbs_2 <- ddbs_disjoint(countries_sf, argentina_sf)
  within_ddbs_2 <- ddbs_within(countries_sf, argentina_sf)
  contains_ddbs_2 <- ddbs_contains(countries_sf, argentina_sf)
  overlaps_ddbs_2 <- ddbs_overlaps(countries_sf, argentina_sf)
  covered_by_ddbs_2 <- ddbs_covered_by(countries_sf, argentina_sf)
  intersects_extent_ddbs_2 <- ddbs_intersects_extent(countries_sf, argentina_sf)
  contains_properly_ddbs_2 <- ddbs_contains_properly(countries_sf, argentina_sf)
  within_properly_ddbs_2 <- ddbs_within_properly(countries_sf, argentina_sf)
  dwithin_ddbs_2 <- ddbs_is_within_distance(countries_sf, argentina_sf, 10)
  
  testthat::expect_equal(covers_ddbs, covers_ddbs_2)
  testthat::expect_equal(touches_ddbs, touches_ddbs_2)
  testthat::expect_equal(disjoint_ddbs, disjoint_ddbs_2)
  testthat::expect_equal(within_ddbs, within_ddbs_2)
  testthat::expect_equal(contains_ddbs, contains_ddbs_2)
  testthat::expect_equal(overlaps_ddbs, overlaps_ddbs_2)
  testthat::expect_equal(covered_by_ddbs, covered_by_ddbs_2)
  testthat::expect_equal(intersects_extent_ddbs, intersects_extent_ddbs_2)
  testthat::expect_equal(contains_properly_ddbs, contains_properly_ddbs_2)
  testthat::expect_equal(within_properly_ddbs, within_properly_ddbs_2)
  testthat::expect_equal(dwithin_ddbs, dwithin_ddbs_2)
  

  ## CHECK 1.3
  testthat::expect_warning(ddbs_predicate("points", "argentina", conn_x = conn_test, conn_y = conn_test_2))
  output_10 <- suppressWarnings(ddbs_predicate("points", "argentina", conn_x = conn_test, conn_y = conn_test_2))
  testthat::expect_equal(output_1, output_10)


  ## CHECK 1.4
  output_11 <- ddbs_predicate(points_ddbs, argentina_ddbs, sparse = FALSE)
  testthat::expect_true(inherits(output_11, "matrix"))


  ## CHECK 1.5
  covers_ddbs_sparse <- ddbs_covers(countries_sf, argentina_sf, sparse = FALSE)
  touches_ddbs_sparse <- ddbs_touches(countries_sf, argentina_sf, sparse = FALSE)
  disjoint_ddbs_sparse <- ddbs_disjoint(countries_sf, argentina_sf, sparse = FALSE)
  within_ddbs_sparse <- ddbs_within(countries_sf, argentina_sf, sparse = FALSE)
  contains_ddbs_sparse <- ddbs_contains(countries_sf, argentina_sf, sparse = FALSE)
  overlaps_ddbs_sparse <- ddbs_overlaps(countries_sf, argentina_sf, sparse = FALSE)
  covered_by_ddbs_sparse <- ddbs_covered_by(countries_sf, argentina_sf, sparse = FALSE)
  intersects_extent_ddbs_sparse <- ddbs_intersects_extent(countries_sf, argentina_sf, sparse = FALSE)
  contains_properly_ddbs_sparse <- ddbs_contains_properly(countries_sf, argentina_sf, sparse = FALSE)


  covers_sf <- sf::st_covers(countries_sf, argentina_sf, sparse = FALSE)
  touches_sf <- sf::st_touches(countries_sf, argentina_sf, sparse = FALSE)
  disjoint_sf <- sf::st_disjoint(countries_sf, argentina_sf, sparse = FALSE)
  within_sf <- sf::st_within(countries_sf, argentina_sf, sparse = FALSE)
  contains_sf <- sf::st_contains(countries_sf, argentina_sf, sparse = FALSE)
  overlaps_sf <- sf::st_overlaps(countries_sf, argentina_sf, sparse = FALSE)
  covered_by_sf <- sf::st_covered_by(countries_sf, argentina_sf, sparse = FALSE)
  intersects_extent_sf <- sf::st_intersects(countries_sf, argentina_sf, sparse = FALSE)
  contains_properly_sf <- sf::st_contains_properly(countries_sf, argentina_sf, sparse = FALSE)


  testthat::expect_equal(covers_ddbs_sparse, covers_sf)
  testthat::expect_equal(touches_ddbs_sparse, touches_sf)
  testthat::expect_equal(disjoint_ddbs_sparse, disjoint_sf)
  testthat::expect_equal(within_ddbs_sparse, within_sf)
  testthat::expect_equal(contains_ddbs_sparse, contains_sf)
  testthat::expect_equal(overlaps_ddbs_sparse, overlaps_sf)
  testthat::expect_equal(covered_by_ddbs_sparse, covered_by_sf)
  testthat::expect_equal(intersects_extent_ddbs_sparse, intersects_extent_sf)
  testthat::expect_equal(contains_properly_ddbs_sparse, contains_properly_sf)

  ## CHECK 1.6
  output_idx <- ddbs_predicate(countries_sf, argentina_sf, "touches", id_x = "CNTR_ID")
  testthat::expect_equal(names(output_idx), countries_sf$CNTR_ID)
  testthat::expect_equal(output_idx[[2]], 1)

  output_idy <- ddbs_predicate(countries_sf, argentina_sf, "touches", id_y = "CNTR_ID")
  testthat::expect_null(names(output_idy))
  testthat::expect_equal(output_idy[[2]], "AR")

  output_idx_idy <- ddbs_predicate(countries_sf, argentina_sf, "touches", id_x = "CNTR_ID", id_y = "CNTR_ID")
  testthat::expect_equal(names(output_idx_idy), countries_sf$CNTR_ID)
  testthat::expect_equal(output_idy[[2]], "AR")
    
})




## 1.2. Errors -------------------------

## CHECK 2.1: Combination of inputs / missing arguments
## CHECK 2.2: other errors
testthat::test_that("ddbs_predicate(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_predicate(argentina_ddbs))
    testthat::expect_error(ddbs_predicate(y = argentina_ddbs))
    testthat::expect_error(ddbs_predicate(argentina_ddbs, points_sf, predicate = "intersect_this"))
    testthat::expect_error(ddbs_predicate(argentina_ddbs, points_sf, sparse = "TRUE"))
    testthat::expect_error(ddbs_is_within_distance(argentina_ddbs, distance = "many kilometers"))
    testthat::expect_error(ddbs_predicate("argentina", conn = NULL))
  
    ## CHECK 2.2.
    testthat::expect_error(ddbs_predicate(x = 999))
    testthat::expect_error(ddbs_predicate(argentina_ddbs, points_sf, conn = 999))
    testthat::expect_error(ddbs_predicate(argentina_ddbs, points_sf, overwrite = 999))
    testthat::expect_error(ddbs_predicate(argentina_ddbs, points_sf, quiet = 999))
    testthat::expect_error(ddbs_predicate(x = "999", points_sf, conn = conn_test))
    testthat::expect_error(ddbs_predicate(argentina_ddbs, points_sf, conn = conn_test, name = c('banana', 'banana')))
  
})

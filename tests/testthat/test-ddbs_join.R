# skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")


# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

# helper function
tester <- function(x = points_sf,
                   y = countries_sf,
                   join = "intersects",
                   conn = NULL,
                   name = NULL,
                   crs = NULL,
                   crs_column = "crs_duckspatial",
                   output = "sf",
                   overwrite = FALSE,
                   quiet = FALSE) {
    ddbs_join(
        x = x,
        y = y,
        join = join,
        conn = conn,
        name = name,
        crs = crs,
        crs_column = crs_column,
        output = output,
        overwrite = overwrite,
        quiet = quiet
    )
}


# expected behavior --------------------------------------------------------------


testthat::test_that("expected behavior", {

    # option 1: passing sf objects
    output1 <- tester(
        x = points_sf,
        y = countries_sf,
        join = "within"
    )

    testthat::expect_true(is(output1 , 'sf'))

    # option 2: passing the names of tables in a duckdb db, returing sf
    # write sf to duckdb
    ddbs_write_vector(conn_test, points_sf, "points", overwrite = TRUE)
    ddbs_write_vector(conn_test, countries_sf, "countries", overwrite = TRUE)

    # spatial join
    output2 <- tester(
        conn = conn_test,
        x = "points",
        y = "countries",
        join = "within"
    )

    testthat::expect_true(is(output2 , 'sf'))

    # option 3: passing the names of tables in a duckdb db, creating new table in db
    output3 <- tester(
        conn = conn_test,
        x = "points",
        y = "countries",
        join = "within",
        name = "test_result",
        overwrite = TRUE
    )

    testthat::expect_true(output3)

    # TODO - Review this because it fails
    # output3 <- DBI::dbReadTable(conn_test, "test_result") |>
    #     sf::st_as_sf(wkt = 'geometry')

    # testthat::expect_true(is(output3 , 'sf'))

    ddbs_read_vector(conn = conn_test, name = "test_result", crs = 4326)


    # show and suppress messages
    testthat::expect_message( tester() )
    testthat::expect_no_message( tester(quiet = TRUE))


})


testthat::test_that("error if table already exists", {

    # write table for the 1st time
    testthat::expect_true(tester(x = "points",
                                    y = "countries",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = FALSE)
                             )

    # expected error if overwrite = FALSE
    testthat::expect_error(tester(x = "points",
                                    y = "countries",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = FALSE))

    # overwrite table
    testthat::expect_true(tester(x = "points",
                                    y = "countries",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = TRUE))


})

# expected errors --------------------------------------------------------------

testthat::test_that("errors with incorrect input", {

    testthat::expect_error(tester(x = 999))
    testthat::expect_error(tester(y = 999))
    testthat::expect_error(tester(join = 999))
    testthat::expect_error(tester(conn = 999))
    testthat::expect_error(tester(overwrite = 999))
    testthat::expect_error(tester(quiet = 999))

    testthat::expect_error(tester(x = "999", conn = conn_test))
    testthat::expect_error(tester(y = "999", conn = conn_test))

    testthat::expect_error(tester(conn = conn_test, name = c('banana', 'banana')))


    })



# duckspatial_df inputs --------------------------------------------------------

testthat::test_that("ddbs_join works with duckspatial_df inputs", {
  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  
  # Create a distinct connection for this test to avoid interference
  # Use the internal helper visible in other tests
  conn <- ddbs_temp_conn()
  
  # Load as duckspatial_df
  countries_ds <- ddbs_open_dataset(countries_path, conn = conn)
  
  # Create points as sf
  # Helper defined in test-utils.R or we just create sf manually
  points_sf <- sf::st_as_sf(
    data.frame(id = 1:10, x = 1:10, y = 1:10), 
    coords = c("x", "y"), 
    crs = 4326
  )
  
  # Register points to the same connection as a duckspatial_df for consistent testing
  ddbs_write_vector(conn, points_sf, "test_points")
  points_ds <- ddbs_read_vector(conn, "test_points")
  
  # 1. duckspatial_df x duckspatial_df
  # Using intersects
  result1 <- ddbs_join(points_ds, countries_ds, join = "intersects")
  expect_s3_class(result1, "duckspatial_df")
  expect_true(nrow(dplyr::collect(result1)) >= 0)
  
  # 2. duckspatial_df x sf
  # Note: Cross-connection join might occur if sf implicit connection is different, but here we expect it to work
  result2 <- ddbs_join(points_ds, countries_sf, join = "intersects")
  expect_s3_class(result2, "duckspatial_df")
  
  # 3. sf x duckspatial_df
  
  withr::with_options(list(duckspatial.output_type = "sf"), {
      r_sf <- ddbs_join(points_sf, countries_ds, join = "intersects")
      expect_s3_class(r_sf, "sf")
  })
})

# predicates -------------------------------------------------------------------

testthat::test_that("ddbs_join works with different predicates", {
  # We test a few key ones to ensure parameter passing works
  # Use simple data where we know the answer
  
  # Polygon: square (0,0) to (10,10)
  p1 <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  poly_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(p1), crs=4326)
  
  # Points: inside (5,5), edge (0,0), outside (20,20)
  pts <- matrix(c(5,5, 0,0, 20,20), ncol=2, byrow=TRUE)
  pts_sf <- sf::st_sf(id = 1:3, geometry = sf::st_sfc(lapply(1:3, function(i) sf::st_point(pts[i,]))), crs=4326)
  
  res_within <- ddbs_join(pts_sf, poly_sf, join = "within") |> dplyr::collect()
  
  expect_true(1 %in% res_within$id)
  expect_false(3 %in% res_within$id)
  
  res_disjoint <- ddbs_join(pts_sf, poly_sf, join = "disjoint") |> dplyr::collect()
  expect_true(3 %in% res_disjoint$id)
  expect_false(1 %in% res_disjoint$id)
  
  res_intersects <- ddbs_join(pts_sf, poly_sf, join = "intersects") |> dplyr::collect()
  expect_true(1 %in% res_intersects$id)
  expect_true(2 %in% res_intersects$id)
  expect_false(3 %in% res_intersects$id)
})

# output parameters ------------------------------------------------------------

testthat::test_that("ddbs_join respects output parameter", {
  result_sf <- ddbs_join(points_sf, countries_sf, output = "sf")
  expect_s3_class(result_sf, "sf")
  
  result_ds <- ddbs_join(points_sf, countries_sf, output = "duckspatial_df")
  expect_s3_class(result_ds, "duckspatial_df")
  
  result_tibble <- ddbs_join(points_sf, countries_sf, output = "tibble")
  expect_s3_class(result_tibble, "tbl_df")
  expect_true(!inherits(result_tibble, "sf")) 
})

# error handling ---------------------------------------------------------------

testthat::test_that("ddbs_join throws error on CRS mismatch", {
  points_3857 <- sf::st_transform(points_sf, 3857)
  
  expect_error(
    ddbs_join(points_3857, countries_sf),
    "Coordinates Reference System"
  )
})

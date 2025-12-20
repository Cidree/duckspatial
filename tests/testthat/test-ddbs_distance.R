# skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")


# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

# using fewer points
points_sf <- points_sf[1:10,]

# helper function
tester <- function(x = points_sf,
                   y = points_sf,
                   dist_type = "haversine",
                   conn = NULL,
                   quiet = FALSE) {
    ddbs_distance(
        x,
        y,
        dist_type,
        conn,
        quiet
    )
}


# expected behavior --------------------------------------------------------------


testthat::test_that("expected behavior", {

    # option 1: passing sf objects
    output1 <- tester(
        x = points_sf,
        y = points_sf
    )

    testthat::expect_true(is(output1 , 'matrix'))
    testthat::expect_true(all(dim(output1)== c(10,10)))

    # option 2: passing the names of tables in a duckdb db, returing sf
    # write sf to duckdb
    ddbs_write_vector(conn_test, points_sf, "points", overwrite = TRUE)

    # spatial join
    output2 <- tester(
        conn = conn_test,
        x = "points",
        y = "points"
    )

    testthat::expect_true(is(output2 , 'matrix'))

    # planar distances
    output_planar <- tester(
        x = points_sf,
        y = countries_sf,
        dist_type = "planar"
    )
    testthat::expect_true(is(output_planar , 'matrix'))

    # show and suppress messages
    testthat::expect_message( tester() )
    testthat::expect_no_message( tester(quiet = TRUE))


})


# testthat::test_that("error if table already exists", {
#
#     # write table for the 1st time
#     testthat::expect_true(tester(conn = conn_test,
#                                  name = 'banana',
#                                  overwrite = FALSE)
#                           )
#
#     # expected error if overwrite = FALSE
#     testthat::expect_error(tester(x = "points",
#                                     y = "points",
#                                     conn = conn_test,
#                                     name = 'banana',
#                                     overwrite = FALSE)
#                            )
#
#     # overwrite table
#     testthat::expect_true(tester(conn = conn_test,
#                                  name = 'banana',
#                                  overwrite = TRUE)
#                           )
#
#
# })

# expected errors --------------------------------------------------------------

testthat::test_that("incorrect geometry or crs", {

    # not POINT with haversine distance
    testthat::expect_error(
        tester(x = points_sf, y = countries_sf)
        )

    # wrong crs with haversine distance
    points_sf_utm <- sf::st_transform(points_sf, 3857)
    testthat::expect_error(
        tester(x = points_sf_utm, y = points_sf_utm)
    )

})



testthat::test_that("incorrect input", {

    testthat::expect_error(tester(x = 999))
    testthat::expect_error(tester(y = 999))
    testthat::expect_error(tester(dist_type = 999))
    testthat::expect_error(tester(conn = 999))
    testthat::expect_error(tester(overwrite = 999))
    testthat::expect_error(tester(quiet = 999))

    testthat::expect_error(tester(x = "999", conn = conn_test))
    testthat::expect_error(tester(y = "999", conn = conn_test))


    })

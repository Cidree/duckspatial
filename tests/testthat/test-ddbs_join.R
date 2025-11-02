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
                   join = "ST_Intersects",
                   conn = NULL,
                   name = NULL,
                   crs = NULL,
                   crs_column = "crs_duckspatial",
                   overwrite = FALSE,
                   quiet = FALSE) {
    ddbs_join(
        x,
        y,
        join,
        conn,
        name,
        crs,
        crs_column,
        overwrite,
        quiet
    )
}


# expected behavior --------------------------------------------------------------

# TO DO
testthat::test_that("expected behavior", {

    # option 1: passing sf objects
    output1 <- tester(
        x = points_sf,
        y = countries_sf,
        join = "ST_Within"
    )

    testthat::expect_true(is(output1 , 'sf'))

    # option 2: passing the names of tables in a duckdb db
    # write sf to duckdb
    ddbs_write_vector(conn_test, points_sf, "points", overwrite = TRUE)
    ddbs_write_vector(conn_test, countries_sf, "countries", overwrite = TRUE)

    # spatial join
    output2 <- ddbs_join(
        conn_test,
        x = "points",
        y = "countries",
        join = "ST_Within"
    )

    testthat::expect_true(is(output2 , 'sf'))

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

    })

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


# expected errors --------------------------------------------------------------

test_that("errors with incorrect input", {

    testthat::expect_error(tester(x = 999))
    testthat::expect_error(tester(y = 999))
    testthat::expect_error(tester(join = 999))
    testthat::expect_error(tester(conn = 999))
    testthat::expect_error(tester(overwrite = 999))
    testthat::expect_error(tester(quiet = 999))

    testthat::expect_error(tester(x = "999", conn = conn_test))
    testthat::expect_error(tester(y = "999", conn = conn_test))

    })

# skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")


rivers_sf <- sf::st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))


# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

# helper function
tester <- function(x = rivers_sf,
                   by_feature = FALSE,
                   conn = NULL,
                   name = NULL,
                   crs = NULL,
                   crs_column = "crs_duckspatial",
                   overwrite = FALSE,
                   quiet = FALSE) {
    ddbs_bbox(
        x,
        by_feature,
        conn,
        name,
        crs,
        crs_column,
        overwrite,
        quiet
    )
}


# expected behavior --------------------------------------------------------------


testthat::test_that("expected behavior", {

    # option 1: passing sf objects
    output1 <- tester(
        x = rivers_sf,
    )

    testthat::expect_true(is(output1 , 'data.frame'))

    # option 2: passing the names of tables in a duckdb db, returing sf
    # write sf to duckdb
    ddbs_write_vector(conn_test, rivers_sf, "rivers_tbl", overwrite = TRUE)

    # spatial join
    output2 <- tester(
        conn_test,
        x = "rivers_tbl"
    )

    testthat::expect_true(is(output2 , 'data.frame'))

    # option 3: passing the names of tables in a duckdb db, creating new table in db
    output3 <- tester(
        conn_test,
        x = "rivers_tbl",
        name = "test_result",
        overwrite = TRUE
    )

    testthat::expect_true(output3)

    # read table from db
    output3 <- DBI::dbReadTable(conn_test, "test_result")
    testthat::expect_true(is(output3 , 'data.frame'))


    # show and suppress messages
    testthat::expect_message( tester() )
    testthat::expect_no_message( tester(quiet = TRUE))


})


testthat::test_that("expected behavior of by_feature", {

    output1 <- tester(
        x = rivers_sf,
        by_feature = FALSE
    )

    testthat::expect_true(nrow(output1)==1)

    output2 <- tester(
        x = rivers_sf,
        by_feature = TRUE
    )

    testthat::expect_true(nrow(output2)==nrow(rivers_sf))


})


testthat::test_that("error if table already exists", {

    # write table for the 1st time
    testthat::expect_true(tester(x = "rivers_tbl",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = FALSE)
                             )

    # expected error if overwrite = FALSE
    testthat::expect_error(tester(x = "rivers_tbl",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = FALSE))

    # overwrite table
    testthat::expect_true(tester(x = "rivers_tbl",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = TRUE))


})

# expected errors --------------------------------------------------------------

testthat::test_that("errors with incorrect input", {

    testthat::expect_error(tester(x = 999))
    testthat::expect_error(tester(conn = 999))
    testthat::expect_error(tester(by_feature = 999))
    testthat::expect_error(tester(overwrite = 999))
    testthat::expect_error(tester(quiet = 999))

    testthat::expect_error(tester(x = "999", conn = conn_test))

    testthat::expect_error(tester(conn = conn_test, name = c('banana', 'banana')))


    })


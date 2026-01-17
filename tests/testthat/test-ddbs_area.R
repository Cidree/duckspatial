# skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")


countries_sf3 <- countries_sf[1:3,]

# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

# helper function
tester <- function(x = countries_sf3,
                   conn = NULL,
                   name = NULL,
                   new_column = NULL,
                   crs = NULL,
                   crs_column = "crs_duckspatial",
                   overwrite = FALSE,
                   quiet = FALSE) {
    ddbs_area(
        x,
        conn,
        name,
        new_column,
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
        x = countries_sf3
    )

    testthat::expect_true(is(output1 , 'vector'))

    # option 2: passing the names of tables in a duckdb db, returing sf
    # write sf to duckdb
    ddbs_write_vector(conn_test, countries_sf3, "countries_tbl", overwrite = TRUE)

    # spatial join
    output2 <- tester(
       conn = conn_test,
        x = "countries_tbl"
    )

    testthat::expect_true(is(output2 , 'vector'))

    # option 3: passing the names of tables in a duckdb db, creating new table in db
    output3 <- tester(
        conn = conn_test,
        new_column = "test_col",
        x = "countries_tbl",
        name = "test_result",
        overwrite = TRUE
    )

    testthat::expect_true(output3)

    # read table from db
    output3 <- DBI::dbReadTable(conn_test, "test_result")
    testthat::expect_true(is(output3 , 'data.frame'))


    # show and suppress messages
    testthat::expect_message( tester(
        conn = conn_test,
        new_column = "test_col",
        x = "countries_tbl",
        name = "test_result",
        overwrite = TRUE
    ) )

    testthat::expect_no_message( tester(
        conn = conn_test,
        new_column = "test_col",
        x = "countries_tbl",
        name = "test_result",
        overwrite = TRUE,
        quiet = TRUE)
        )

})


testthat::test_that("expected behavior of new_column", {

    output1 <- tester(
        x = countries_sf3,
        new_column = "test_column"
    )

    testthat::expect_true(is(output1 , 'sf'))
    testthat::expect_true(nrow(output1)==3)
    testthat::expect_true("test_column" %in% names(output1))

    tester(
        conn = conn_test,
        x = "countries_tbl",
        name = "test_result",
        new_column = "test_column2",
        overwrite = TRUE
    )
    output3 <- duckspatial::ddbs_read_vector(conn_test, name = "test_result")

    testthat::expect_true(is(output3 , 'sf'))
    testthat::expect_true(nrow(output3)==3)
    testthat::expect_true("test_column2" %in% names(output3))


})


testthat::test_that("error if table already exists", {

    # write table for the 1st time
    testthat::expect_true(tester(x = "countries_tbl",
                                    conn = conn_test,
                                    new_column = "test_col",
                                    name = 'banana',
                                    overwrite = TRUE)
                             )

    # expected error if overwrite = FALSE
    testthat::expect_error(tester(x = "countries_tbl",
                                  conn = conn_test,
                                  new_column = "test_col",
                                  name = 'banana',
                                    overwrite = FALSE))

    # overwrite table
    testthat::expect_true(tester(x = "countries_tbl",
                                 conn = conn_test,
                                 new_column = "test_col",
                                 name = 'banana',
                                   overwrite = TRUE))


})

# expected errors --------------------------------------------------------------

testthat::test_that("errors with incorrect input", {

    testthat::expect_error(tester(x = 999))
    testthat::expect_error(tester(conn = 999))
    testthat::expect_error(tester(new_column = 999))
    testthat::expect_error(tester(overwrite = 999))
    testthat::expect_error(tester(quiet = 999))

    testthat::expect_error(tester(x = "999", conn = conn_test))

    testthat::expect_error(tester(conn = conn_test, name = c('banana', 'banana')))


    })


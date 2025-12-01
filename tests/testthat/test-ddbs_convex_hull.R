# skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")


argentina_sf <- sf::st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))


# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

# helper function
tester <- function(x = argentina_sf,
                   conn = NULL,
                   name = NULL,
                   crs = NULL,
                   crs_column = "crs_duckspatial",
                   overwrite = FALSE,
                   quiet = FALSE) {
    ddbs_convex_hull(
        x,
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
        x = argentina_sf
    )

    testthat::expect_true(is(output1 , 'sf'))

    # option 2: passing the names of tables in a duckdb db, returing sf
    # write sf to duckdb
    ddbs_write_vector(conn_test, argentina_sf, "argentina_tbl", overwrite = TRUE)

    # spatial join
    output2 <- tester(
        conn_test,
        x = "argentina_tbl"
    )

    testthat::expect_true(is(output2 , 'sf'))

    # option 3: passing the names of tables in a duckdb db, creating new table in db
    output3 <- tester(
        conn_test,
        x = "argentina_tbl",
        name = "test_result",
        overwrite = TRUE
    )

    testthat::expect_true(output3)

    # TODO - Review this because it fails
    # output3 <- DBI::dbReadTable(conn_test, "test_result") |>
    #     sf::st_as_sf(wkt = 'geometry')

    # testthat::expect_true(is(output3 , 'sf'))

    testthat::expect_true(
        is(ddbs_read_vector(conn_test, name = "test_result", crs = 4326) ,
           'sf'
           )
        )



    # show and suppress messages
    testthat::expect_message( tester() )
    testthat::expect_no_message( tester(quiet = TRUE))


})


testthat::test_that("error if table already exists", {

    # write table for the 1st time
    testthat::expect_true(tester(x = "argentina_tbl",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = FALSE)
                             )

    # expected error if overwrite = FALSE
    testthat::expect_error(tester(x = "argentina_tbl",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = FALSE))

    # overwrite table
    testthat::expect_true(tester(x = "argentina_tbl",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = TRUE))


})

# expected errors --------------------------------------------------------------

testthat::test_that("errors with incorrect input", {

    testthat::expect_error(tester(x = 999))
    testthat::expect_error(tester(conn = 999))
    testthat::expect_error(tester(overwrite = 999))
    testthat::expect_error(tester(quiet = 999))

    testthat::expect_error(tester(x = "999", conn = conn_test))

    testthat::expect_error(tester(conn = conn_test, name = c('banana', 'banana')))


    })


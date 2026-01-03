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

# duckdbfs interoperability ---------------------------------------------------

testthat::test_that("ddbs_join works with raw duckdbfs tbl_duckdb_connection", {
  skip_if_not_installed("duckdbfs")
  
  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  
  # Both from duckdbfs - different connections but same CRS
  # Using open_dataset directly (no head) preserves source table for CRS detection
  conn1 <- ddbs_temp_conn()
  conn2 <- ddbs_temp_conn()
  
  countries1 <- duckdbfs::open_dataset(
    countries_path, format = "sf", conn = conn1
  )
  countries2 <- duckdbfs::open_dataset(
    countries_path, format = "sf", conn = conn2
  )
  
  # Cross-connection join should work with warning about different connections
  testthat::expect_warning(
    result <- ddbs_join(countries1, countries2, join = "intersects"),
    "different DuckDB connections"
  )
  
  testthat::expect_true(
    inherits(result, "duckspatial_df") || inherits(result, "sf")
  )
})

# skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
skip_on_cran()
skip_if_not_installed("duckdb")


# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

# helper function
tester <- function(data = points_sf,
                   name = "test_view",
                   conn = conn_test,
                   overwrite = FALSE,
                   quiet = FALSE) {
    ddbs_register_vector(
        conn,
        data,
        name,
        overwrite,
        quiet
    )
}


# expected behavior --------------------------------------------------------------

test_that("can register sf object as arrow view", {

    # register sf object as view
    result <- tester(
        data = points_sf,
        name = "points_view"
    )

    expect_true(result)

    # check that view exists in arrow views
    arrow_views <- duckdb::duckdb_list_arrow(conn_test)
    expect_true("points_view" %in% arrow_views)

})

test_that("can read registered view back with ddbs_read_vector", {

    # register sf object as view
    ddbs_register_vector(conn_test, points_sf, "points_view2", overwrite = TRUE)

    # read back
    result <- ddbs_read_vector(conn_test, "points_view2", crs = 4326)
  

    # check that result is sf object
    expect_true(inherits(result, "sf"))

    # check that number of rows matches
    expect_equal(nrow(result), nrow(points_sf))

    # check that CRS matches
    expect_equal(sf::st_crs(result), sf::st_crs(points_sf))

})

test_that("crs_duckspatial column is created with SRID", {

    # register sf object with known EPSG CRS
    ddbs_register_vector(conn_test, points_sf, "points_crs_test", overwrite = TRUE)

    # query the crs_duckspatial column
    crs_result <- DBI::dbGetQuery(
        conn_test,
        "SELECT DISTINCT crs_duckspatial FROM points_crs_test"
    )

    # check that crs_duckspatial contains SRID
    expect_true(grepl("EPSG:", crs_result$crs_duckspatial[1]))

})

test_that("overwrite=TRUE replaces existing view", {

    # register first view
    ddbs_register_vector(conn_test, points_sf, "overwrite_test", overwrite = FALSE)

    # try to register again without overwrite - should error
    expect_error(
        ddbs_register_vector(conn_test, countries_sf, "overwrite_test", overwrite = FALSE)
    )

    # register again with overwrite - should succeed
    result <- ddbs_register_vector(conn_test, countries_sf, "overwrite_test", overwrite = TRUE)
    expect_true(result)

    # check that the view now contains countries data
    count_result <- DBI::dbGetQuery(conn_test, "SELECT COUNT(*) as n FROM overwrite_test")
    expect_equal(count_result$n, nrow(countries_sf))

})

test_that("can register sf object from file path", {

    # get path to test file
    file_path <- system.file("spatial/countries.geojson", package = "duckspatial")

    # register from file path
    result <- ddbs_register_vector(conn_test, file_path, "countries_from_file", overwrite = TRUE)

    expect_true(result)

    # check that view exists
    arrow_views <- duckdb::duckdb_list_arrow(conn_test)
    expect_true("countries_from_file" %in% arrow_views)

})

test_that("registered view contains geometry column", {

    # register sf object
    ddbs_register_vector(conn_test, points_sf, "geom_test", overwrite = TRUE)

    # check columns
    columns <- DBI::dbListFields(conn_test, "geom_test")

    expect_true("geometry" %in% columns)
    expect_true("crs_duckspatial" %in% columns)

})

test_that("registered view contains CRS column or has CRS (in duckdb 1.5+)", {
    # register sf object
    ddbs_register_vector(conn_test, points_sf, "crs_test", overwrite = TRUE)

    # check columns
    columns <- DBI::dbListFields(conn_test, "crs_test")

    expect_true("crs_duckspatial" %in% columns)
})

# expected errors --------------------------------------------------------------

test_that("error when view name exists and overwrite=FALSE", {

    # register first view
    ddbs_register_vector(conn_test, points_sf, "duplicate_test", overwrite = TRUE)

    # try to register again - should error
    expect_error(
        ddbs_register_vector(conn_test, points_sf, "duplicate_test", overwrite = FALSE),
        "overwrite = TRUE"
    )

})

# New tests for duckspatial_df and existing crs_duckspatial column ----

test_that("can register duckspatial_df directly", {
    # Create a duckspatial_df by reading from existing view
    ddbs_register_vector(conn_test, points_sf, "points_for_lazy", overwrite = TRUE)
    df_lazy <- ddbs_read_vector(conn_test, "points_for_lazy", crs = 4326) |>
        as_duckspatial_df()

    # Register duckspatial_df as new view
    result <- ddbs_register_vector(conn_test, df_lazy, "lazy_view_direct", overwrite = TRUE)
    expect_true(result)

    # Verify view exists
    arrow_views <- duckdb::duckdb_list_arrow(conn_test)
    expect_true("lazy_view_direct" %in% arrow_views)

    # Verify data is queryable
    count_result <- DBI::dbGetQuery(conn_test, "SELECT COUNT(*) as n FROM lazy_view_direct")
    expect_equal(count_result$n, nrow(points_sf))
})

test_that("handles sf with existing crs_duckspatial column", {
    # Create sf with existing crs_duckspatial
    points_with_crs <- points_sf
    points_with_crs$crs_duckspatial <- "EPSG:4326"

    # Should not error
    result <- ddbs_register_vector(conn_test, points_with_crs, "crs_col_test", overwrite = TRUE)
    expect_true(result)

    # Verify view exists
    arrow_views <- duckdb::duckdb_list_arrow(conn_test)
    expect_true("crs_col_test" %in% arrow_views)

    # Check only one crs_duckspatial column
    columns <- DBI::dbListFields(conn_test, "crs_col_test")
    expect_equal(sum(columns == "crs_duckspatial"), 1)
})

test_that("error for unsupported data types", {
    expect_error(
        ddbs_register_vector(conn_test, list(a = 1), "bad_input"),
        "must be an"
    )

    expect_error(
        ddbs_register_vector(conn_test, 123, "bad_input"),
        "must be an"
    )
})

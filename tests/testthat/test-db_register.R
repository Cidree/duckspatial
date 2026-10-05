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
    ddbs_register_table(
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

    # check that view exists in arrow views (under hidden raw prefix)
    arrow_views <- duckdb::duckdb_list_arrow(conn_test)
    expect_true("__raw_points_view" %in% arrow_views)

})

test_that("can read registered view back with ddbs_read_table", {

    # register sf object as view
    ddbs_register_table(conn_test, points_sf, "points_view2", overwrite = TRUE)

    # read back
    result <- ddbs_read_table(conn_test, "points_view2")
  

    # check that result is sf object
    expect_true(inherits(result, "sf"))

    # check that number of rows matches
    expect_equal(nrow(result), nrow(points_sf))

    # check that CRS matches
    expect_equal(sf::st_crs(result), sf::st_crs(points_sf))

})


test_that("overwrite=TRUE replaces existing view", {

    # register first view
    ddbs_register_table(conn_test, points_sf, "overwrite_test", overwrite = FALSE)

    # try to register again without overwrite - should error
    expect_error(
        ddbs_register_table(conn_test, countries_sf, "overwrite_test", overwrite = FALSE)
    )

    # register again with overwrite - should succeed
    result <- ddbs_register_table(conn_test, countries_sf, "overwrite_test", overwrite = TRUE)
    expect_true(result)

    # check that the view now contains countries data
    count_result <- DBI::dbGetQuery(conn_test, "SELECT COUNT(*) as n FROM overwrite_test")
    expect_equal(count_result$n, nrow(countries_sf))

})

test_that("can register sf object from file path", {

    # get path to test file
    file_path <- system.file("spatial/countries.geojson", package = "duckspatial")

    # register from file path
    result <- ddbs_register_table(conn_test, file_path, "countries_from_file", overwrite = TRUE)

    expect_true(result)

    # check that view exists
    arrow_views <- duckdb::duckdb_list_arrow(conn_test)
    expect_true("__raw_countries_from_file" %in% arrow_views)

})

# test_that("registered view contains geometry column", {

#     # register sf object
#     ddbs_register_table(conn_test, points_sf, "geom_test", overwrite = TRUE)

#     # check columns
#     columns <- DBI::dbListFields(conn_test, "geom_test")

#     expect_true("geometry" %in% columns)

# })

test_that("registered view contains CRS column or has CRS (in duckdb 1.5+)", {
    # register sf object
    ddbs_register_table(conn_test, points_sf, "crs_test", overwrite = TRUE)

    # check that the requested view is queryable
    count_result <- DBI::dbGetQuery(conn_test, "SELECT COUNT(*) as n FROM crs_test")
    expect_equal(count_result$n, nrow(points_sf))

    # check that the exposed view has a CRS-bearing geometry type
    desc <- DBI::dbGetQuery(conn_test, "DESCRIBE crs_test")
    geom_type <- desc$column_type[desc$column_name == attr(points_sf, "sf_column")]
    expect_equal(geom_type, "GEOMETRY('EPSG:4326')")

    duckdb_crs <- DBI::dbGetQuery(
        conn_test,
        "SELECT ST_CRS(geometry) AS crs FROM crs_test LIMIT 1"
    )
    expect_equal(duckdb_crs$crs, "EPSG:4326")

    crs_test <- ddbs_crs(conn_test, "crs_test")

    expect_s3_class(crs_test, "crs")
})

test_that("registered sf object without CRS exposes generic geometry", {
    points_no_crs <- points_sf
    sf::st_crs(points_no_crs) <- NA

    ddbs_register_table(conn_test, points_no_crs, "no_crs_test", overwrite = TRUE)

    count_result <- DBI::dbGetQuery(conn_test, "SELECT COUNT(*) as n FROM no_crs_test")
    expect_equal(count_result$n, nrow(points_no_crs))

    desc <- DBI::dbGetQuery(conn_test, "DESCRIBE no_crs_test")
    geom_type <- desc$column_type[desc$column_name == attr(points_no_crs, "sf_column")]
    expect_equal(geom_type, "GEOMETRY")
})

test_that("registered sf object with non-EPSG CRS keeps queryable DuckDB CRS", {
    custom_crs <- "+proj=longlat +a=6378137 +b=6378137 +no_defs"
    custom_sf <- sf::st_as_sf(
        data.frame(id = 1, x = 0, y = 0),
        coords = c("x", "y"),
        crs = custom_crs
    )

    expect_true(is.na(sf::st_crs(custom_sf)$epsg))

    ddbs_register_table(conn_test, custom_sf, "custom_crs_test", overwrite = TRUE)

    duckdb_crs <- DBI::dbGetQuery(
        conn_test,
        "SELECT ST_CRS(geometry) AS crs FROM custom_crs_test LIMIT 1"
    )
    expect_false(is.na(duckdb_crs$crs[[1]]))
    expect_s3_class(sf::st_crs(duckdb_crs$crs[[1]]), "crs")
})

# expected errors --------------------------------------------------------------

test_that("error when view name exists and overwrite=FALSE", {

    # register first view
    ddbs_register_table(conn_test, points_sf, "duplicate_test", overwrite = TRUE)

    # try to register again - should error
    expect_error(
        ddbs_register_table(conn_test, points_sf, "duplicate_test", overwrite = FALSE),
        "overwrite = TRUE"
    )

})

# New tests for duckspatial_df and existing crs_duckspatial column ----

test_that("can register duckspatial_df directly", {
    # Create a duckspatial_df by reading from existing view
    ddbs_register_table(conn_test, points_sf, "points_for_lazy", overwrite = TRUE)
    df_lazy <- ddbs_read_table(conn_test, "points_for_lazy") |>
        as_duckspatial_df()

    # Register duckspatial_df as new view
    result <- ddbs_register_table(conn_test, df_lazy, "lazy_view_direct", overwrite = TRUE)
    expect_true(result)

    # Verify view exists
    arrow_views <- duckdb::duckdb_list_arrow(conn_test)
    expect_true("__raw_lazy_view_direct" %in% arrow_views)

    # Verify data is queryable
    count_result <- DBI::dbGetQuery(conn_test, "SELECT COUNT(*) as n FROM lazy_view_direct")
    expect_equal(count_result$n, nrow(points_sf))
})


test_that("error for unsupported data types", {
    expect_error(
        ddbs_register_table(conn_test, list(a = 1), "bad_input"),
        "must be an"
    )

    expect_error(
        ddbs_register_table(conn_test, 123, "bad_input"),
        "must be an"
    )
})


# edge cases: chunking ------------------------------------------------------

test_that("a 0-row sf can be registered and keeps columns and CRS", {

    nc_empty <- nc_sf[0, ]

    expect_true(ddbs_register_table(conn_test, nc_empty, "empty_view", overwrite = TRUE, quiet = TRUE))

    cols <- DBI::dbGetQuery(conn_test, "DESCRIBE empty_view")
    expect_equal(cols$column_name, c(setdiff(names(nc_sf), "geometry"), "geometry"))
    expect_equal(cols$column_type[cols$column_name == "geometry"], "GEOMETRY('EPSG:4267')")
    expect_equal(DBI::dbGetQuery(conn_test, "SELECT COUNT(*) AS n FROM empty_view")$n, 0)

    # as_duckspatial_df() goes through the same code
    empty_ddbs <- as_duckspatial_df(nc_empty)
    expect_s3_class(empty_ddbs, "duckspatial_df")
    result <- ddbs_collect(empty_ddbs)
    expect_s3_class(result, "sf")
    expect_equal(nrow(result), 0L)
    expect_equal(names(result), names(nc_sf))
    expect_equal(sf::st_crs(result), sf::st_crs(nc_sf))
})

test_that("data split over several Arrow chunks gives the same result as one chunk", {

    set.seed(1)
    pts <- data.frame(id = 1:2500, x = runif(2500), y = runif(2500)) |>
        sf::st_as_sf(coords = c("x", "y"), crs = 4326)

    one_chunk <- as_duckspatial_df(pts) |> ddbs_collect()

    # force chunk_size to its 1000-row minimum -> 3 chunks
    local_mocked_bindings(register_chunk_target_bytes = function() 1)
    ddbs_register_table(conn_test, pts, "chunked_view", overwrite = TRUE, quiet = TRUE)

    # the view can be scanned more than once (a RecordBatchReader is single-use)
    count_sql <- "SELECT COUNT(*) AS n FROM chunked_view"
    expect_equal(DBI::dbGetQuery(conn_test, count_sql)$n, 2500)
    expect_equal(DBI::dbGetQuery(conn_test, count_sql)$n, 2500)

    result <- ddbs_read_table(conn_test, "chunked_view")

    expect_equal(nrow(result), 2500L)
    expect_equal(result$id, pts$id)
    expect_equal(sf::st_crs(result), sf::st_crs(pts))
    expect_equal(sf::st_coordinates(result), sf::st_coordinates(one_chunk))
})

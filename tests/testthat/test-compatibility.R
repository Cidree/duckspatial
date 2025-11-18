test_that("Compatibility: Arrow Views behave like Persistent Tables", {
    skip_if_not_installed("duckdb")

    # Setup
    conn <- ddbs_create_conn("memory")
    on.exit(ddbs_stop_conn(conn), add = TRUE)

    # Create test data with a NON-STANDARD geometry column name
    # This tests if register/read respects column naming
    data_sf <- sf::st_as_sf(
        data.frame(id = 1:5, x = 0, y = 0, val = letters[1:5]),
        coords = c("x", "y"),
        crs = 4326
    )
    sf::st_geometry(data_sf) <- "my_custom_geom"

    # 1. Register as Arrow View
    expect_no_error(
        ddbs_register_vector(conn, data_sf, "view_test", overwrite = TRUE)
    )

    # 2. Test ddbs_crs on View
    expect_no_error(crs_out <- ddbs_crs(conn, "view_test"))
    expect_equal(crs_out, sf::st_crs(4326))

    # 3. Test ddbs_read_vector on View
    # Should handle WKB conversion automatically and preserve "my_custom_geom"
    read_view <- ddbs_read_vector(conn, "view_test", crs = 4326)

    expect_s3_class(read_view, "sf")
    expect_equal(attr(read_view, "sf_column"), "my_custom_geom")
    expect_equal(nrow(read_view), 5)

    # 4. Verify Data Integrity vs Persistent Table
    ddbs_write_vector(conn, data_sf, "table_test", overwrite = TRUE)
    read_table <- ddbs_read_vector(conn, "table_test", crs = 4326)

    # Compare View result vs Table result
    # (Ignore attribute order if necessary, but data should match)
    expect_equal(sf::st_drop_geometry(read_view), sf::st_drop_geometry(read_table))
    expect_equal(sf::st_geometry(read_view), sf::st_geometry(read_table))
})

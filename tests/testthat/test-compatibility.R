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
    # sf::st_geometry(data_sf) <- "my_custom_geom"

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
    # expect_equal(attr(read_view, "sf_column"), "my_custom_geom")
    expect_equal(nrow(read_view), 5)

    # 4. Verify Data Integrity vs Persistent Table
    ddbs_write_vector(conn, data_sf, "table_test", overwrite = TRUE)
    read_table <- ddbs_read_vector(conn, "table_test", crs = 4326)

    # Compare View result vs Table result
    # (Ignore attribute order if necessary, but data should match)
    expect_equal(sf::st_drop_geometry(read_view), sf::st_drop_geometry(read_table))
    expect_equal(sf::st_geometry(read_view), sf::st_geometry(read_table))
})


test_that("Round trip: write -> read for various geometry types", {
    skip_if_not_installed("duckdb")
    conn <- ddbs_create_conn("memory")
    on.exit(ddbs_stop_conn(conn), add = TRUE)

    # Test data
    line <- sf::st_as_sfc("LINESTRING(0 0, 1 1)") |> sf::st_sf(id = 1, geom = _, crs = 4326)
    polygon <- sf::st_as_sfc("POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))") |> sf::st_sf(id = 1, geom = _, crs = 4326)
    multipoint <- sf::st_as_sfc("MULTIPOINT(0 0, 1 1)") |> sf::st_sf(id = 1, geom = _, crs = 4326)

    datasets <- list(line = line, polygon = polygon, multipoint = multipoint)

    for (name in names(datasets)) {
        data <- datasets[[name]]
        table_name <- paste0("rt_write_", name)

        # Write -> Read
        ddbs_write_vector(conn, data, table_name, overwrite = TRUE)
        result <- ddbs_read_vector(conn, table_name)

        # Verification
        expect_s3_class(result, "sf")
        expect_equal(nrow(result), 1)
        expect_equal(sf::st_crs(result), sf::st_crs(data))
        # use all.equal for comparing geometries to handle potential precision issues
        expect_true(all.equal(sf::st_geometry(result), sf::st_geometry(data), check.attributes = FALSE))
    }
})

test_that("Round trip: register -> read for various geometry types", {
    skip_if_not_installed("duckdb")
    conn <- ddbs_create_conn("memory")
    on.exit(ddbs_stop_conn(conn), add = TRUE)

    # Test data
    line <- sf::st_as_sfc("LINESTRING(0 0, 1 1)") |> sf::st_sf(id = 1, geom = _, crs = 4326)
    polygon <- sf::st_as_sfc("POLYGON((0 0, 1 0, 1 1, 0 1, 0 0))") |> sf::st_sf(id = 1, geom = _, crs = 4326)
    multipoint <- sf::st_as_sfc("MULTIPOINT(0 0, 1 1)") |> sf::st_sf(id = 1, geom = _, crs = 4326)

    datasets <- list(line = line, polygon = polygon, multipoint = multipoint)

    for (name in names(datasets)) {
        data <- datasets[[name]]
        view_name <- paste0("rt_register_", name)

        # Register -> Read
        ddbs_register_vector(conn, data, view_name, overwrite = TRUE)
        result <- ddbs_read_vector(conn, view_name)

        # Verification
        expect_s3_class(result, "sf")
        expect_equal(nrow(result), 1)
        expect_equal(sf::st_crs(result), sf::st_crs(data))
        expect_true(all.equal(sf::st_geometry(result), sf::st_geometry(data), check.attributes = FALSE))
    }
})

test_that("Compatibility: Writing from file path and reading back", {
    skip_if_not_installed("duckdb")
    conn <- ddbs_create_conn("memory")
    on.exit(ddbs_stop_conn(conn), add = TRUE)

    file_path <- system.file("spatial/countries.geojson", package = "duckspatial")
    table_name <- "countries_from_file_compat"

    # Write from file, then read back
    ddbs_write_vector(conn, file_path, table_name, overwrite = TRUE)
    read_data <- ddbs_read_vector(conn, table_name)

    # Read the original file directly with sf for comparison
    original_sf <- sf::st_read(file_path, quiet = TRUE)

    # Basic checks
    expect_s3_class(read_data, "sf")
    expect_equal(nrow(read_data), nrow(original_sf))

    # CRS check (read_data will have CRS)
    expect_equal(sf::st_crs(read_data)$srid, sf::st_crs(original_sf)$srid)

    expect_true("NAME_ENGL" %in% names(read_data))
    expect_true(all.equal(
        sf::st_geometry(read_data),
        sf::st_geometry(original_sf),
        check.attributes = FALSE
    ))
})

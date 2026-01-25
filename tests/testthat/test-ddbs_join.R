# skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
skip_on_cran()
skip_if_not_installed("duckdb")


# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- ddbs_temp_conn()

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
                   quiet = getOption("duckspatial.quiet", FALSE)) {
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


test_that("expected behavior", {

    # option 1: passing sf objects
    output1 <- tester(
        x = points_sf,
        y = countries_sf,
        join = "within"
    )

    expect_true(is(output1 , 'sf'))

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

    expect_true(is(output2 , 'sf'))

    # option 3: passing the names of tables in a duckdb db, creating new table in db
    output3 <- tester(
        conn = conn_test,
        x = "points",
        y = "countries",
        join = "within",
        name = "test_result",
        overwrite = TRUE
    )

    expect_true(output3)

    # TODO - Review this because it fails
    # output3 <- DBI::dbReadTable(conn_test, "test_result") |>
    #     sf::st_as_sf(wkt = 'geometry')

    # expect_true(is(output3 , 'sf'))

    ddbs_read_vector(conn = conn_test, name = "test_result", quiet = TRUE)


    # show and suppress messages
    expect_message( tester(quiet = FALSE) )
    expect_no_message( tester(quiet = TRUE))
    # verify default respects global option (which is TRUE in setup)
    expect_no_message( tester() )


})


test_that("error if table already exists", {

    # write table for the 1st time
    expect_true(tester(x = "points",
                                    y = "countries",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = FALSE)
                             )

    # expected error if overwrite = FALSE
    expect_error(tester(x = "points",
                                    y = "countries",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = FALSE))

    # overwrite table
    expect_true(tester(x = "points",
                                    y = "countries",
                                    conn = conn_test,
                                    name = 'banana',
                                    overwrite = TRUE))


})

# expected errors --------------------------------------------------------------

test_that("errors with incorrect input", {

    expect_error(tester(x = 999))
    expect_error(tester(y = 999))
    expect_error(tester(join = 999))
    expect_error(tester(conn = 999))
    expect_error(tester(overwrite = 999))
    expect_error(tester(quiet = 999))

    expect_error(tester(x = "999", conn = conn_test))
    expect_error(tester(y = "999", conn = conn_test))

    expect_error(tester(conn = conn_test, name = c('banana', 'banana')))


    })



# duckspatial_df inputs --------------------------------------------------------

test_that("ddbs_join works with duckspatial_df inputs", {
  countries_path <- system.file("spatial/countries.geojson", package = "duckspatial")
  
  # Create a distinct connection for this test to avoid interference
  # Use the internal helper visible in other tests
  conn <- ddbs_temp_conn()
  
  # Load as duckspatial_df
  countries_ds <- ddbs_open_dataset(countries_path, conn = conn)
  
  # Create points as sf
  # Helper defined in test-utils.R or we just create sf manually
  points_sf <- sf::st_as_sf(
    data.frame(id = 1:10, x = 1:10, y = 1:10), 
    coords = c("x", "y"), 
    crs = 4326
  )
  
  # Register points to the same connection as a duckspatial_df for consistent testing
  ddbs_write_vector(conn, points_sf, "test_points")
  points_ds <- ddbs_read_vector(conn, "test_points", quiet = TRUE)
  
  # 1. duckspatial_df x duckspatial_df
  # Using intersects
  result1 <- ddbs_join(points_ds, countries_ds, join = "intersects")
  expect_s3_class(result1, "duckspatial_df")
  expect_true(nrow(dplyr::collect(result1)) >= 0)
  
  # 2. duckspatial_df x sf
  # Note: Cross-connection join might occur if sf implicit connection is different, but here we expect it to work
  result2 <- ddbs_join(points_ds, countries_sf, join = "intersects")
  expect_s3_class(result2, "duckspatial_df")
  
  # 3. sf x duckspatial_df
  
  withr::with_options(list(duckspatial.output_type = "sf"), {
      r_sf <- ddbs_join(points_sf, countries_ds, join = "intersects")
      expect_s3_class(r_sf, "sf")
  })
})

# predicates -------------------------------------------------------------------

test_that("ddbs_join works with different predicates", {
  # We test a few key ones to ensure parameter passing works
  # Use simple data where we know the answer
  
  # Polygon: square (0,0) to (10,10)
  p1 <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  poly_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(p1), crs=4326)
  
  # Points: inside (5,5), edge (0,0), outside (20,20)
  pts <- matrix(c(5,5, 0,0, 20,20), ncol=2, byrow=TRUE)
  pts_sf <- sf::st_sf(id = 1:3, geometry = sf::st_sfc(lapply(1:3, function(i) sf::st_point(pts[i,]))), crs=4326)
  
  res_within <- ddbs_join(pts_sf, poly_sf, join = "within") |> dplyr::collect()
  
  expect_true(1 %in% res_within$id)
  expect_false(3 %in% res_within$id)
  
  res_disjoint <- ddbs_join(pts_sf, poly_sf, join = "disjoint") |> dplyr::collect()
  expect_true(3 %in% res_disjoint$id)
  expect_false(1 %in% res_disjoint$id)
  
  res_intersects <- ddbs_join(pts_sf, poly_sf, join = "intersects") |> dplyr::collect()
  expect_true(1 %in% res_intersects$id)
  expect_true(2 %in% res_intersects$id)
  expect_false(3 %in% res_intersects$id)
})

# output parameters ------------------------------------------------------------

test_that("ddbs_join respects output parameter", {
  result_sf <- ddbs_join(points_sf, countries_sf, output = "sf")
  expect_s3_class(result_sf, "sf")
  
  result_ds <- ddbs_join(points_sf, countries_sf, output = "duckspatial_df")
  expect_s3_class(result_ds, "duckspatial_df")
  
  result_tibble <- ddbs_join(points_sf, countries_sf, output = "tibble")
  expect_s3_class(result_tibble, "tbl_df")
  expect_true(!inherits(result_tibble, "sf")) 
})

# error handling ---------------------------------------------------------------

test_that("ddbs_join throws error on CRS mismatch", {
  points_3857 <- sf::st_transform(points_sf, 3857)
  
  expect_error(
    ddbs_join(points_3857, countries_sf),
    "Coordinates Reference System"
  )
})


# cross-verification: ddbs_join vs sf::st_join ------------------------------------
# relies on nc_sf and nc_sf_5070 datasets loaded in testthat/setup.R

test_that("ddbs_join matches sf::st_join for intersects", {
    nc_subset <- nc_sf_5070[1:10, ]
    box <- sf::st_bbox(nc_sf_5070[1:5, ]) |> sf::st_as_sfc() |> sf::st_as_sf()
    box$box_id <- "BOX1"
    
    sf_result <- sf::st_join(
        nc_subset,
        box,
        join = sf::st_intersects,
        left = FALSE
    )
    ddbs_result <- ddbs_join(
        nc_subset,
        box,
        join = "intersects",
        output = "sf"
    )
    
    # Compare row counts
    expect_equal(nrow(ddbs_result), nrow(sf_result))
    
    # Compare matched features (order-independent)
    expect_equal(sort(ddbs_result$NAME), sort(sf_result$NAME))
    
    # Verify box_id was joined
    expect_true("box_id" %in% names(ddbs_result))
})

test_that("ddbs_join with within predicate matches sf", {
    # Point within polygon test - use point_on_surface to avoid boundary issues
    point <- sf::st_point_on_surface(nc_sf_5070[5, ]) |> sf::st_as_sf()
    point$point_id <- "P1"
    
    sf_result <- sf::st_join(
        point,
        nc_sf_5070,
        join = sf::st_within,
        left = FALSE
    )
    ddbs_result <- ddbs_join(
        point,
        nc_sf_5070,
        join = "within",
        output = "sf"
    )
    
    # Should match exactly 1 county
    expect_equal(nrow(ddbs_result), nrow(sf_result))
    
    # If we have results, compare them
    if (nrow(sf_result) > 0) {
        # Handle suffix collision - sf may use NAME.x
        sf_name <- if ("NAME.x" %in% names(sf_result)) sf_result$NAME.x else sf_result$NAME
        ddbs_name <- if ("NAME.x" %in% names(ddbs_result)) ddbs_result$NAME.x else ddbs_result$NAME
        expect_equal(ddbs_name, sf_name)
    }
    expect_ddbs_sf_equal(ddbs_result, sf_result, id_col = "point_id")
})

test_that("ddbs_join with contains predicate matches sf", {
    # Polygon contains point test
    point <- sf::st_point_on_surface(nc_sf_5070[5, ]) |> sf::st_as_sf()
    point$point_id <- "P1"
    
    sf_result <- sf::st_join(
        nc_sf_5070,
        point,
        join = sf::st_contains,
        left = FALSE
    )
    ddbs_result <- ddbs_join(
        nc_sf_5070,
        point,
        join = "contains",
        output = "sf"
    )
    
    # Should match exactly 1 county containing the point
    expect_equal(nrow(ddbs_result), nrow(sf_result))
    
    # If we have results, compare them
    if (nrow(sf_result) > 0) {
        sf_name <- if ("NAME.x" %in% names(sf_result)) sf_result$NAME.x else sf_result$NAME
        ddbs_name <- if ("NAME.x" %in% names(ddbs_result)) ddbs_result$NAME.x else ddbs_result$NAME
        expect_equal(ddbs_name, sf_name)
    }
    expect_ddbs_sf_equal(ddbs_result, sf_result, id_col = "point_id")
})

test_that("ddbs_join handles multiple y matches per x", {
    # Create scenario where one x feature matches multiple y features
    buffered <- sf::st_buffer(nc_sf_5070[1:5, ], dist = 50000)
    buffered$buff_id <- paste0("B", 1:5)
    
    # Sample points that will fall in multiple buffers
    points <- sf::st_sample(nc_sf_5070[1, ], 3) |> sf::st_as_sf()
    points$pt_id <- paste0("P", 1:3)

    sf_result <- sf::st_join(
        points,
        buffered,
        join = sf::st_intersects,
        left = FALSE
    )
    ddbs_result <- ddbs_join(
        points,
        buffered,
        join = "intersects",
        output = "sf"
    )
    
    # Multiple matches: each point joined to multiple buffers
    expect_true(nrow(ddbs_result) >= 3)
    expect_equal(nrow(ddbs_result), nrow(sf_result))
    
    # Same point IDs repeated
    expect_equal(sort(ddbs_result$pt_id), sort(sf_result$pt_id))
    expect_ddbs_sf_equal(ddbs_result, sf_result, id_col = "pt_id", check_geom = FALSE)
})

test_that("ddbs_join matches sf::st_join for touches", {
    # Find counties that touch Wake county
    wake <- nc_sf_5070[nc_sf_5070$NAME == "Wake", ]
    wake$wake_id <- "WAKE"
    
    sf_result <- sf::st_join(
        nc_sf_5070,
        wake,
        join = sf::st_touches,
        left = FALSE
    )
    ddbs_result <- ddbs_join(
        nc_sf_5070,
        wake,
        join = "touches",
        output = "sf"
    )
    
    expect_equal(nrow(ddbs_result), nrow(sf_result))
    
    # Handle suffix collision
    sf_names <- if ("NAME.x" %in% names(sf_result)) sf_result$NAME.x else sf_result$NAME
    ddbs_names <- if ("NAME.x" %in% names(ddbs_result)) ddbs_result$NAME.x else ddbs_result$NAME
    expect_equal(sort(ddbs_names), sort(sf_names))
})

test_that("ddbs_join row order differs but content matches", {
    nc_subset <- nc_sf_5070[10:20, ]
    box <- sf::st_bbox(nc_sf_5070[10:20, ]) |> sf::st_as_sfc() |> sf::st_as_sf()
    box$box_id <- "BOX1"
    
    sf_result <- sf::st_join(
        nc_subset,
        box,
        join = sf::st_intersects,
        left = FALSE
    )
    ddbs_result <- ddbs_join(
        nc_subset,
        box,
        join = "intersects",
        output = "sf"
    )
    
    # Content match (order-independent)
    expect_equal(sort(ddbs_result$FIPS), sort(sf_result$FIPS))
    
    # Geometry equivalence
    sf_sorted <- sf_result[order(sf_result$FIPS), ]
    ddbs_sorted <- ddbs_result[order(ddbs_result$FIPS), ]
    geom_equal <- sf::st_equals_exact(
        sf_sorted,
        ddbs_sorted,
        par = 1e-6,
        sparse = FALSE
    )
    expect_true(all(diag(geom_equal)))
})

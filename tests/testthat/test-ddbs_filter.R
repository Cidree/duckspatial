# skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
skip_on_cran()
skip_if_not_installed("duckdb")


# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- ddbs_temp_conn()

# helper function
tester <- function(x = points_sf,
                   y = argentina_sf,
                   predicate = "intersects",
                   conn = NULL,
                   name = NULL,
                   crs = NULL,
                   crs_column = "crs_duckspatial",
                   distance = NULL,
                   output = "sf",
                   overwrite = FALSE,
                   quiet = getOption("duckspatial.quiet", FALSE)) {
    ddbs_filter(
        x = x,
        y = y,
        predicate = predicate,
        conn = conn,
        name = name,
        crs = crs,
        crs_column = crs_column,
        distance = distance,
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
        y = argentina_sf,
        predicate = "intersects"
    )

    expect_true(is(output1 , 'sf'))

    # option 2: passing the names of tables in a duckdb db, returing sf
    # write sf to duckdb
    ddbs_write_vector(conn_test, points_sf, "points", overwrite = TRUE)
    ddbs_write_vector(conn_test, argentina_sf, "argentina", overwrite = TRUE)

    # spatial filter
    output2 <- tester(
        conn = conn_test,
        x = "points",
        y = "argentina",
        predicate = "intersects"
    )

    expect_true(is(output2 , 'sf'))

    # option 3: passing the names of tables in a duckdb db, creating new table in db
    output3 <- tester(
        conn = conn_test,
        x = "points",
        y = "argentina",
        predicate = "intersects",
        name = "filter_result",
        overwrite = TRUE
    )

    expect_true(output3)

    ddbs_read_vector(conn = conn_test, name = "filter_result", quiet = TRUE)


    # show and suppress messages
    expect_message( tester(quiet = FALSE) )
    expect_no_message( tester(quiet = TRUE))
    # verify default respects global option
    expect_no_message( tester() )


})


test_that("error if table already exists", {

    # write table for the 1st time
    expect_true(tester(x = "points",
                                    y = "argentina",
                                    conn = conn_test,
                                    name = 'banana_filter',
                                    overwrite = FALSE)
                             )

    # expected error if overwrite = FALSE
    expect_error(tester(x = "points",
                                    y = "argentina",
                                    conn = conn_test,
                                    name = 'banana_filter',
                                    overwrite = FALSE))

    # overwrite table
    expect_true(tester(x = "points",
                                    y = "argentina",
                                    conn = conn_test,
                                    name = 'banana_filter',
                                    overwrite = TRUE))


})

# expected errors --------------------------------------------------------------

test_that("errors with incorrect input", {

    expect_error(tester(x = 999))
    expect_error(tester(y = 999))
    expect_error(tester(predicate = 999))
    expect_error(tester(conn = 999))
    expect_error(tester(overwrite = 999))
    expect_error(tester(quiet = 999))

    expect_error(tester(x = "999", conn = conn_test))
    expect_error(tester(y = "999", conn = conn_test))

    expect_error(tester(conn = conn_test, name = c('banana', 'banana')))


    })



# duckspatial_df inputs --------------------------------------------------------

test_that("ddbs_filter works with duckspatial_df inputs", {
  argentina_path <- system.file("spatial/argentina.geojson", package = "duckspatial")
  
  # Create a distinct connection for this test to avoid interference
  conn <- ddbs_temp_conn()
  
  # Load as duckspatial_df
  argentina_ds <- ddbs_open_dataset(argentina_path, conn = conn)
  
  # Create points as sf
  points_small_sf <- sf::st_as_sf(
    data.frame(id = 1:5, x = c(-60, -60, 0, 0, 0), y = c(-34, -34, 0, 0, 0)), 
    coords = c("x", "y"), 
    crs = 4326
  )
  
  # Register points to the same connection as a duckspatial_df
  ddbs_write_vector(conn, points_small_sf, "test_points_filter")
  points_ds <- ddbs_read_vector(conn, "test_points_filter", quiet = TRUE)
  
  # 1. duckspatial_df x duckspatial_df
  result1 <- ddbs_filter(points_ds, argentina_ds, predicate = "intersects")
  expect_s3_class(result1, "duckspatial_df")
  expect_true(nrow(dplyr::collect(result1)) >= 0)
  
  # 2. duckspatial_df x sf
  result2 <- ddbs_filter(points_ds, argentina_sf, predicate = "intersects")
  expect_s3_class(result2, "duckspatial_df")
  
  # 3. sf x duckspatial_df
  withr::with_options(list(duckspatial.output_type = "sf"), {
      r_sf <- ddbs_filter(points_small_sf, argentina_ds, predicate = "intersects")
      expect_s3_class(r_sf, "sf")
  })
})

# predicates -------------------------------------------------------------------


test_that("ddbs_filter works with different predicates", {
  # Polygon: square (0,0) to (10,10)
  p1 <- sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE)))
  poly_sf <- sf::st_sf(id = 1, geometry = sf::st_sfc(p1), crs=4326)
  
  # Points: inside (5,5), edge (0,0), outside (20,20)
  pts <- matrix(c(5,5, 0,0, 20,20), ncol=2, byrow=TRUE)
  pts_sf <- sf::st_sf(id = 1:3, geometry = sf::st_sfc(lapply(1:3, function(i) sf::st_point(pts[i,]))), crs=4326)
  
  res_within <- ddbs_filter(pts_sf, poly_sf, predicate = "within") |> dplyr::collect()
  expect_true(1 %in% res_within$id)
  expect_false(3 %in% res_within$id)
  
  res_disjoint <- ddbs_filter(pts_sf, poly_sf, predicate = "disjoint") |> dplyr::collect()
  expect_true(3 %in% res_disjoint$id)
  expect_false(1 %in% res_disjoint$id)
  
  # ST_DWithin
  res_dwithin <- ddbs_filter(pts_sf, poly_sf, predicate = "dwithin", distance = 15) |> dplyr::collect()
  expect_true(3 %in% res_dwithin$id) # 20,20 is ~14.1 units from 10,10
  
  res_dwithin_small <- ddbs_filter(pts_sf, poly_sf, predicate = "dwithin", distance = 5) |> dplyr::collect()
  expect_false(3 %in% res_dwithin_small$id)
})

# output parameters ------------------------------------------------------------

test_that("ddbs_filter respects output parameter", {
  result_sf <- ddbs_filter(points_sf, argentina_sf, output = "sf")
  expect_s3_class(result_sf, "sf")
  
  result_ds <- ddbs_filter(points_sf, argentina_sf, output = "duckspatial_df")
  expect_s3_class(result_ds, "duckspatial_df")
  
  result_tibble <- ddbs_filter(points_sf, argentina_sf, output = "tibble")
  expect_s3_class(result_tibble, "tbl_df")
  expect_true(!inherits(result_tibble, "sf")) 
})

# error handling ---------------------------------------------------------------

test_that("ddbs_filter throws error on CRS mismatch", {
  points_3857 <- sf::st_transform(points_sf[1:10,], 3857)
  
  expect_error(
    ddbs_filter(points_3857, argentina_sf),
    "Coordinates Reference System"
  )
})


# cross-verification: ddbs_filter vs sf::st_filter ------------------------------------
# relies on nc_sf and nc_sf_5070 datasets loaded in testthat/setup.R

test_that("ddbs_filter matches sf::st_filter for intersects", {
    bbox <- sf::st_bbox(nc_sf_5070[1:5, ]) |> sf::st_as_sfc() |> sf::st_as_sf()
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        bbox,
        .predicate = sf::st_intersects
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        bbox,
        predicate = "intersects",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for within", {
    # Interior bbox to avoid boundary edge cases
    central <- nc_sf_5070[nc_sf_5070$NAME %in% c("Wake", "Durham", "Orange", "Chatham"), ]
    bbox_proj <- sf::st_bbox(central) |>
        sf::st_as_sfc() |>
        sf::st_buffer(20000)
    bbox <- sf::st_as_sf(bbox_proj)
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        bbox,
        .predicate = sf::st_within
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        bbox,
        predicate = "within",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter dwithin matches sf::st_is_within_distance", {
    centroid <- sf::st_centroid(sf::st_union(nc_sf_5070))
    point_sf <- sf::st_as_sf(centroid)
    distance_m <- 100000
    
    within_dist <- sf::st_is_within_distance(nc_sf_5070, point_sf, dist = distance_m)
    sf_result <- nc_sf_5070[lengths(within_dist) > 0, ]
    
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        point_sf,
        predicate = "dwithin",
        distance = distance_m,
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for contains", {
    # Use a larger polygon to contain smaller ones
    large_counties <- nc_sf_5070[nc_sf_5070$AREA > 0.2, ]
    container <- sf::st_union(large_counties) |> sf::st_as_sf()
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        container,
        .predicate = sf::st_contains
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        container,
        predicate = "contains",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for touches", {
    # Find counties that touch Wake county
    wake <- nc_sf_5070[nc_sf_5070$NAME == "Wake", ]
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        wake,
        .predicate = sf::st_touches
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        wake,
        predicate = "touches",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for overlaps", {
    # Create overlapping polygons by buffering
    buffered <- sf::st_buffer(nc_sf_5070[1:5, ], dist = 10000)
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        buffered,
        .predicate = sf::st_overlaps
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        buffered,
        predicate = "overlaps",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for crosses", {
    # Create a line crossing the state
    bbox <- sf::st_bbox(nc_sf_5070)
    line <- sf::st_linestring(matrix(
        c(
            bbox["xmin"], (bbox["ymin"] + bbox["ymax"]) / 2,
            bbox["xmax"], (bbox["ymin"] + bbox["ymax"]) / 2
        ),
        ncol = 2, byrow = TRUE
    ))
    line_sf <- sf::st_sfc(line, crs = sf::st_crs(nc_sf_5070)) |> sf::st_as_sf()
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        line_sf,
        .predicate = sf::st_crosses
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        line_sf,
        predicate = "crosses",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for covers", {
    # Use bbox that covers some counties
    bbox <- sf::st_bbox(nc_sf_5070[1:10, ]) |> sf::st_as_sfc() |> sf::st_as_sf()
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        bbox,
        .predicate = sf::st_covers
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        bbox,
        predicate = "covers",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for covered_by", {
    # Find counties covered by a large polygon
    large_area <- sf::st_union(nc_sf_5070[1:20, ]) |>
        sf::st_buffer(dist = 5000) |>
        sf::st_as_sf()
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        large_area,
        .predicate = sf::st_covered_by
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        large_area,
        predicate = "covered_by",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for disjoint", {
    # Find counties disjoint from a specific region
    western_counties <- nc_sf_5070[nc_sf_5070$NAME %in% c("Mecklenburg", "Gaston"), ]
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        western_counties,
        .predicate = sf::st_disjoint
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        western_counties,
        predicate = "disjoint",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for equals", {
    # Test with identical geometries
    test_counties <- nc_sf_5070[1:5, ]
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        test_counties,
        .predicate = sf::st_equals
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        test_counties,
        predicate = "equals",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

test_that("ddbs_filter matches sf::st_filter for contains_properly", {
    # Buffer to ensure strict containment (no shared boundaries)
    # Already using 5070, so simplified transformation logic
    container <- sf::st_union(nc_sf_5070[nc_sf_5070$AREA > 0.2, ]) |>
        sf::st_buffer(dist = 1000) |>
        sf::st_as_sf()
    
    sf_result <- sf::st_filter(
        nc_sf_5070,
        container,
        .predicate = sf::st_contains_properly
    )
    ddbs_result <- ddbs_filter(
        nc_sf_5070,
        container,
        predicate = "contains_properly",
        output = "sf"
    )
    
    expect_ddbs_sf_equal(ddbs_result, sf_result)
})

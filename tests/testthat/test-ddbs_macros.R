

countries_ddbs
rivers_ddbs
points_ddbs

# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")


# 1. Geometry validation functions ---------------------------------------

testthat::describe("Geometry validation functions", {

  ## DDBS_IS_EMPTY
  testthat::it("ddbs_is_empty() macro works", {
    normal_result <- ddbs_is_empty(countries_ddbs) |> ddbs_collect()
    macro_result  <- countries_ddbs |>
      dplyr::mutate(is_empty = ddbs_is_empty(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$is_empty, macro_result$is_empty)
  })

  ## DDBS_IS_VALID
  testthat::it("ddbs_is_valid() macro works", {
    normal_result <- ddbs_is_valid(countries_ddbs) |> ddbs_collect()
    macro_result  <- countries_ddbs |>
      dplyr::mutate(is_valid = ddbs_is_valid(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$is_valid, macro_result$is_valid)
  })

  ## DDBS_IS_SIMPLE
  testthat::it("ddbs_is_simple() macro works", {
    normal_result <- ddbs_is_simple(countries_ddbs) |> ddbs_collect()
    macro_result  <- countries_ddbs |>
      dplyr::mutate(is_simple = ddbs_is_simple(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$is_simple, macro_result$is_simple)
  })

  ## DDBS_IS_CLOSED
  testthat::it("ddbs_is_closed() macro works on lines", {
    normal_result <- ddbs_is_closed(rivers_ddbs) |> ddbs_collect()
    macro_result  <- rivers_ddbs |>
      dplyr::mutate(is_closed = ddbs_is_closed(geom)) |>
      ddbs_collect()
    expect_equal(normal_result$is_closed, macro_result$is_closed)
  })

  ## DDBS_IS_RING
  testthat::it("ddbs_is_ring() macro works on lines", {
    normal_result <- ddbs_is_ring(rivers_ddbs) |> ddbs_collect()
    macro_result  <- rivers_ddbs |>
      dplyr::mutate(is_ring = ddbs_is_ring(geom)) |>
      ddbs_collect()
    expect_equal(normal_result$is_ring, macro_result$is_ring)
  })

  ## DDBS_GEOMETRY_TYPE
  testthat::it("ddbs_geometry_type() macro works", {
    normal_result <- ddbs_geometry_type(countries_ddbs)
    macro_result  <- countries_ddbs |>
      dplyr::mutate(geometry_type = ddbs_geometry_type(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result, macro_result$geometry_type)
  })

})


# 2. Measure functions ---------------------------------------------------

testthat::describe("Measure functions", {

  ## DDBS_AREA
  testthat::it("ddbs_area() macro works on polygons", {
    normal_result <- ddbs_area(countries_ddbs) |> ddbs_collect()
    macro_result  <- countries_ddbs |>
      dplyr::mutate(area = ddbs_area(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$area, macro_result$area)
  })

  ## DDBS_LENGTH
  testthat::it("ddbs_length() macro works on lines", {
    normal_result <- ddbs_length(rivers_ddbs) |> ddbs_collect()
    macro_result  <- rivers_ddbs |>
      dplyr::mutate(length = ddbs_length(geom)) |>
      ddbs_collect()
    expect_equal(normal_result$length, macro_result$length)
  })

  ## DDBS_PERIMETER
  testthat::it("ddbs_perimeter() macro works on polygons", {
    normal_result <- ddbs_perimeter(countries_ddbs) |> ddbs_collect()
    macro_result  <- countries_ddbs |>
      dplyr::mutate(perimeter = ddbs_perimeter(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$perimeter, macro_result$perimeter)
  })

})


# 3. Coordinate operations -----------------------------------------------

testthat::describe("Coordinate operations", {

  ## DDBS_X
  testthat::it("ddbs_x() macro works on points", {
    normal_result <- ddbs_x(points_ddbs) |> ddbs_collect()
    macro_result  <- points_ddbs |>
      dplyr::mutate(x = ddbs_x(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$X, macro_result$x)
  })

  ## DDBS_Y
  testthat::it("ddbs_y() macro works on points", {
    normal_result <- ddbs_y(points_ddbs) |> ddbs_collect()
    macro_result  <- points_ddbs |>
      dplyr::mutate(y = ddbs_y(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$Y, macro_result$y)
  })

  ## DDBS_Z
  testthat::it("ddbs_z() macro works on points", {
    normal_result <- ddbs_z(points_ddbs) |> ddbs_collect()
    macro_result  <- points_ddbs |>
      dplyr::mutate(z = ddbs_z(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$Z, macro_result$z)
  })

  ## DDBS_M
  testthat::it("ddbs_m() macro works on points", {
    normal_result <- ddbs_m(points_ddbs) |> ddbs_collect()
    macro_result  <- points_ddbs |>
      dplyr::mutate(m = ddbs_m(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$M, macro_result$m)
  })

})


# 4. Dimension operations ------------------------------------------------

testthat::describe("Dimension operations", {

  ## DDBS_HAS_Z
  testthat::it("ddbs_has_z() macro works", {
    normal_result <- ddbs_has_z(points_ddbs) |> ddbs_collect()
    macro_result  <- points_ddbs |>
      dplyr::mutate(has_z = ddbs_has_z(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$has_z, macro_result$has_z)
  })

  ## DDBS_HAS_M
  testthat::it("ddbs_has_m() macro works", {
    normal_result <- ddbs_has_m(points_ddbs) |> ddbs_collect()
    macro_result  <- points_ddbs |>
      dplyr::mutate(has_m = ddbs_has_m(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$has_m, macro_result$has_m)
  })

})


# 5. Format conversion ---------------------------------------------------

testthat::describe("Format conversion functions", {

  ## DDBS_AS_TEXT
  testthat::it("ddbs_as_text() macro works", {
    normal_result <- ddbs_as_text(countries_ddbs)
    macro_result  <- countries_ddbs |>
      dplyr::mutate(wkt = ddbs_as_text(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result, macro_result$wkt)
  })

  ## DDBS_AS_WKB
  testthat::it("ddbs_as_wkb() macro works", {
    normal_result <- ddbs_as_wkb(countries_ddbs)
    macro_result  <- countries_ddbs |>
      dplyr::mutate(wkb = ddbs_as_wkb(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result, macro_result$wkb)
  })

  ## DDBS_AS_HEXWKB
  testthat::it("ddbs_as_hexwkb() macro works", {
    normal_result <- ddbs_as_hexwkb(countries_ddbs)
    macro_result  <- countries_ddbs |>
      dplyr::mutate(hexwkb = ddbs_as_hexwkb(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result, macro_result$hexwkb)
  })

  ## DDBS_AS_GEOJSON
  testthat::it("ddbs_as_geojson() macro works", {
    normal_result <- ddbs_as_geojson(countries_ddbs)
    macro_result  <- countries_ddbs |>
      dplyr::mutate(geojson = ddbs_as_geojson(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result, macro_result$geojson)
  })

})


# 6. Extent functions ----------------------------------------------------

testthat::describe("Extent functions", {

  ## DDBS_BBOX
  testthat::it("ddbs_bbox() macro works", {
    normal_result <- ddbs_bbox(countries_ddbs, by_feature = TRUE) |> collect()
    macro_result  <- countries_ddbs |>
      dplyr::mutate(bbox = ddbs_bbox(geometry)) |>
      ddbs_collect()
    expect_equal(normal_result$xmin, macro_result$bbox$xmin)
    expect_equal(normal_result$xmax, macro_result$bbox$xmax)
    expect_equal(normal_result$ymin, macro_result$bbox$ymin)
    expect_equal(normal_result$ymax, macro_result$bbox$ymax)
  })

})

# 7. Union ---------------------------------------------------------------

testthat::describe("Extent functions", {

  ## Calculate length
  rivers_length_ddbs <- ddbs_length(rivers_ddbs)

  ## DDBS_BBOX
  testthat::it("ddbs_union() macro works", {

    normal_result <- rivers_length_ddbs |> 
      ddbs_union_agg("RIVER_NAME") |> 
      ddbs_length() |> 
      ddbs_collect(as = "tibble") |> 
      dplyr::arrange(RIVER_NAME)

    macro_result  <- rivers_length_ddbs |>
      dplyr::summarise(
        geom = ddbs_union(geom),
        length = sum(length),
        .by = RIVER_NAME
      )  |> 
      ddbs_collect(as = "tibble") |> 
      dplyr::arrange(RIVER_NAME)

    expect_equal(normal_result$length, macro_result$length)
  })

})

# =============================================================================
# Tests for duckspatial_df dplyr methods
# Tests: dplyr_reconstruct, collect, compute, left_join, inner_join, head
# Note: nc_sf is loaded from setup.R
# =============================================================================

# =============================================================================
# dplyr verb class preservation
# =============================================================================

test_that("dplyr verbs preserve duckspatial_df class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  # Test filter
  filtered <- nc_lazy |> dplyr::filter(AREA > 0.1)
  expect_s3_class(filtered, "duckspatial_df")
  expect_equal(attr(filtered, "crs"), attr(nc_lazy, "crs"))
  
  # Test mutate
  mutated <- nc_lazy |> dplyr::mutate(area_sq = AREA * AREA)
  expect_s3_class(mutated, "duckspatial_df")
  expect_equal(attr(mutated, "crs"), attr(nc_lazy, "crs"))
  
  # Test select
  selected <- nc_lazy |> dplyr::select(NAME, AREA, geometry)
  expect_s3_class(selected, "duckspatial_df")
  expect_equal(attr(selected, "crs"), attr(nc_lazy, "crs"))
  
  # Test arrange
  arranged <- nc_lazy |> dplyr::arrange(AREA)
  expect_s3_class(arranged, "duckspatial_df")
  expect_equal(attr(arranged, "crs"), attr(nc_lazy, "crs"))
})

test_that("group_by preserves duckspatial_df class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  grouped <- nc_lazy |> dplyr::group_by(SID74)
  
  expect_s3_class(grouped, "duckspatial_df")
  expect_equal(attr(grouped, "crs"), attr(nc_lazy, "crs"))
  expect_equal(attr(grouped, "sf_column"), attr(nc_lazy, "sf_column"))
})

test_that("summarize preserves duckspatial_df class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  summarized <- nc_lazy |> 
    dplyr::group_by(SID74) |> 
    dplyr::summarize(total_area = sum(AREA, na.rm = TRUE), .groups = "drop")
  
  expect_s3_class(summarized, "duckspatial_df")
  expect_equal(attr(summarized, "crs"), attr(nc_lazy, "crs"))
})

test_that("distinct preserves duckspatial_df class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  distinct_result <- nc_lazy |> dplyr::distinct(SID74, .keep_all = TRUE)
  
  expect_s3_class(distinct_result, "duckspatial_df")
  expect_equal(attr(distinct_result, "crs"), attr(nc_lazy, "crs"))
})

test_that("rename preserves duckspatial_df class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  renamed <- nc_lazy |> dplyr::rename(county_name = NAME)
  
  expect_s3_class(renamed, "duckspatial_df")
  expect_equal(attr(renamed, "crs"), attr(nc_lazy, "crs"))
})

test_that("slice_min preserves duckspatial_df class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  sliced <- nc_lazy |> dplyr::slice_min(AREA, n = 5)
  
  expect_s3_class(sliced, "duckspatial_df")
  expect_equal(attr(sliced, "crs"), attr(nc_lazy, "crs"))
})

test_that("head preserves duckspatial_df class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  headed <- nc_lazy |> head(10)
  
  expect_s3_class(headed, "duckspatial_df")
  expect_equal(attr(headed, "crs"), attr(nc_lazy, "crs"))
  expect_equal(attr(headed, "sf_column"), attr(nc_lazy, "sf_column"))
})

test_that("chained dplyr operations preserve duckspatial_df class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  result <- nc_lazy |>
    dplyr::filter(AREA > 0.1) |>
    dplyr::mutate(area_double = AREA * 2) |>
    dplyr::select(NAME, AREA, area_double, geometry) |>
    dplyr::arrange(dplyr::desc(AREA)) |>
    head(10)
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), attr(nc_lazy, "crs"))
  expect_equal(attr(result, "sf_column"), attr(nc_lazy, "sf_column"))
  
  collected <- dplyr::collect(result, as = "tibble")
  expect_s3_class(collected, "tbl_df")
  expect_equal(nrow(collected), 10)
})

# =============================================================================
# collect() tests
# =============================================================================

test_that("ddbs_collect works with duckspatial_df", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(geom_col = "geometry", crs = sf::st_crs(nc_sf))
  
  result <- ddbs_collect(nc_lazy)
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(nc_sf))
})

# =============================================================================
# compute() tests
# =============================================================================

test_that("compute.duckspatial_df forces execution and preserves class", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  computed <- dplyr::compute(nc_lazy)
  
  expect_s3_class(computed, "duckspatial_df")
  expect_s3_class(computed, "tbl_lazy")
  expect_equal(attr(computed, "crs"), attr(nc_lazy, "crs"))
  expect_equal(attr(computed, "sf_column"), attr(nc_lazy, "sf_column"))
})

test_that("compute.duckspatial_df simplifies query plan", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry") |>
    dplyr::filter(AREA > 0.1) |>
    dplyr::mutate(area_sq = AREA * AREA)
  
  query_before <- as.character(dbplyr::sql_render(nc_lazy))
  expect_true(grepl("AREA", query_before))
  
  computed <- dplyr::compute(nc_lazy)
  query_after <- as.character(dbplyr::sql_render(computed))
  
  expect_true(grepl("dbplyr_", query_after))
  expect_false(grepl("nc_test", query_after))
})

test_that("ddbs_compute wrapper works correctly", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf), geom_col = "geometry")
  
  computed <- ddbs_compute(nc_lazy)
  
  expect_s3_class(computed, "duckspatial_df")
  expect_equal(attr(computed, "crs"), attr(nc_lazy, "crs"))
  
  expect_error(ddbs_compute(data.frame(x = 1)), "duckspatial_df")
})

# =============================================================================
# join tests
# =============================================================================

test_that("left_join.duckspatial_df preserves spatial attributes", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  extra_data <- data.frame(NAME = nc_sf$NAME[1:5], extra_col = 1:5)
  DBI::dbWriteTable(conn, "extra_data", extra_data)
  extra_lazy <- dplyr::tbl(conn, "extra_data")
  
  result <- dplyr::left_join(nc_lazy, extra_lazy, by = "NAME")
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), attr(nc_lazy, "crs"))
  expect_equal(attr(result, "sf_column"), attr(nc_lazy, "sf_column"))
})

test_that("inner_join.duckspatial_df preserves spatial attributes", {
  conn <- ddbs_temp_conn()
  ddbs_write_vector(conn, nc_sf, "nc_test", quiet = TRUE)
  
  nc_lazy <- dplyr::tbl(conn, "nc_test") |>
    as_duckspatial_df(crs = sf::st_crs(nc_sf))
  
  extra_data <- data.frame(NAME = nc_sf$NAME[1:5], extra_col = 1:5)
  DBI::dbWriteTable(conn, "extra_data", extra_data)
  extra_lazy <- dplyr::tbl(conn, "extra_data")
  
  result <- dplyr::inner_join(nc_lazy, extra_lazy, by = "NAME")
  
  expect_s3_class(result, "duckspatial_df")
  expect_equal(attr(result, "crs"), attr(nc_lazy, "crs"))
  expect_equal(attr(result, "sf_column"), attr(nc_lazy, "sf_column"))
})

# =============================================================================
# Regression tests
# =============================================================================

test_that("dplyr::filter is preserved when chaining with ddbs_filter", {
  countries <- ddbs_open_dataset(
    system.file("spatial/countries.geojson", package = "duckspatial")
  )
  argentina <- ddbs_open_dataset(
    system.file("spatial/argentina.geojson", package = "duckspatial")
  )
  
  # Test 1: filter to subset, then spatial filter
  # Brazil, Uruguay, Chile, France - only first 3 touch Argentina
  subset <- countries |>
    dplyr::filter(CNTR_ID %in% c("BR", "UY", "CL", "FR"))
  
  result <- subset |>
    ddbs_filter(argentina, predicate = "touches") |>
    dplyr::collect()
  
  # Should be 3 (BR, UY, CL touch Argentina), not 4 (FR doesn't touch) 
  # and not 5 (all Argentina neighbors, which was the bug)
  expect_equal(nrow(result), 3)
  expect_setequal(result$CNTR_ID, c("BR", "UY", "CL"))
})

test_that("dplyr::select is preserved when chaining with ddbs_filter", {
  countries <- ddbs_open_dataset(
    system.file("spatial/countries.geojson", package = "duckspatial")
  )
  argentina <- ddbs_open_dataset(
    system.file("spatial/argentina.geojson", package = "duckspatial")
  )
  
  # Select only certain columns before spatial filter
  result <- countries |>
    dplyr::select(CNTR_ID, NAME_ENGL, geom) |>
    ddbs_filter(argentina, predicate = "touches") |>
    dplyr::collect()
  
  # Should only have selected columns
  expect_true("CNTR_ID" %in% names(result))
  expect_true("NAME_ENGL" %in% names(result))
  # Other original columns should NOT be present
  expect_false("ISO3_CODE" %in% names(result))
})

test_that("chained filter + select + ddbs_filter works", {
  countries <- ddbs_open_dataset(
    system.file("spatial/countries.geojson", package = "duckspatial")
  )
  argentina <- ddbs_open_dataset(
    system.file("spatial/argentina.geojson", package = "duckspatial")
  )
  
  result <- countries |>
    dplyr::filter(CNTR_ID %in% c("BR", "UY", "CL", "FR", "DE")) |>
    dplyr::select(CNTR_ID, geom) |>
    ddbs_filter(argentina, predicate = "touches") |>
    dplyr::collect()
  
  # Should be 3 (BR, UY, CL) and only selected columns (maybe crs_duckspatial added)
  expect_equal(nrow(result), 3)
  expect_setequal(result$CNTR_ID, c("BR", "UY", "CL"))
  expect_true("CNTR_ID" %in% names(result))
  expect_false("ISO3_CODE" %in% names(result))  # This was NOT selected
})

test_that("unmodified duckspatial_df still uses optimization", {
  countries <- ddbs_open_dataset(
    system.file("spatial/countries.geojson", package = "duckspatial")
  )
  argentina <- ddbs_open_dataset(
    system.file("spatial/argentina.geojson", package = "duckspatial")
  )
  
  # Without any dplyr operations, source_table optimization should still work
  result <- countries |>
    ddbs_filter(argentina, predicate = "touches") |>
    dplyr::collect()
  
  # All countries touching Argentina: BR, UY, PY, BO, CL
  expect_equal(nrow(result), 5)
})

test_that("mutate is preserved in ddbs_filter", {
  countries <- ddbs_open_dataset(
    system.file("spatial/countries.geojson", package = "duckspatial")
  )
  argentina <- ddbs_open_dataset(
    system.file("spatial/argentina.geojson", package = "duckspatial")
  )
  
  # Create a dummy column and filter on it
  # If mutate is ignored, "dummy_col" won't exist or filter won't work
  result <- countries |>
    dplyr::mutate(dummy_col = 1) |>
    dplyr::filter(dummy_col == 1) |>
    # Add a filter that relies on mutate result
    dplyr::mutate(is_ar = grepl("^AR", ISO3_CODE)) |>
    dplyr::filter(is_ar) |>
    ddbs_filter(argentina, predicate = "touches") |>
    dplyr::collect()
  
  # Should work just like the direct filter case (0 rows)
  expect_equal(nrow(result), 0)
})

test_that("rename is preserved in ddbs_filter", {
  countries <- ddbs_open_dataset(
    system.file("spatial/countries.geojson", package = "duckspatial")
  )
  argentina <- ddbs_open_dataset(
    system.file("spatial/argentina.geojson", package = "duckspatial")
  )
  
  # Rename ID column, then filter using new name
  # If rename ignored, new name won't exist
  result <- countries |>
    dplyr::rename(new_id = CNTR_ID) |>
    dplyr::filter(new_id %in% c("BR", "UY", "CL")) |>
    ddbs_filter(argentina, predicate = "touches") |>
    dplyr::collect()
  
  expect_equal(nrow(result), 3)
  expect_true("new_id" %in% names(result))
  expect_false("CNTR_ID" %in% names(result))
})

test_that("slice/head is preserved in ddbs_filter", {
  countries <- ddbs_open_dataset(
    system.file("spatial/countries.geojson", package = "duckspatial")
  )
  argentina <- ddbs_open_dataset(
    system.file("spatial/argentina.geojson", package = "duckspatial")
  )
  
  # Take top 1 country (Afghanistan usually), confirm it doesn't touch Argentina
  # If slice ignored, we get all neighbors
  result <- countries |>
    dplyr::arrange(NAME_ENGL) |>
    head(1) |>
    ddbs_filter(argentina, predicate = "touches") |>
    dplyr::collect()
    
  expect_equal(nrow(result), 0)
})

test_that("summarize before spatial op fails correctly (missing geom)", {
  # Summarize drops geometry for non-spatial summaries
  countries <- ddbs_open_dataset(
    system.file("spatial/countries.geojson", package = "duckspatial")
  )
  
  # Summarize to drop geometry, result is just a tibble (lazy)
  summarized <- countries |>
    dplyr::group_by(CNTR_ID) |>
    dplyr::summarize(n = dplyr::n())
    
  # Should fail because geometry column is missing in the summarized view
  # If it ignored summarize and used source_table, it would mistakenly succeed
  expect_error(
    ddbs_filter(summarized, countries),
    "Values list .* does not have a column named .*geom"
  )
})

# testthat::skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")
testthat::skip_if_not_installed("sf")
testthat::skip_if_not_installed("areal")

library(sf)
library(areal)

# Data Setup
# race: Source data (polygons with data)
# wards: Target data (polygons to interpolate to)
race <- areal::ar_stl_race
wards <- areal::ar_stl_wards

# -------------------------------------------------------------------------
# Core Logic Tests (Accuracy & Semantics)
# -------------------------------------------------------------------------

test_that("ddbs_interpolate_aw matches areal::aw_interpolate (weight='sum')", {
  # Logic: weight='sum' implies mass preservation relative to target coverage.
  # Denominator = Sum of overlapping areas.
  # This is the default behavior of areal::aw_interpolate.

  # 1. Run areal
  res_areal <- areal::aw_interpolate(
    wards,
    tid = WARD,
    source = race,
    sid = GEOID,
    weight = "sum",
    output = "tibble",
    extensive = "TOTAL_E"
  )

  # 2. Run duckspatial
  res_duck <- ddbs_interpolate_aw(
    target = wards,
    source = race,
    tid = "WARD",
    sid = "GEOID",
    extensive = "TOTAL_E",
    weight = "sum",
    keep_NA = TRUE, # Matches areal default
    conn = NULL
  )

  # Compare
  res_duck_df <- sf::st_drop_geometry(res_duck)
  cmp <- merge(
    res_areal[, c("WARD", "TOTAL_E")],
    res_duck_df[, c("WARD", "TOTAL_E")],
    by = "WARD",
    suffixes = c("_areal", "_duck")
  )

  # Check 1: Per-target agreement
  expect_equal(cmp$TOTAL_E_areal, cmp$TOTAL_E_duck, tolerance = 1e-6)

  # Check 2: Mass preservation
  expect_equal(
    sum(cmp$TOTAL_E_areal, na.rm = TRUE),
    sum(cmp$TOTAL_E_duck, na.rm = TRUE),
    tolerance = 1e-6
  )
})

test_that("ddbs_interpolate_aw matches sf::st_interpolate_aw (Extensive / weight='total')", {
  # Logic: weight='total' implies strict mass preservation of source.
  # Denominator = Total area of source polygon.
  # This matches sf::st_interpolate_aw(extensive=TRUE).

  # 1. Run sf (defaults: keep_NA=FALSE)
  res_sf <- suppressWarnings(sf::st_interpolate_aw(
    x = race["TOTAL_E"],
    to = wards,
    extensive = TRUE,
    keep_NA = FALSE
  ))

  # 2. Run duckspatial
  res_duck <- ddbs_interpolate_aw(
    target = wards,
    source = race,
    tid = "WARD",
    sid = "GEOID",
    extensive = "TOTAL_E",
    weight = "total",
    keep_NA = FALSE, # Explicitly match sf default
    conn = NULL
  )

  # Match rows by ID (sf doesn't keep ID by default, so we match on WARD via rowname mapping or assume order)
  # Here we rely on the fact that both use 'wards' as base.
  # To be robust, we attach WARD ID back to sf result based on geometry or index.
  res_sf$WARD <- wards$WARD[match(row.names(res_sf), row.names(wards))]

  cmp <- merge(
    sf::st_drop_geometry(res_sf),
    sf::st_drop_geometry(res_duck),
    by = "WARD"
  )

  # Values check
  expect_equal(cmp$TOTAL_E.x, cmp$TOTAL_E.y, tolerance = 1e-5)

  # Row count check (keep_NA=FALSE should drop non-overlapping targets)
  expect_equal(nrow(res_sf), nrow(res_duck))
})

test_that("ddbs_interpolate_aw matches sf::st_interpolate_aw (Intensive)", {
  # Logic: Intensive variables (densities).
  # Denominator = Sum of overlap areas per target.
  # Logic implies weight='sum' on the target aggregation side.

  # Create a dummy intensive variable
  race$density <- race$TOTAL_E / sf::st_area(race)

  # 1. Run sf
  res_sf <- suppressWarnings(sf::st_interpolate_aw(
    x = race["density"],
    to = wards,
    extensive = FALSE
  ))

  # 2. Run duckspatial
  res_duck <- ddbs_interpolate_aw(
    target = wards,
    source = race,
    tid = "WARD",
    sid = "GEOID",
    intensive = "density",
    weight = "sum",
    conn = NULL
  )

  vals_sf <- as.numeric(res_sf$density)
  vals_duck <- as.numeric(res_duck$density)

  # Compare only non-NA values
  idx <- !is.na(vals_sf) & !is.na(vals_duck)
  expect_equal(vals_sf[idx], vals_duck[idx], tolerance = 1e-4)
})

test_that("ddbs_interpolate_aw handles Mixed Interpolation (Extensive + Intensive)", {
  # areal supports mixed interpolation in one call.
  # weight argument applies to extensive variables (sum/total).
  # intensive variables always use sum-overlap logic.
  
  # Prepare Source Data with both types
  race_mixed <- race
  race_mixed$pop_density <- race_mixed$TOTAL_E / as.numeric(sf::st_area(race_mixed))
  
  # 1. Run areal
  res_areal <- areal::aw_interpolate(
    wards, tid = WARD, source = race_mixed, sid = GEOID,
    weight = "sum", output = "tibble", 
    extensive = "TOTAL_E", intensive = "pop_density"
  )
  
  # 2. Run duckspatial
  res_duck <- ddbs_interpolate_aw(
    target = wards, source = race_mixed, tid = "WARD", sid = "GEOID",
    extensive = "TOTAL_E", intensive = "pop_density",
    weight = "sum", keep_NA = TRUE, conn = NULL
  )
  
  res_duck_df <- sf::st_drop_geometry(res_duck)
  cmp <- merge(
    res_areal[, c("WARD", "TOTAL_E", "pop_density")],
    res_duck_df[, c("WARD", "TOTAL_E", "pop_density")],
    by = "WARD", suffixes = c("_areal", "_duck")
  )
  
  # Check Extensive
  expect_equal(cmp$TOTAL_E_areal, cmp$TOTAL_E_duck, tolerance = 1e-6)
  
  # Check Intensive
  expect_equal(cmp$pop_density_areal, cmp$pop_density_duck, tolerance = 1e-6)
})

# -------------------------------------------------------------------------
# Feature & Argument Tests (keep_NA, na.rm, output types)
# -------------------------------------------------------------------------

test_that("ddbs_interpolate_aw respects keep_NA=FALSE", {
  # Create a target that definitely does NOT overlap with the source
  # Source: St. Louis (race)
  # Target: St. Louis (wards) + 1 dummy polygon far away
  dummy_poly <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))))
  sf::st_crs(dummy_poly) <- sf::st_crs(wards)
  dummy_row <- wards[1, ]
  dummy_row$geometry <- dummy_poly
  dummy_row$WARD <- "DUMMY"
  wards_expanded <- rbind(wards, dummy_row)

  # 1. keep_NA = FALSE (Inner Join behavior)
  res_drop <- ddbs_interpolate_aw(
    target = wards_expanded, source = race, tid = "WARD", sid = "GEOID",
    extensive = "TOTAL_E", keep_NA = FALSE
  )

  # The dummy row should be gone
  expect_false("DUMMY" %in% res_drop$WARD)
  expect_equal(nrow(res_drop), nrow(wards)) # Should match original overlapping count

  # 2. keep_NA = TRUE (Left Join behavior)
  res_keep <- ddbs_interpolate_aw(
    target = wards_expanded, source = race, tid = "WARD", sid = "GEOID",
    extensive = "TOTAL_E", keep_NA = TRUE
  )

  # The dummy row should be present, with NA/0 for data
  expect_true("DUMMY" %in% res_keep$WARD)
  expect_equal(nrow(res_keep), nrow(wards_expanded))
})

test_that("ddbs_interpolate_aw respects na.rm=TRUE", {
  # Inject NAs into source data
  race_na <- race
  race_na$TOTAL_E[1:5] <- NA # First 5 rows are NA

  # 1. Run with na.rm=TRUE
  # Source rows with NA should be completely ignored during interpolation
  res_na_rm <- ddbs_interpolate_aw(
    target = wards, source = race_na, tid = "WARD", sid = "GEOID",
    extensive = "TOTAL_E", weight = "sum",
    na.rm = TRUE
  )

  # 2. Run with na.rm=FALSE
  # NAs propagate (results for overlapping targets will be NA or skewed depending on SQL sum behavior with nulls)
  # In SQL, SUM(col) usually ignores NULLs, but if ALL are NULL, result is NULL.
  # However, na.rm=TRUE filters the geometry entirely from the overlap calculation,
  # whereas na.rm=FALSE keeps the geometry but with NULL value.
  # For weighted interpolation, keeping geometry with NULL value affects denominators!
  res_keep_na <- ddbs_interpolate_aw(
    target = wards, source = race_na, tid = "WARD", sid = "GEOID",
    extensive = "TOTAL_E", weight = "sum",
    na.rm = FALSE
  )

  # Comparison:
  # Check if results differ. Specifically, if we remove source polygons,
  # the denominators (total area) might change for 'sum' weight if overlapping.
  # Just checking they are not identical validates the switch works.
  expect_false(identical(res_na_rm$TOTAL_E, res_keep_na$TOTAL_E))

  # Expect fewer NAs (or different values) when we actively remove bad source rows vs letting them sit in the join.
})

test_that("ddbs_interpolate_aw handles projection via join_crs", {
  conn <- ddbs_create_conn()
  ddbs_write_vector(conn, wards, "wards_tbl", overwrite = TRUE)
  ddbs_write_vector(conn, race, "race_tbl", overwrite = TRUE)

  # Run with explicit reprojection to Mercator (3857)
  res_proj <- ddbs_interpolate_aw(
    target = "wards_tbl",
    source = "race_tbl",
    tid = "WARD",
    sid = "GEOID",
    extensive = "TOTAL_E",
    weight = "sum",
    join_crs = 3857,
    conn = conn
  )

  expect_s3_class(res_proj, "sf")
  expect_true("TOTAL_E" %in% names(res_proj))

  # Total sum check
  total_pop <- sum(race$TOTAL_E, na.rm = TRUE)
  res_pop <- sum(res_proj$TOTAL_E, na.rm = TRUE)
  expect_equal(res_pop, total_pop, tolerance = 0.05)

  ddbs_stop_conn(conn)
})

test_that("ddbs_interpolate_aw handles output to table", {
  conn <- ddbs_create_conn()

  ddbs_interpolate_aw(
    target = wards,
    source = race,
    tid = "WARD",
    sid = "GEOID",
    extensive = "TOTAL_E",
    name = "result_table",
    conn = conn
  )

  expect_true("result_table" %in% DBI::dbListTables(conn))

  # Check content
  res <- ddbs_read_vector(conn, "result_table", crs = sf::st_crs(wards))
  expect_true("TOTAL_E" %in% names(res))
  expect_equal(nrow(res), nrow(wards))

  ddbs_stop_conn(conn)
})

test_that("ddbs_interpolate_aw throws errors for missing arguments", {
  expect_error(
    ddbs_interpolate_aw(wards, race, tid = "WARD"),
    "sid"
  )
  expect_error(
    ddbs_interpolate_aw(wards, race, sid = "GEOID"),
    "tid"
  )
  expect_error(
    ddbs_interpolate_aw(wards, race, tid = "WARD", sid = "GEOID"),
    "extensive"
  )
})

# -------------------------------------------------------------------------
# Edge Cases (CRS Mismatch, Disjoint)
# -------------------------------------------------------------------------

test_that("ddbs_interpolate_aw errors on CRS mismatch if join_crs is NULL", {
  # Create version with different CRS (Mercator)
  wards_3857 <- sf::st_transform(wards, 3857)
  
  # 1. Should error because wards is 3857 and race is NAD83, and no join_crs provided
  # assert_crs is called internally which triggers "different" message
  expect_error(
    ddbs_interpolate_aw(
      target = wards_3857, source = race, tid = "WARD", sid = "GEOID",
      extensive = "TOTAL_E"
    ),
    "different"
  )
  
  # 2. Should succeed if we provide join_crs (forcing reprojection of both to common CRS)
  expect_no_error(
    ddbs_interpolate_aw(
      target = wards_3857, source = race, tid = "WARD", sid = "GEOID",
      extensive = "TOTAL_E", join_crs = 3857
    )
  )
})

test_that("ddbs_interpolate_aw handles disjoint data correctly", {
  # Create a source that is miles away from target
  source_far <- race
  
  # shift coordinates by 1,000,000 meters (1000km) to ensure disjointness
  # (100m was not enough for projected data)
  shifted_geom <- sf::st_geometry(source_far) + c(1000000, 1000000)
  shifted_geom <- sf::st_set_crs(shifted_geom, sf::st_crs(source_far))
  sf::st_geometry(source_far) <- shifted_geom
  
  # 1. keep_NA = TRUE -> All target rows returned, but values should be NA (no overlap)
  res <- ddbs_interpolate_aw(
    target = wards, source = source_far, tid = "WARD", sid = "GEOID",
    extensive = "TOTAL_E", keep_NA = TRUE
  )
  
  expect_equal(nrow(res), nrow(wards))
  # All values should be NA because there is no overlap
  expect_true(all(is.na(res$TOTAL_E)))
  
  # 2. keep_NA = FALSE -> Empty result (targets with no overlap are dropped)
  res_drop <- ddbs_interpolate_aw(
    target = wards, source = source_far, tid = "WARD", sid = "GEOID",
    extensive = "TOTAL_E", keep_NA = FALSE
  )
  expect_equal(nrow(res_drop), 0)
})


# -------------------------------------------------------------------------
# New Feature Tests (Tibble output, Validation, Conflicts)
# -------------------------------------------------------------------------

test_that("ddbs_interpolate_aw supports output = 'tibble' (Non-spatial)", {
  # This tests the performance optimization path (skipping geometry construction)
  res_tbl <- ddbs_interpolate_aw(
    target = wards, 
    source = race, 
    tid = "WARD", 
    sid = "GEOID",
    extensive = "TOTAL_E", 
    output = "tibble"
  )
  
  # Should be a tibble/data.frame, NOT an sf object
  expect_s3_class(res_tbl, "tbl_df")
  expect_false(inherits(res_tbl, "sf"))
  
  # Should still have attributes
  expect_true("TOTAL_E" %in% names(res_tbl))
  expect_true("WARD" %in% names(res_tbl))
  
  # Should NOT have a geometry column (unless it was explicitly named differently)
  # Standard sf geometry column is usually "geometry" or "geom"
  expect_false("geometry" %in% names(res_tbl))
})

test_that("ddbs_interpolate_aw enforces strict logic validation", {
  # 1. Error if weight='total' is used with intensive variables
  # (Mathematically invalid to sum densities over total areas)
  expect_error(
    ddbs_interpolate_aw(
      target = wards, source = race, tid = "WARD", sid = "GEOID",
      intensive = "density", # Assume density exists or checks pass before logic
      weight = "total"
    ),
    "intensive variables must use" # Expecting the error message from cli_abort
  )
  
  # 2. Error if output argument is invalid
  expect_error(
    ddbs_interpolate_aw(
      target = wards, source = race, tid = "WARD", sid = "GEOID",
      extensive = "TOTAL_E", 
      output = "geojson"
    ),
    "must be either 'sf' or 'tibble'"
  )
})

test_that("ddbs_interpolate_aw warns on Column Name Conflicts", {
  # Create a target that already has the column "TOTAL_E"
  wards_conflict <- wards
  wards_conflict$TOTAL_E <- 99999
  
  # Logic: The function should Warn the user, but proceed by 
  # prioritizing the Interpolated value over the original Target value.
  expect_warning(
    res_conflict <- ddbs_interpolate_aw(
      target = wards_conflict, source = race, tid = "WARD", sid = "GEOID",
      extensive = "TOTAL_E"
    ),
    "conflict detected"
  )
  
  # Check that the value is the interpolated value (~4000ish), not the dummy 99999
  # We check the first row that isn't NA
  val <- res_conflict$TOTAL_E[!is.na(res_conflict$TOTAL_E)][1]
  expect_lt(val, 10000) 
})

# -------------------------------------------------------------------------
# Advanced CRS Edge Cases (Missing CRS)
# -------------------------------------------------------------------------
test_that("ddbs_interpolate_aw handles Missing CRS inputs appropriately", {
  # Setup data with NO CRS
  wards_no_crs <- wards
  sf::st_crs(wards_no_crs) <- NA
  
  race_no_crs <- race
  sf::st_crs(race_no_crs) <- NA
  
  # Case 1: Both missing CRS, NO join_crs requested.
  # This should pass (assuming raw coordinates overlap).
  expect_no_error(
    res <- ddbs_interpolate_aw(
      target = wards_no_crs, source = race_no_crs, 
      tid = "WARD", sid = "GEOID", extensive = "TOTAL_E"
    )
  )
  expect_true(is.na(sf::st_crs(res)))
  
  # Case 2: Missing CRS, but join_crs IS requested.
  # This MUST fail because we cannot project something if we don't know what it is.
  expect_error(
    ddbs_interpolate_aw(
      target = wards_no_crs, source = race_no_crs, 
      tid = "WARD", sid = "GEOID", extensive = "TOTAL_E",
      join_crs = 5070
    ),
    "Cannot transform" # Matches: "Target CRS unknown... Cannot transform..."
  )
  
  # Case 3: One has CRS, one does not (No join_crs).
  # This should error due to mismatch.
  expect_error(
    ddbs_interpolate_aw(
      target = wards_no_crs, source = race, 
      tid = "WARD", sid = "GEOID", extensive = "TOTAL_E"
    ),
    "mismatch" # Matches: "CRS mismatch: One input has a defined CRS..."
  )
})

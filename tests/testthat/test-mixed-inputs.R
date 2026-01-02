
# Test ddbs_filter and mixed inputs
test_that("ddbs_filter works with mixed inputs", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  # Setup connections/data
  conn <- ddbs_create_conn() # separate connection for isolation
  on.exit(ddbs_stop_conn(conn))
  
  # Prepare inputs
  # 1. sf object
  p_sf <- points_sf[1:10, ]
  c_sf <- countries_sf[1:5, ]
  
  # 2. duckspatial_df (lazy) in the same connection
  # (Requires writing first)
  ddbs_write_vector(conn, p_sf, "points_lazy")
  p_lazy <- dplyr::tbl(conn, "points_lazy") |> 
    as_duckspatial_df(
       geom_col = "geometry", 
       crs = sf::st_crs(p_sf)
    )
    
  # TEST 1: filter(sf, sf) -> covered implicitly by other tests, but good to have
  res1 <- ddbs_filter(p_sf, c_sf, output = "sf")
  expect_s3_class(res1, "sf")
  
  # TEST 2: filter(duckspatial_df, sf)
  # Should treat duckspatial_df as target connection origin
  res2 <- ddbs_filter(p_lazy, c_sf)
  # Default output is now duckspatial_df (lazy)
  expect_s3_class(res2, "duckspatial_df")
  expect_true(inherits(res2, "tbl_lazy"))
  
  # VERIFY CRS RETENTION (Regression Test)
  expect_false(is.na(sf::st_crs(res2)))
  expect_true(sf::st_crs(res2) == sf::st_crs(p_sf))
  
  # Test result correctness
  res2_collected <- collect(res2)
  expect_s3_class(res2_collected, "sf")
  expect_equal(nrow(res2_collected), nrow(res1))
  
  # TEST 3: filter(sf, duckspatial_df)
  # Should use duckspatial_df connection as target
  res3 <- ddbs_filter(p_sf, p_lazy) # Point in Point (just for type testing)
  expect_s3_class(res3, "duckspatial_df")
  expect_false(is.na(sf::st_crs(res3))) 
  
  # TEST 4: filter(duckspatial_df, duckspatial_df)
  ddbs_write_vector(conn, c_sf, "countries_lazy")
  c_lazy <- dplyr::tbl(conn, "countries_lazy") |>
    as_duckspatial_df(geom_col = "geometry", crs = sf::st_crs(c_sf))
    
  res4 <- ddbs_filter(p_lazy, c_lazy)
  expect_s3_class(res4, "duckspatial_df")
  expect_false(is.na(sf::st_crs(res4)))
})

test_that("ddbs_join works with mixed inputs and cross-connection", {
  conn1 <- ddbs_create_conn()
  conn2 <- ddbs_create_conn()
  on.exit({
    ddbs_stop_conn(conn1)
    ddbs_stop_conn(conn2)
  })
  
  p_sf <- points_sf[1:10, ]
  c_sf <- countries_sf[1:5, ]
  
  # Setup: p_lazy in conn1
  ddbs_write_vector(conn1, p_sf, "points_c1")
  p_lazy1 <- dplyr::tbl(conn1, "points_c1") |> as_duckspatial_df(geom_col="geometry", crs=sf::st_crs(4326))
  
  # Setup: c_lazy in conn2
  ddbs_write_vector(conn2, c_sf, "countries_c2")
  c_lazy2 <- dplyr::tbl(conn2, "countries_c2") |> as_duckspatial_df(geom_col="geometry", crs=sf::st_crs(4326))
  
  # Cross-connection join: p_lazy1 (conn1) LEFT JOIN c_lazy2 (conn2)
  # Should import c_lazy2 into conn1
  expect_warning(
    res <- ddbs_join(p_lazy1, c_lazy2, join = "intersects"),
    "different DuckDB connections"
  )
  
  # Result should be lazy table in conn1
  expect_true(inherits(res, "duckspatial_df"))
  expect_identical(dbplyr::remote_con(res), conn1)
  
  # VERIFY CRS RETENTION
  expect_false(is.na(sf::st_crs(res)))
  expect_true(sf::st_crs(res) == sf::st_crs(p_sf))
  
  # Collect to verify
  res_df <- collect(res)
  expect_s3_class(res_df, "sf")
})

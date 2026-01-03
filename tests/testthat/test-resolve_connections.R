
test_that("resolve_spatial_connections handles single connection correctly", {
  conn <- ddbs_temp_conn()
  
  # Case 1: All on same connection, explicit conn
  res <- resolve_spatial_connections("t1", "t2", conn = conn)
  expect_identical(res$conn, conn)
  expect_equal(res$x, "t1")
  expect_equal(res$y, "t2")
  
  # Case 2: Implicit connection from objects
  # We need get_conn_from_input to work. It uses dbplyr::remote_con(x) for tbl_lazy/duckspatial_df
  # For the test, we can use real tables to avoid mocking internals that might fail
  ddbs_write_vector(conn, sf::st_sf(geom=sf::st_sfc(sf::st_point(1:2)), crs=4326), "t1")
  t1_df <- dplyr::tbl(conn, "t1")
  t2_df <- dplyr::tbl(conn, "t1") # reuse
  
  res2 <- resolve_spatial_connections(t1_df, t2_df)
  expect_identical(res2$conn, conn)
})

test_that("resolve_spatial_connections handles cross-connection correctly", {
  conn1 <- ddbs_temp_conn()
  conn2 <- ddbs_temp_conn()
  
  # Setup data
  ddbs_write_vector(conn1, sf::st_sf(geom=sf::st_sfc(sf::st_point(c(0,0))), crs=4326), "t1")
  ddbs_write_vector(conn2, sf::st_sf(geom=sf::st_sfc(sf::st_point(c(1,1))), crs=4326), "t2")
  
  # t2 (foreign) should be imported to conn1 (target)
  # Case: conn explicit
  suppressWarnings(expect_warning(
    res <- resolve_spatial_connections("t1", "t2", conn = conn1, conn_x = conn1, conn_y = conn2),
    "target connection are different"
  ))
  
  expect_identical(res$conn, conn1)
  expect_equal(res$x, "t1")
  expect_false(res$y == "t2") # imported view name
  
  # Verify import worked
  imported_y <- DBI::dbReadTable(conn1, res$y)
  expect_equal(nrow(imported_y), 1)
  
  # Note: cleanup behavior depends on import strategy used
  # For in-memory DBs, collect+register is used which creates a table not a view
})

test_that("resolve_spatial_connections imports x when explicit conn differs", {
  conn_target <- ddbs_temp_conn()
  conn_source <- ddbs_temp_conn()
  
  ddbs_write_vector(conn_source, sf::st_sf(geom=sf::st_sfc(sf::st_point(c(0,0))), crs=4326), "t1")
  
  suppressWarnings(expect_warning(
    res <- resolve_spatial_connections("t1", "t2", conn = conn_target, conn_x = conn_source),
    "Importing `x` to the target connection"
  ))
  
  expect_identical(res$conn, conn_target)
  # x should be imported (new name), y should be "t2" (assuming strict mode off or character pass-through)
  expect_false(res$x == "t1") 
  expect_equal(res$y, "t2")
})

test_that("resolve_spatial_connections defaults to conn_x if available", {
  conn1 <- ddbs_temp_conn()
  
  ddbs_write_vector(conn1, sf::st_sf(geom=sf::st_sfc(sf::st_point(c(0,0))), crs=4326), "t1")
  t1_df <- dplyr::tbl(conn1, "t1")
  
  res <- resolve_spatial_connections(t1_df, "t2")
  expect_identical(res$conn, conn1)
})

test_that("resolve_spatial_connections warns when conn_x != conn_y without explicit conn", {
  conn1 <- ddbs_temp_conn()
  conn2 <- ddbs_temp_conn()
  
  ddbs_write_vector(conn1, sf::st_sf(geom=sf::st_sfc(sf::st_point(c(0,0))), crs=4326), "t1")
  ddbs_write_vector(conn2, sf::st_sf(geom=sf::st_sfc(sf::st_point(c(1,1))), crs=4326), "t2")
  
  t1_df <- dplyr::tbl(conn1, "t1")
  t2_df <- dplyr::tbl(conn2, "t2")
  
  # Should warn about different connections when no explicit conn provided
  suppressWarnings(expect_warning(
    res <- resolve_spatial_connections(t1_df, t2_df),
    "different DuckDB connections"
  ))
  
  # Should use x's connection as target
  expect_identical(res$conn, conn1)
})

test_that("resolve_spatial_connections cleanup is always a function", {
  conn <- ddbs_temp_conn()
  
  res <- resolve_spatial_connections("t1", "t2", conn = conn)
  
  # cleanup should always be a function

  expect_true(is.function(res$cleanup))
  
  # Should be callable without error
  expect_no_error(res$cleanup())
})

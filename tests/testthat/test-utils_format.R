test_that("get_parquet_crs handles column names with dots and special characters", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("sf")

  # Create a tiny Parquet file with a dot in geometry name
  df <- data.frame(id = 1L)
  wkb <- as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00))
  df[["geom.1"]] <- I(list(wkb))
  
  tbl <- arrow::Table$create(df)
  
  # Fetch full PROJJSON from sf to ensure it is valid for DuckDB
  projjson_str <- sf::st_crs(3857)$projjson
  
  # Fallback to a valid complete PROJJSON for 3857 if sf returns NULL
  if (is.null(projjson_str)) {
      projjson_str <- projjson_fallback_3857
  }
  
  geo_meta <- list(
    version = "1.1.0",
    primary_column = "geom.1",
    columns = list(
      "geom.1" = list(
        encoding = "WKB",
        geometry_types = list("Point"),
        crs = jsonlite::fromJSON(projjson_str, simplifyVector = FALSE)
      )
    )
  )
  
  tbl$metadata[["geo"]] <- jsonlite::toJSON(geo_meta, auto_unbox = TRUE)
  tmp_pq <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tbl, tmp_pq)
  on.exit(unlink(tmp_pq), add = TRUE)
  
  conn <- tryCatch(ddbs_default_conn(), error = function(e) DBI::dbConnect(duckdb::duckdb()))
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  duckspatial::ddbs_install(conn, extension = "json", quiet = TRUE)
  duckspatial::ddbs_load(conn, extension = "json", quiet = TRUE)
  
  crs <- duckspatial:::get_parquet_crs(tmp_pq, conn)
  expect_false(is.null(crs))
  expect_equal(crs$epsg, 3857)
})

test_that("get_parquet_crs handles column names with spaces", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("sf")

  df <- data.frame(id = 1L)
  wkb <- as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00))
  df[["my geom"]] <- I(list(wkb))
  
  tbl <- arrow::Table$create(df)
  
  projjson_str <- sf::st_crs(4326)$projjson
  if (is.null(projjson_str)) {
      # Fallback to a valid complete PROJJSON for 4326
      projjson_str <- projjson_fallback_4326
  }
  
  geo_meta <- list(
    version = "1.1.0",
    primary_column = "my geom",
    columns = list(
      "my geom" = list(
        encoding = "WKB",
        geometry_types = list("Point"),
        crs = jsonlite::fromJSON(projjson_str, simplifyVector = FALSE)
      )
    )
  )
  
  tbl$metadata[["geo"]] <- jsonlite::toJSON(geo_meta, auto_unbox = TRUE)
  tmp_pq <- tempfile(fileext = ".parquet")
  arrow::write_parquet(tbl, tmp_pq)
  on.exit(unlink(tmp_pq), add = TRUE)
  
  conn <- tryCatch(ddbs_default_conn(), error = function(e) DBI::dbConnect(duckdb::duckdb()))
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  duckspatial::ddbs_install(conn, extension = "json", quiet = TRUE)
  duckspatial::ddbs_load(conn, extension = "json", quiet = TRUE)
  
  crs <- duckspatial:::get_parquet_crs(tmp_pq, conn)
  expect_false(is.null(crs))
  expect_equal(crs$epsg, 4326)
})

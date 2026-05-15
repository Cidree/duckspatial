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
      projjson_str <- '{"$schema":"https://proj.org/schemas/v0.7/projjson.schema.json","type":"ProjectedCRS","name":"WGS 84 / Pseudo-Mercator","base_crs":{"type":"GeographicCRS","name":"WGS 84","datum":{"type":"GeodeticReferenceFrame","name":"World Geodetic System 1984","ellipsoid":{"name":"WGS 84","semi_major_axis":6378137,"inverse_flattening":298.257223563}},"coordinate_system":{"subtype":"ellipsoidal","axis":[{"name":"Latitude","abbreviation":"lat","direction":"north","unit":"degree"},{"name":"Longitude","abbreviation":"lon","direction":"east","unit":"degree"}]},"id":{"authority":"EPSG","code":4326}},"conversion":{"name":"unnamed","method":{"name":"Popular Visualisation Pseudo Mercator","id":{"authority":"EPSG","code":1024}},"parameters":[{"name":"Latitude of natural origin","value":0,"unit":"degree","id":{"authority":"EPSG","code":8801}},{"name":"Longitude of natural origin","value":0,"unit":"degree","id":{"authority":"EPSG","code":8802}},{"name":"False easting","value":0,"unit":"metre","id":{"authority":"EPSG","code":8806}},{"name":"False northing","value":0,"unit":"metre","id":{"authority":"EPSG","code":8807}}]},"coordinate_system":{"subtype":"Cartesian","axis":[{"name":"Easting","abbreviation":"X","direction":"east","unit":"metre"},{"name":"Northing","abbreviation":"Y","direction":"north","unit":"metre"}]},"id":{"authority":"EPSG","code":3857}}'
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

test_with_space <- function() {
  df <- data.frame(id = 1L)
  wkb <- as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00))
  df[["my geom"]] <- I(list(wkb))
  
  tbl <- arrow::Table$create(df)
  
  projjson_str <- sf::st_crs(4326)$projjson
  if (is.null(projjson_str)) {
      # Fallback to a valid complete PROJJSON for 4326
      projjson_str <- '{"$schema":"https://proj.org/schemas/v0.7/projjson.schema.json","type":"GeographicCRS","name":"WGS 84","datum_ensemble":{"name":"World Geodetic System 1984 ensemble","members":[{"name":"World Geodetic System 1984 (Transit)","id":{"authority":"EPSG","code":1166}},{"name":"World Geodetic System 1984 (G730)","id":{"authority":"EPSG","code":1152}},{"name":"World Geodetic System 1984 (G873)","id":{"authority":"EPSG","code":1153}},{"name":"World Geodetic System 1984 (G1150)","id":{"authority":"EPSG","code":1154}},{"name":"World Geodetic System 1984 (G1674)","id":{"authority":"EPSG","code":1155}},{"name":"World Geodetic System 1984 (G1762)","id":{"authority":"EPSG","code":1156}},{"name":"World Geodetic System 1984 (G2139)","id":{"authority":"EPSG","code":1157}}],"ellipsoid":{"name":"WGS 84","semi_major_axis":6378137,"inverse_flattening":298.257223563},"accuracy":"2.0","id":{"authority":"EPSG","code":6326}},"coordinate_system":{"subtype":"ellipsoidal","axis":[{"name":"Geodetic latitude","abbreviation":"Lat","direction":"north","unit":"degree"},{"name":"Geodetic longitude","abbreviation":"Lon","direction":"east","unit":"degree"}]},"id":{"authority":"EPSG","code":4326}}'
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
}

test_that("get_parquet_crs handles column names with spaces", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("sf")
  test_with_space()
})


# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## insert data
ddbs_write_table(conn_test, countries_sf, "countries")


# 1. ddbs_as_text() ------------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (we can't because DuckDB retrieves more decimals)
## - CHECK 2.1: errors
describe("ddbs_as_text()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------
  describe("expected behavior", {

    it("works on all input formats", {
      output_sf   <- ddbs_as_text(countries_sf)
      output_ddbs <- ddbs_as_text(countries_ddbs)
      output_conn <- ddbs_as_text("countries", conn_test)

      expect_equal(output_sf, output_ddbs)
      expect_equal(output_sf, output_conn)
    })

    it("doesn't display a message", {
      expect_no_message(ddbs_as_text(countries_sf))
    })

  })

  ### ERRORS ------------------------------------------------------------
  describe("errors work", {

    it("throws errors for invalid inputs", {
      expect_error(ddbs_as_text(x = 999))
      expect_error(ddbs_as_text(countries_ddbs, conn = 999))
      expect_error(ddbs_as_text(x = "999", conn = conn_test))
    })

  })

})



# 2. ddbs_as_wkb ---------------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (the class is different, so we compare the first 
## and last elements)
## - CHECK 2.1: errors
describe("ddbs_as_wkb()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------
  describe("expected behavior", {

    it("works on all input formats and produces consistent WKB", {
      output_sf   <- ddbs_as_wkb(countries_sf)
      output_ddbs <- ddbs_as_wkb(countries_ddbs)
      output_conn <- ddbs_as_wkb("countries", conn_test)

      expect_equal(output_sf, output_ddbs)
      expect_equal(output_sf, output_conn)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_as_wkb(countries_sf))
    })

    it("matches SF WKB output at first and last elements", {
      sf_output <- sf::st_as_binary(countries_sf$geometry)
      output_sf   <- ddbs_as_wkb(countries_sf)

      expect_equal(output_sf[[1]], sf_output[[1]])
      expect_equal(length(output_sf), length(sf_output))
      expect_equal(output_sf[[length(output_sf)]], sf_output[[length(sf_output)]])
    })

  })

  ### ERRORS ------------------------------------------------------------
  describe("errors work", {

    it("throws errors for invalid inputs", {
      expect_error(ddbs_as_wkb(x = 999))
      expect_error(ddbs_as_wkb(countries_ddbs, conn = 999))
      expect_error(ddbs_as_wkb(x = "999", conn = conn_test))
    })

  })

})


# 3. ddbs_as_hexwkb() ----------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: message works
## - CHECK 1.3: compare to SF (the class is different, so we compare the first 
## and last elements)
## - CHECK 2.1: errors
describe("ddbs_as_hexwkb()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------
  describe("expected behavior", {

    it("works on all input formats and produces consistent HEX WKB", {
      output_sf   <- ddbs_as_hexwkb(countries_sf)
      output_ddbs <- ddbs_as_hexwkb(countries_ddbs)
      output_conn <- ddbs_as_hexwkb("countries", conn_test)

      expect_equal(output_sf, output_ddbs)
      expect_equal(output_sf, output_conn)
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_as_hexwkb(countries_sf))
    })

    it("matches SF HEX WKB output at first and last elements", {
      sf_output <- sf::st_as_binary(countries_sf$geometry, hex = TRUE)
      output_sf   <- ddbs_as_hexwkb(countries_sf)
      output_sf_lower <- lapply(output_sf, tolower)

      expect_equal(output_sf_lower[[1]], sf_output[[1]])
      expect_equal(length(output_sf_lower), length(sf_output))
      expect_equal(output_sf_lower[[length(output_sf_lower)]], sf_output[[length(sf_output)]])
    })

  })

  ### ERRORS ------------------------------------------------------------
  describe("errors work", {

    it("throws errors for invalid inputs", {
      expect_error(ddbs_as_hexwkb(x = 999))
      expect_error(ddbs_as_hexwkb(countries_ddbs, conn = 999))
      expect_error(ddbs_as_hexwkb(x = "999", conn = conn_test))
    })

  })

})



# 4. ddbs_as_geojson() ---------------------------------------------------

## - CHECK 1.1: works on all input formats and they agree
## - CHECK 1.2: default returns a single FeatureCollection (class geojson/json)
## - CHECK 1.3: feature_collection = FALSE returns one Feature per row
## - CHECK 1.4: non-geometry columns are included as properties
## - CHECK 1.5: output is valid, parseable GeoJSON
## - CHECK 1.6: structurally matches geojsonsf::sf_geojson()
## - CHECK 1.7: geometry-only input yields empty properties
## - CHECK 1.8: empty input yields an empty FeatureCollection
## - CHECK 2.1: errors
describe("ddbs_as_geojson()", {

  ## encoding feature properties needs the DuckDB json extension (to_json);
  ## skip where it is not available (e.g. offline CI such as r-hub)
  skip_if(isFALSE(check_installed_extension(conn_test, "json")), "json extension not available")

  ### EXPECTED BEHAVIOR -------------------------------------------------
  describe("expected behavior", {

    it("works on all input formats and they agree", {
      output_sf   <- ddbs_as_geojson(countries_sf)
      output_ddbs <- ddbs_as_geojson(countries_ddbs)
      output_conn <- ddbs_as_geojson("countries", conn_test)

      expect_type(output_sf, "character")
      expect_equal(output_sf, output_ddbs)
      expect_equal(output_sf, output_conn)
    })

    it("returns a single FeatureCollection by default", {
      output <- ddbs_as_geojson(countries_sf)
      expect_length(output, 1L)
      expect_s3_class(output, "geojson")
      parsed <- jsonlite::fromJSON(output, simplifyVector = FALSE)
      expect_equal(parsed$type, "FeatureCollection")
      expect_length(parsed$features, nrow(countries_sf))
    })

    it("returns one Feature per row when feature_collection = FALSE", {
      output <- ddbs_as_geojson(countries_sf, feature_collection = FALSE)
      expect_type(output, "character")
      expect_length(output, nrow(countries_sf))
      expect_true(all(grepl('"type":"Feature"', output, fixed = TRUE)))
    })

    it("includes all non-geometry columns as properties", {
      output <- ddbs_as_geojson(countries_sf, feature_collection = FALSE)
      parsed <- jsonlite::fromJSON(output[1])

      expect_named(parsed, c("type", "properties", "geometry"))
      prop_names <- setdiff(names(countries_sf), attr(countries_sf, "sf_column"))
      expect_setequal(names(parsed$properties), prop_names)
    })

    it("produces valid, parseable GeoJSON geometry", {
      parsed <- jsonlite::fromJSON(ddbs_as_geojson(countries_sf), simplifyVector = FALSE)
      expect_equal(parsed$features[[1]]$type, "Feature")
      expect_false(is.null(parsed$features[[1]]$geometry$type))
    })

    it("matches the structure of geojsonsf::sf_geojson()", {
      skip_if_not_installed("geojsonsf")
      ours <- jsonlite::fromJSON(ddbs_as_geojson(countries_sf), simplifyVector = FALSE)
      ref  <- jsonlite::fromJSON(geojsonsf::sf_geojson(countries_sf), simplifyVector = FALSE)
      expect_equal(ours$type, ref$type)
      expect_equal(length(ours$features), length(ref$features))
      expect_equal(ours$features[[1]]$properties, ref$features[[1]]$properties)
    })

    it("yields empty properties when there are no other columns", {
      geom_only <- countries_sf[, attr(countries_sf, "sf_column")]
      output    <- ddbs_as_geojson(geom_only, feature_collection = FALSE)
      expect_true(all(grepl('"properties":{}', output, fixed = TRUE)))
    })

    it("returns an empty FeatureCollection for empty input", {
      DBI::dbExecute(conn_test, "CREATE OR REPLACE TABLE empty_geo AS SELECT * FROM countries WHERE 1=0;")
      output <- ddbs_as_geojson("empty_geo", conn_test)
      parsed <- jsonlite::fromJSON(output, simplifyVector = FALSE)
      expect_equal(parsed$type, "FeatureCollection")
      expect_length(parsed$features, 0L)
    })

    it("doesn't display a message", {
      expect_no_message(ddbs_as_geojson(countries_sf))
    })
  })

  ### ERRORS ------------------------------------------------------------
  describe("errors work", {

    it("throws errors for invalid inputs", {
      expect_error(ddbs_as_geojson(x = 999))
      expect_error(ddbs_as_geojson(countries_ddbs, conn = 999))
      expect_error(ddbs_as_geojson(x = "999", conn = conn_test))
      expect_error(ddbs_as_geojson(countries_ddbs, feature_collection = "yes"))
    })
  })
})



# 5. ddbs_geom_from_*() --------------------------------------------------

## Build serialized representations of two known points to round-trip back
## - CHECK 1.x: each parser returns the expected output class/format
## - CHECK 2.x: CRS is assigned, extra columns kept, named tables written
## - CHECK 3.x: round-trips reproduce the original coordinates
## - CHECK 4.x: errors on invalid inputs
geom_from_ser <- DBI::dbGetQuery(conn_test, "
  SELECT ST_AsText(geom)    AS wkt,
         ST_AsHEXWKB(geom)  AS hex,
         ST_AsGeoJSON(geom) AS gj,
         ST_AsWKB(geom)     AS wkb
  FROM (VALUES
    (ST_Point(10, 20)::GEOMETRY),
    (ST_Point(-58.38, -34.60)::GEOMETRY)
  ) t(geom);
")

describe("ddbs_geom_from_text()", {

  describe("expected behavior", {

    it("returns a duckspatial_df by default", {
      output <- ddbs_geom_from_text(geom_from_ser$wkt, crs = 4326)
      expect_s3_class(output, "duckspatial_df")
    })

    it("returns an sf object with mode = 'sf'", {
      output <- ddbs_geom_from_text(geom_from_ser$wkt, crs = 4326, mode = "sf")
      expect_s3_class(output, "sf")
      expect_equal(nrow(output), 2L)
    })

    it("assigns the CRS", {
      output <- ddbs_geom_from_text(geom_from_ser$wkt, crs = 4326, mode = "sf")
      expect_equal(sf::st_crs(output), sf::st_crs(4326))
    })

    it("round-trips coordinates correctly", {
      output <- ddbs_geom_from_text(geom_from_ser$wkt, crs = 4326, mode = "sf")
      expect_equal(sf::st_coordinates(output)[1, ], c(X = 10, Y = 20))
    })

    it("keeps extra columns passed via ...", {
      output <- ddbs_geom_from_text(geom_from_ser$wkt, id = 1:2, crs = 4326, mode = "sf")
      expect_true("id" %in% names(output))
      expect_equal(output$id, 1:2)
    })

    it("respects geom_col", {
      output <- ddbs_geom_from_text(geom_from_ser$wkt, crs = 4326, geom_col = "the_geom", mode = "sf")
      expect_equal(attr(output, "sf_column"), "the_geom")
    })

    it("writes a table when name is provided", {
      ok <- ddbs_geom_from_text(geom_from_ser$wkt, crs = 4326, conn = conn_test, name = "gft_tbl")
      expect_true(ok)
      expect_true(DBI::dbExistsTable(conn_test, "gft_tbl"))
    })
  })

  describe("errors work", {

    it("errors on non-character input", {
      expect_error(ddbs_geom_from_text(123))
    })

    it("errors when name is given without conn", {
      expect_error(ddbs_geom_from_text(geom_from_ser$wkt, name = "no_conn"))
    })

    it("errors on unnamed ... arguments", {
      expect_error(ddbs_geom_from_text(geom_from_ser$wkt, 1:2))
    })

    it("errors when extra columns have the wrong length", {
      expect_error(ddbs_geom_from_text(geom_from_ser$wkt, id = 1:3))
    })
  })
})

describe("ddbs_geom_from_hexwkb()", {

  it("returns a duckspatial_df by default", {
    expect_s3_class(ddbs_geom_from_hexwkb(geom_from_ser$hex, crs = 4326), "duckspatial_df")
  })

  it("round-trips coordinates correctly", {
    output <- ddbs_geom_from_hexwkb(geom_from_ser$hex, crs = 4326, mode = "sf")
    expect_equal(sf::st_coordinates(output)[1, ], c(X = 10, Y = 20))
  })

  it("errors on non-character input", {
    expect_error(ddbs_geom_from_hexwkb(123))
  })
})

describe("ddbs_geom_from_hexewkb()", {

  it("returns a duckspatial_df by default", {
    expect_s3_class(ddbs_geom_from_hexewkb(geom_from_ser$hex, crs = 4326), "duckspatial_df")
  })

  it("round-trips coordinates correctly", {
    output <- ddbs_geom_from_hexewkb(geom_from_ser$hex, crs = 4326, mode = "sf")
    expect_equal(sf::st_coordinates(output)[1, ], c(X = 10, Y = 20))
  })
})

describe("ddbs_geom_from_geojson()", {

  it("returns a duckspatial_df by default", {
    expect_s3_class(ddbs_geom_from_geojson(geom_from_ser$gj, crs = 4326), "duckspatial_df")
  })

  it("round-trips coordinates correctly", {
    output <- ddbs_geom_from_geojson(geom_from_ser$gj, crs = 4326, mode = "sf")
    expect_equal(sf::st_coordinates(output)[1, ], c(X = 10, Y = 20))
  })

  it("errors on non-character input", {
    expect_error(ddbs_geom_from_geojson(123))
  })
})

describe("ddbs_geom_from_wkb()", {

  it("returns a duckspatial_df by default", {
    expect_s3_class(ddbs_geom_from_wkb(geom_from_ser$wkb, crs = 4326), "duckspatial_df")
  })

  it("round-trips coordinates correctly", {
    output <- ddbs_geom_from_wkb(geom_from_ser$wkb, crs = 4326, mode = "sf")
    expect_equal(sf::st_coordinates(output)[1, ], c(X = 10, Y = 20))
  })

  it("errors when input is not a list of raw vectors", {
    expect_error(ddbs_geom_from_wkb("not raw"))
  })
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)

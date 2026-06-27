
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## A 10x10 polygon (in EPSG:4326) that exactly fills the c(0,0,10,10) bounds
mvt_sf <- sf::st_sf(
  id = 1L,
  geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
)
mvt_ddbs <- as_duckspatial_df(mvt_sf)
ddbs_write_table(conn_test, mvt_sf, "mvt_poly")


# 1. ddbs_as_mvt_geom() --------------------------------------------------

describe("ddbs_as_mvt_geom()", {

  describe("expected behavior", {

    it("returns a duckspatial_df by default", {
      expect_s3_class(ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10)), "duckspatial_df")
    })

    it("returns an sf object with mode = 'sf'", {
      output <- ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10), mode = "sf")
      expect_s3_class(output, "sf")
    })

    it("works on all input formats", {
      out_sf   <- ddbs_as_mvt_geom(mvt_sf, bounds = c(0, 0, 10, 10), mode = "sf")
      out_ddbs <- ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10), mode = "sf")
      out_conn <- ddbs_as_mvt_geom("mvt_poly", bounds = c(0, 0, 10, 10), conn = conn_test, mode = "sf")

      expect_equal(sf::st_as_text(sf::st_geometry(out_sf)), sf::st_as_text(sf::st_geometry(out_ddbs)))
      expect_equal(sf::st_as_text(sf::st_geometry(out_sf)), sf::st_as_text(sf::st_geometry(out_conn)))
    })

    it("maps the geometry into [0, extent] tile space", {
      output <- ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10), extent = 4096, mode = "sf")
      coords <- sf::st_coordinates(output)[, c("X", "Y")]
      expect_equal(range(coords), c(0, 4096))
    })

    it("respects the extent argument", {
      output <- ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10), extent = 256, mode = "sf")
      expect_equal(max(sf::st_coordinates(output)[, c("X", "Y")]), 256)
    })

    it("drops the CRS (output is in tile pixel coordinates)", {
      output <- ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10))
      expect_true(is.na(ddbs_crs(output)))
    })

    it("accepts an sf bbox as bounds", {
      output <- ddbs_as_mvt_geom(mvt_ddbs, bounds = sf::st_bbox(mvt_sf), mode = "sf")
      expect_s3_class(output, "sf")
    })

    it("keeps non-geometry columns", {
      output <- ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10), mode = "sf")
      expect_true("id" %in% names(output))
    })

    it("writes a table when name is provided", {
      ok <- ddbs_as_mvt_geom("mvt_poly", bounds = c(0, 0, 10, 10), conn = conn_test, name = "mvt_out")
      expect_true(ok)
      expect_true(DBI::dbExistsTable(conn_test, "mvt_out"))
    })
  })

  describe("errors", {

    it("errors on bounds of the wrong length", {
      expect_error(ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10)))
    })

    it("errors when xmax <= xmin or ymax <= ymin", {
      expect_error(ddbs_as_mvt_geom(mvt_ddbs, bounds = c(10, 10, 0, 0)))
    })

    it("errors on a non-positive extent", {
      expect_error(ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10), extent = 0))
    })

    it("errors on a negative buffer", {
      expect_error(ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10), buffer = -1))
    })

    it("errors on a non-logical clip_geom", {
      expect_error(ddbs_as_mvt_geom(mvt_ddbs, bounds = c(0, 0, 10, 10), clip_geom = "yes"))
    })

    it("errors on invalid x type", {
      expect_error(ddbs_as_mvt_geom(999, bounds = c(0, 0, 10, 10)))
    })

    it("requires conn when x is a table name", {
      expect_error(ddbs_as_mvt_geom("mvt_poly", bounds = c(0, 0, 10, 10)))
    })
  })
})


# 2. ddbs_write_mbtiles() ------------------------------------------------

## sample data with a string, an integer and a double attribute column
tile_sf <- sf::st_read(
  system.file("spatial/argentina.geojson", package = "duckspatial"), quiet = TRUE
)
tile_sf$name <- "Argentina"
tile_sf$pop  <- 45000000L
tile_sf$dens <- 16.5
tile_ddbs <- as_duckspatial_df(tile_sf[, c("name", "pop", "dens")])

## helper to read an mbtiles file back through a fresh duckdb sqlite attach
read_mbtiles <- function(conn, dsn) {
  DBI::dbExecute(conn, "INSTALL sqlite; LOAD sqlite;")
  DBI::dbExecute(conn, sprintf("ATTACH '%s' AS mbr (TYPE sqlite, READ_ONLY);", dsn))
  on.exit(try(DBI::dbExecute(conn, "DETACH mbr;"), silent = TRUE), add = TRUE)
  list(
    tiles    = DBI::dbGetQuery(conn, "SELECT zoom_level, tile_column, tile_row, octet_length(tile_data) nbytes FROM mbr.tiles;"),
    metadata = DBI::dbGetQuery(conn, "SELECT name, value FROM mbr.metadata;")
  )
}

## the sqlite extension may be unavailable offline; skip the section if so
sqlite_ok <- tryCatch({
  DBI::dbExecute(conn_test, "INSTALL sqlite; LOAD sqlite;")
  TRUE
}, error = function(e) FALSE)

describe("ddbs_write_mbtiles()", {

  testthat::skip_if_not(sqlite_ok, "DuckDB sqlite extension not available")

  describe("expected behavior", {

    it("writes an mbtiles file and returns its path invisibly", {
      dsn <- tempfile(fileext = ".mbtiles")
      res <- withVisible(ddbs_write_mbtiles(tile_ddbs, dsn = dsn, layer_name = "argentina", zoom_levels = 0:4, quiet = TRUE))
      expect_false(res$visible)
      expect_equal(res$value, dsn)
      expect_true(file.exists(dsn))
    })

    it("produces a tile pyramid across the requested zoom levels", {
      dsn <- tempfile(fileext = ".mbtiles")
      ddbs_write_mbtiles(tile_ddbs, dsn = dsn, layer_name = "argentina", zoom_levels = 0:4, quiet = TRUE)
      mb <- read_mbtiles(conn_test, dsn)
      expect_true(nrow(mb$tiles) > 0)
      expect_setequal(sort(unique(mb$tiles$zoom_level)), 0:4)
      expect_true(all(mb$tiles$nbytes > 0))
    })

    it("writes the expected metadata (format, zoom range, layer fields)", {
      dsn <- tempfile(fileext = ".mbtiles")
      ddbs_write_mbtiles(tile_ddbs, dsn = dsn, layer_name = "argentina", zoom_levels = 0:3, quiet = TRUE)
      meta <- read_mbtiles(conn_test, dsn)$metadata
      kv <- stats::setNames(meta$value, meta$name)
      expect_equal(kv[["format"]], "pbf")
      expect_equal(kv[["minzoom"]], "0")
      expect_equal(kv[["maxzoom"]], "3")
      expect_match(kv[["json"]], "\"argentina\"")
      expect_match(kv[["json"]], "\"pop\":\"Number\"")
      expect_match(kv[["json"]], "\"name\":\"String\"")
    })

    it("works with a character table name input", {
      ddbs_write_table(conn_test, tile_sf[, c("name", "pop", "dens")], "tile_src")
      dsn <- tempfile(fileext = ".mbtiles")
      expect_invisible(ddbs_write_mbtiles("tile_src", dsn = dsn, conn = conn_test, zoom_levels = 0:2, quiet = TRUE))
      expect_true(file.exists(dsn))
    })
  })

  describe("errors", {

    it("errors when dsn exists and overwrite = FALSE", {
      dsn <- tempfile(fileext = ".mbtiles")
      ddbs_write_mbtiles(tile_ddbs, dsn = dsn, zoom_levels = 0:1, quiet = TRUE)
      expect_error(ddbs_write_mbtiles(tile_ddbs, dsn = dsn, zoom_levels = 0:1))
    })

    it("errors on invalid zoom_levels", {
      expect_error(ddbs_write_mbtiles(tile_ddbs, dsn = tempfile(fileext = ".mbtiles"), zoom_levels = c(-1, 2)))
    })

    it("errors when the input has no CRS", {
      no_crs <- sf::st_set_crs(tile_sf[, "name"], NA)
      expect_error(ddbs_write_mbtiles(as_duckspatial_df(no_crs), dsn = tempfile(fileext = ".mbtiles")))
    })

    it("errors on invalid x type", {
      expect_error(ddbs_write_mbtiles(999, dsn = tempfile(fileext = ".mbtiles")))
    })
  })
})


## stop connection
duckspatial::ddbs_stop_conn(conn_test)

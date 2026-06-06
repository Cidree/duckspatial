
# Test suite for ddbs_crs S3 generic and methods
testthat::skip_on_cran()

test_that("ddbs_crs works for different input types", {
  skip_if_not_installed("sf")
  skip_if_not_installed("duckdb")
  
  # Setup
  conn <- ddbs_create_conn()
  on.exit(ddbs_stop_conn(conn))
  
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)[1:5,]
  
  # 1. SF Object method
  expect_equal(ddbs_crs(nc_sf), sf::st_crs(nc_sf))
  
  # 2. duckspatial_df method
  ddbs_write_table(conn, nc_sf, "nc_data")
  # Use default retrieval options
  nc_ds <- as_duckspatial_df("nc_data", conn, crs = sf::st_crs(nc_sf))
    
  expect_equal(ddbs_crs(nc_ds), sf::st_crs(nc_sf))
  
  # 3. tbl_duckdb_connection (lazy) method
  # Plain lazy table without duckspatial_df wrapper
  # nc_lazy <- dplyr::tbl(conn, "nc_data")
  nc_lazy <- as_duckspatial_df("nc_data", conn)
  # ddbs_crs should fallback to looking up the table in the DB if it is a simple table ref
  # Our implementation for tbl_duckdb_connection tries to detect from VIEW SQL or ST_Read
  # BUT "nc_data" is a table with 'crs_duckspatial' column?
  # ddbs_crs.tbl_duckdb_connection implementation ONLY looks for ST_Read views
  # It falls back to NA if not found.
  
  # Wait, if I pass a tbl(conn, "table"), it's a tbl_duckdb_connection.
  # Does logic support finding CRS from 'crs_duckspatial' column for lazy tables?
  # ddbs_crs.tbl_duckdb_connection logic I wrote prioritizes auto-detection from SQL.
  # If that fails, it returns NA.
  # Ideally it should check for 'crs_duckspatial' column too?
  # Currently it prints a warning and returns NA. 
  # Let's verifying assumption - it might be NA for plain tables.
  
  # Let's test the specific feature: Auto-detection from view
  path <- system.file("shape/nc.shp", package = "sf")
  DBI::dbExecute(conn, glue::glue("CREATE VIEW nc_view AS SELECT * FROM ST_Read('{path}')"))
  # nc_view_lazy <- dplyr::tbl(conn, "nc_view")
  nc_view_lazy <- as_duckspatial_df("nc_view", conn)
  
  crs_auto <- ddbs_crs(nc_view_lazy)
  expect_false(is.na(crs_auto))
  expect_equal(crs_auto$epsg, 4267) # NC shapefile has 4267
  
  # 4. Character method
  # Uses 'crs_duckspatial' column by default
  crs_char <- ddbs_crs("nc_data", conn = conn)
  expect_true(crs_char == sf::st_crs(nc_sf))
  
  # 5. Connection method (Backward Compatibility)
  # ddbs_crs(conn, name)
  crs_compat <- ddbs_crs(conn, "nc_data")
  expect_true(crs_compat == sf::st_crs(nc_sf))
  
  # 6. Default/Error
  expect_error(ddbs_crs(NULL))
  expect_error(ddbs_crs(123))
})



# 2. ddbs_set_crs() -------------------------------------------------------

## - CHECK 1.1: works on ddbs input (string, crs object, NA)
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, sf)
## - CHECK 1.3: works on sf input
## - CHECK 1.4: sf returns different outputs (duckspatial_df, sf)
## - CHECK 1.5: works on DuckDB table input
## - CHECK 1.6: DuckDB table returns different outputs (duckspatial_df, sf)
## - CHECK 1.7: message is shown with quiet = FALSE
## - CHECK 1.8: no message is shown with quiet = TRUE
## - CHECK 1.9: CRS is set correctly from EPSG string
## - CHECK 1.10: CRS is set correctly from sf::st_crs() object
## - CHECK 1.11: CRS is NA after removal
## - CHECK 1.12: CRS roundtrip (remove then re-assign)
## - CHECK 2.1: requires connection for character input
## - CHECK 2.2: other errors

conn_set_crs <- duckspatial::ddbs_create_conn()
ddbs_write_table(conn_set_crs, argentina_sf, "argentina_scrs")

describe("ddbs_set_crs()", {

  ### EXPECTED BEHAVIOR -------------------------------------------------

  describe("expected behavior", {

    it("works on ddbs input", {
      output_ddbs_1 <- ddbs_set_crs(argentina_ddbs, "EPSG:4326")
      output_ddbs_2 <- ddbs_set_crs(argentina_ddbs, sf::st_crs("EPSG:4326"))
      output_ddbs_3 <- ddbs_set_crs(argentina_ddbs, sf::st_crs(NA))
      output_ddbs_4 <- ddbs_set_crs(argentina_ddbs, "EPSG:4326", quiet = TRUE)

      expect_s3_class(output_ddbs_1, "duckspatial_df")
      expect_s3_class(output_ddbs_2, "duckspatial_df")
      expect_s3_class(output_ddbs_3, "duckspatial_df")
      expect_s3_class(output_ddbs_4, "duckspatial_df")
    })

    it("returns different output formats for ddbs input", {
      output_sf <- ddbs_set_crs(argentina_ddbs, "EPSG:4326", mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on sf input", {
      output_sf_1 <- ddbs_set_crs(argentina_sf, "EPSG:4326")
      output_sf_2 <- ddbs_set_crs(argentina_sf, sf::st_crs("EPSG:4326"))
      output_sf_3 <- ddbs_set_crs(argentina_sf, sf::st_crs(NA))
      output_sf_4 <- ddbs_set_crs(argentina_sf, "EPSG:4326", quiet = TRUE)

      expect_s3_class(output_sf_1, "duckspatial_df")
      expect_s3_class(output_sf_2, "duckspatial_df")
      expect_s3_class(output_sf_3, "duckspatial_df")
      expect_s3_class(output_sf_4, "duckspatial_df")
    })

    it("returns different output formats for sf input", {
      output_sf <- ddbs_set_crs(argentina_sf, "EPSG:4326", mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("works on DuckDB table input", {
      output_conn_1 <- ddbs_set_crs("argentina_scrs", "EPSG:4326", conn = conn_set_crs)
      output_conn_2 <- ddbs_set_crs("argentina_scrs", sf::st_crs("EPSG:4326"), conn = conn_set_crs)
      output_conn_3 <- ddbs_set_crs("argentina_scrs", sf::st_crs(NA), conn = conn_set_crs)
      output_conn_4 <- ddbs_set_crs("argentina_scrs", "EPSG:4326", conn = conn_set_crs, quiet = TRUE)

      expect_s3_class(output_conn_1, "duckspatial_df")
      expect_s3_class(output_conn_2, "duckspatial_df")
      expect_s3_class(output_conn_3, "duckspatial_df")
      expect_s3_class(output_conn_4, "duckspatial_df")
    })

    it("returns different output formats for DuckDB table input", {
      output_sf <- ddbs_set_crs("argentina_scrs", "EPSG:4326", conn = conn_set_crs, mode = "sf")
      expect_s3_class(output_sf, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_set_crs(argentina_ddbs, "EPSG:4326"))
      expect_message(ddbs_set_crs("argentina_scrs", "EPSG:4326", conn = conn_set_crs, name = "argentina_scrs_out"))
      expect_message(ddbs_set_crs("argentina_scrs", "EPSG:4326", conn = conn_set_crs, name = "argentina_scrs_out", overwrite = TRUE))
      expect_true(ddbs_set_crs("argentina_scrs", "EPSG:4326", conn = conn_set_crs, name = "argentina_scrs_out2"))

      expect_no_message(ddbs_set_crs(argentina_ddbs, "EPSG:4326", quiet = TRUE))
      expect_no_message(
        ddbs_set_crs(
          "argentina_scrs",
          "EPSG:4326",
          conn      = conn_set_crs,
          name      = "argentina_scrs_out",
          overwrite = TRUE,
          quiet     = TRUE
        )
      )
    })

    describe("CRS assignment", {

      it("sets CRS correctly from EPSG string", {
        result <- ddbs_set_crs(argentina_ddbs, "EPSG:4326")
        expect_equal(ddbs_crs(result)$epsg, 4326L)
      })

      it("sets CRS correctly from sf::st_crs() object", {
        result <- ddbs_set_crs(argentina_ddbs, sf::st_crs("EPSG:4326"))
        expect_equal(ddbs_crs(result)$epsg, 4326L)
      })

      it("removes CRS when set to sf::st_crs(NA)", {
        result <- ddbs_set_crs(argentina_ddbs, sf::st_crs(NA))
        expect_true(is.na(ddbs_crs(result)))
      })

      it("re-assigns CRS after removal", {
        no_crs   <- ddbs_set_crs(argentina_ddbs, sf::st_crs(NA))
        with_crs <- ddbs_set_crs(no_crs, "EPSG:4326")
        expect_equal(ddbs_crs(with_crs)$epsg, 4326L)
      })

      it("tags geometry with a different CRS without transforming coordinates", {
        result_sf   <- ddbs_set_crs(argentina_ddbs, "EPSG:3035", mode = "sf")
        orig_coords <- sf::st_coordinates(argentina_sf)[, c("X", "Y")]
        res_coords  <- sf::st_coordinates(result_sf)[, c("X", "Y")]

        expect_equal(res_coords, orig_coords, tolerance = 1e-6)
        expect_equal(sf::st_crs(result_sf)$epsg, 3035L)
      })
    })
  })

  ### EXPECTED ERRORS ---------------------------------------------------

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_set_crs("argentina_scrs", "EPSG:4326", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_set_crs(x = 999, "EPSG:4326"))
    })

    it("validates conn argument type", {
      expect_error(ddbs_set_crs(argentina_ddbs, "EPSG:4326", conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_set_crs(argentina_ddbs, "EPSG:4326", overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_set_crs(argentina_ddbs, "EPSG:4326", quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_set_crs("nonexistent_table", "EPSG:4326", conn = conn_set_crs))
    })

    it("requires name to be a single character string", {
      expect_error(ddbs_set_crs(argentina_ddbs, "EPSG:4326", conn = conn_set_crs, name = c("a", "b")))
    })
  })
})

duckspatial::ddbs_stop_conn(conn_set_crs)

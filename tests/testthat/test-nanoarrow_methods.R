# Set up --------------------------------------------------------------

testthat::skip_if_not_installed("nanoarrow")
testthat::skip_if_not_installed("geoarrow")
testthat::skip_if_not_installed("arrow")

# Use nc_ddbs from setup.R
# North Carolina data has 100 features and 14 columns (geom is #14)

# 1. as_nanoarrow_array_stream.duckspatial_df ---------------------------

describe("as_nanoarrow_array_stream.duckspatial_df()", {

  it("produces a valid nanoarrow_array_stream with geoarrow.wkb extension", {
    # Default path (native = FALSE)
    stream <- nanoarrow::as_nanoarrow_array_stream(nc_ddbs)
    expect_s3_class(stream, "nanoarrow_array_stream")
    
    schema <- stream$get_schema()
    geom_col <- attr(nc_ddbs, "sf_column")
    
    # Find geometry child in the schema
    child_names <- vapply(schema$children, function(x) x$name, character(1))
    geom_idx <- which(child_names == geom_col)
    
    expect_length(geom_idx, 1)
    geom_schema <- schema$children[[geom_idx]]
    
    # Check for geoarrow.wkb metadata
    expect_equal(geom_schema$metadata[["ARROW:extension:name"]], "geoarrow.wkb")
    
    # Check CRS in metadata
    ext_meta <- geom_schema$metadata[["ARROW:extension:metadata"]]
    expect_true(nchar(ext_meta) > 0)
    expect_true(grepl("crs", ext_meta))
    
    # Clean up
    stream$release()
  })

  it("works with native = TRUE (materialized path)", {
    # Skip if sf can't be loaded (unlikely here but safe)
    testthat::skip_if_not_installed("sf")
    
    stream <- nanoarrow::as_nanoarrow_array_stream(nc_ddbs, native = TRUE)
    expect_s3_class(stream, "nanoarrow_array_stream")
    
    schema <- stream$get_schema()
    child_names <- vapply(schema$children, function(x) x$name, character(1))
    geom_idx <- which(child_names == attr(nc_ddbs, "sf_column"))
    
    geom_schema <- schema$children[[geom_idx]]
    
    # native = TRUE should produce native geoarrow types (e.g. geoarrow.polygon)
    # depending on the geometry type of the input
    ext_name <- geom_schema$metadata[["ARROW:extension:name"]]
    expect_true(grepl("^geoarrow\\.", ext_name))
    
    stream$release()
  })

  it("works with geometry_schema (Native layout)", {
    # Request a native point schema
    target_schema <- geoarrow::geoarrow_point()
    # Use points_ddbs instead of nc_ddbs (polygons) to avoid conversion error
    stream <- nanoarrow::as_nanoarrow_array_stream(points_ddbs, geometry_schema = target_schema)
    
    expect_s3_class(stream, "nanoarrow_array_stream")
    schema <- stream$get_schema()
    geom_col <- attr(points_ddbs, "sf_column")
    child_names <- vapply(schema$children, function(x) x$name, character(1))
    geom_idx <- which(child_names == geom_col)
    geom_schema <- schema$children[[geom_idx]]
    
    expect_equal(geom_schema$metadata[["ARROW:extension:name"]], "geoarrow.wkb")
    
    stream$release()
  })

  it("errors if connection is missing", {
    # Create a dummy duckspatial_df without a connection
    x_bad <- structure(
        list(),
        class = c("duckspatial_df", "tbl_duckdb_connection", "tbl_dbi", "tbl_sql", "tbl_lazy", "tbl"),
        sf_column = "geom",
        crs = sf::st_crs(4326)
    )
    # Ensure remote_con(x_bad) is NULL
    
    expect_error(
      nanoarrow::as_nanoarrow_array_stream(x_bad),
      "connection is missing"
    )
  })

  it("errors if geometry column is missing from the query results", {
    # Point the sf_column attribute to something that doesn't exist
    nc_broken <- nc_ddbs
    attr(nc_broken, "sf_column") <- "non_existent_geom"
    
    # This might error in mutate (DuckDB Binder Error) or in our geom_found check
    expect_error(
      nanoarrow::as_nanoarrow_array_stream(nc_broken)
    )
  })

})

# 2. infer_geoarrow_schema.duckspatial_df -------------------------------

describe("infer_geoarrow_schema.duckspatial_df()", {

  it("returns a geoarrow.wkb schema", {
    schema <- geoarrow::infer_geoarrow_schema(nc_ddbs)
    expect_s3_class(schema, "nanoarrow_schema")
    expect_equal(schema$metadata[["ARROW:extension:name"]], "geoarrow.wkb")
  })

})

# 3. as_record_batch_reader.duckspatial_df ------------------------------

describe("as_record_batch_reader.duckspatial_df()", {

  it("returns an arrow RecordBatchReader", {
    reader <- arrow::as_record_batch_reader(nc_ddbs)
    expect_s3_class(reader, "RecordBatchReader")
    
    # Verify the reader's schema has the extension
    geom_col <- attr(nc_ddbs, "sf_column")
    field <- reader$schema$GetFieldByName(geom_col)
    
    # In the arrow R package, extension metadata is mapped to the type's extension_name()
    expect_true(inherits(field$type, "ExtensionType"))
    expect_equal(field$type$extension_name(), "geoarrow.wkb")
  })

})

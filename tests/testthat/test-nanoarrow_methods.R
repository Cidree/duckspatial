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

  native_extension <- function(x) {
    stream <- nanoarrow::as_nanoarrow_array_stream(x, native = TRUE)
    on.exit(stream$release())

    schema <- stream$get_schema()
    child_names <- vapply(schema$children, function(x) x$name, character(1))
    geom_idx <- which(child_names == attr(x, "sf_column"))

    geom_schema <- schema$children[[geom_idx]]
    geom_schema$metadata[["ARROW:extension:name"]]
  }

  it("produces native geometry layouts when native = TRUE", {
    expect_equal(native_extension(points_ddbs), "geoarrow.point")
    expect_equal(native_extension(rivers_ddbs), "geoarrow.linestring")
    expect_equal(native_extension(nc_ddbs), "geoarrow.multipolygon")
  })

  it("uses chunk_size for streamed record batches", {
    stream <- nanoarrow::as_nanoarrow_array_stream(
      points_ddbs,
      native = TRUE,
      chunk_size = 200
    )
    on.exit(stream$release())

    table <- arrow::as_arrow_table(stream)
    expect_equal(table$num_rows, 1000)
    expect_equal(table[["id"]]$num_chunks, 5)
    expect_equal(table[[attr(points_ddbs, "sf_column")]]$num_chunks, 5)
  })

  it("writes Arrow IPC for results larger than one Arrow chunk", {
    # Arrow chunks the fetched table at 1e6 rows. Converting the geometry
    # column into a single array leaves every batch after the first holding a
    # sliced geometry child with a nonzero offset, which the IPC writer
    # rejects, so anything over one chunk could not be serialised at all.
    skip_on_cran()
    conn <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
    duckspatial::ddbs_load(conn)

    DBI::dbExecute(conn, paste(
      "CREATE TABLE wide AS SELECT i AS id,",
      "ST_Point(i * 0.000001, i * 0.000001) AS geom",
      "FROM range(2000000) s(i)"
    ))

    x <- duckspatial::as_duckspatial_df(
      dplyr::tbl(conn, "wide"),
      crs = sf::st_crs(4326)
    )
    stream <- nanoarrow::as_nanoarrow_array_stream(x, native = TRUE)

    connection <- rawConnection(raw(0), "w")
    on.exit(close(connection), add = TRUE)
    expect_no_error(nanoarrow::write_nanoarrow(stream, connection))
    expect_gt(length(rawConnectionValue(connection)), 0)
  })

  it("preserves non-geometry column types when native = TRUE", {
    # The native path must replace only the geometry column. Rebuilding the
    # table from R vectors re-infers every other column's type, which silently
    # narrows int64 to int32 whenever the values happen to fit.
    conn <- DBI::dbConnect(duckdb::duckdb())
    on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
    duckspatial::ddbs_load(conn)

    DBI::dbExecute(conn, paste(
      "CREATE TABLE typed AS SELECT",
      "i AS small_id,",
      "3000000000 + i AS large_id,",
      "i::VARCHAR AS label,",
      "ST_Point(i * 0.01, i * 0.01) AS geom",
      "FROM range(10) s(i)"
    ))

    typed <- function() {
      duckspatial::as_duckspatial_df(
        dplyr::tbl(conn, "typed"),
        crs = sf::st_crs(4326)
      )
    }

    stream <- nanoarrow::as_nanoarrow_array_stream(typed(), native = TRUE)
    on.exit(stream$release(), add = TRUE)
    schema <- stream$get_schema()

    # "l" is int64, "i" is int32, "u" is utf8
    expect_equal(schema$children$small_id$format, "l")
    expect_equal(schema$children$large_id$format, "l")
    expect_equal(schema$children$label$format, "u")
    expect_equal(
      schema$children$geom$metadata[["ARROW:extension:name"]],
      "geoarrow.point"
    )

    values <- as.data.frame(
      nanoarrow::as_nanoarrow_array_stream(typed(), native = TRUE)
    )
    expect_equal(nrow(values), 10)
    expect_equal(as.numeric(values$small_id), 0:9)
    expect_equal(as.numeric(values$large_id), 3000000000 + 0:9)
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

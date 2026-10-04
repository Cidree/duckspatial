#' Convert a duckspatial_df to a nanoarrow_array_stream
#'
#' @param x A \code{duckspatial_df} object
#' @param ... Additional arguments passed to downstream methods
#' @param schema Optional target schema for the entire stream.
#' @param native If TRUE, transforms WKB to a "Native" GeoArrow layout (e.g., 
#'   Point, Polygon) using optimized Arrow-to-Arrow kernels. This layout is 
#'   optimized for high-performance rendering in tools like Deck.GL.
#' @param chunk_size Maximum number of rows in each Arrow record batch.
#'
#' @return A \code{nanoarrow_array_stream}
#' @exportS3Method nanoarrow::as_nanoarrow_array_stream duckspatial_df
as_nanoarrow_array_stream.duckspatial_df <- function(x, ..., 
                                                     schema = NULL, 
                                                     native = FALSE,
                                                     chunk_size = 1e6) {
  
  geom_col <- attr(x, "sf_column") %||% "geom"
  conn <- dbplyr::remote_con(x)
  
  if (is.null(conn)) {
    cli::cli_abort("Cannot stream {.cls duckspatial_df}: connection is missing.")
  }

  # Strip class to treat as standard lazy table for further manipulation.
  x_lazy <- x
  class(x_lazy) <- setdiff(class(x_lazy), "duckspatial_df")

  # 1. Force DuckDB to output standard WKB binary
  x_wkb <- dplyr::mutate(
    x_lazy,
    !!rlang::sym(geom_col) := dbplyr::sql(glue::glue("ST_AsWKB({DBI::dbQuoteIdentifier(conn, geom_col)})"))
  )
  query_sql <- dbplyr::sql_render(x_wkb)

  # 2. Execute and get Arrow data
  res <- DBI::dbSendQuery(conn, query_sql, arrow = TRUE)
  arrow_obj <- duckdb::duckdb_fetch_arrow(res, chunk_size = chunk_size)
  
  # 3. Native Path: Transform WKB to Native GeoArrow entirely in Arrow memory
  if (isTRUE(native)) {
    tab <- arrow::as_arrow_table(arrow_obj)

    geometry_types <- unique(as.character(ddbs_geometry_type(
      x,
      by_feature = FALSE
    )))
    geometry_type <- if (all(geometry_types %in% c("POINT", "MULTIPOINT"))) {
      if ("MULTIPOINT" %in% geometry_types) "MULTIPOINT" else "POINT"
    } else if (all(geometry_types %in% c("LINESTRING", "MULTILINESTRING"))) {
      if ("MULTILINESTRING" %in% geometry_types) {
        "MULTILINESTRING"
      } else {
        "LINESTRING"
      }
    } else if (all(geometry_types %in% c("POLYGON", "MULTIPOLYGON"))) {
      if ("MULTIPOLYGON" %in% geometry_types) "MULTIPOLYGON" else "POLYGON"
    }

    if (is.null(geometry_type)) {
      cli::cli_abort("Cannot infer one native GeoArrow geometry type.")
    }

    target_geom_schema <- geoarrow::geoarrow_native(
      geometry_type,
      coord_type = "SEPARATE",
      crs = ddbs_crs(x)
    )
    
    # Cast WKB to Native layout using geoarrow kernels, replacing only the
    # geometry column. Every other column stays an Arrow array throughout:
    # `as.list()` on an Arrow table converts columns to R vectors, and
    # rebuilding from those re-infers their types, silently narrowing int64 to
    # int32 whenever the values happen to fit.
    tab_list <- stats::setNames(
      lapply(names(tab), function(column) tab[[column]]),
      names(tab)
    )

    # Convert one chunk at a time so the geometry column keeps the same chunk
    # layout as the rest of the table. Combining it into a single array instead
    # makes every record batch after the first hold a sliced geometry child
    # with a nonzero offset, which the Arrow IPC writer rejects
    # ("Cannot encode arrays with nonzero offset").
    # geoarrow::as_geoarrow_array needs a nanoarrow_array or wk object
    tab_list[[geom_col]] <- arrow::ChunkedArray$create(
      !!!lapply(tab[[geom_col]]$chunks, function(chunk) {
        arrow::as_arrow_array(
          geoarrow::as_geoarrow_array(
            nanoarrow::as_nanoarrow_array(chunk),
            schema = target_geom_schema
          )
        )
      })
    )

    new_tab <- arrow::arrow_table(!!!tab_list)
    return(nanoarrow::as_nanoarrow_array_stream(new_tab, schema = schema))
  }

  # 4. Lazy Path: Zero-copy metadata injection into the stream
  old_schema <- nanoarrow::as_nanoarrow_schema(arrow_obj$schema)
  
  if (!is.null(schema)) {
    new_schema <- nanoarrow::as_nanoarrow_schema(schema)
  } else {
    dots <- list(...)
    wkb_args <- dots[names(dots) %in% c("edges")]
    
    ga_type <- do.call(geoarrow::geoarrow_wkb, c(list(crs = ddbs_crs(x)), wkb_args))
    ga_schema <- nanoarrow::as_nanoarrow_schema(ga_type)
    
    geom_found <- FALSE
    new_children <- lapply(old_schema$children, function(child) {
      if (child$name == geom_col) {
        geom_found <<- TRUE
        child_new <- ga_schema
        child_new$name <- child$name
        child_new$flags <- child$flags
        child_new
      } else {
        child
      }
    })
    
    if (!geom_found) {
      cli::cli_abort("Geometry column {.field {geom_col}} not found in query results.")
    }
    
    new_schema <- nanoarrow::nanoarrow_schema_modify(old_schema, list(children = new_children))
  }
  
  nanoarrow::as_nanoarrow_array_stream(arrow_obj, schema = new_schema)
}

#' @exportS3Method arrow::as_arrow_table duckspatial_df
as_arrow_table.duckspatial_df <- function(x, ...) {
  # Delegate to our optimized stream, then collect to a table
  stream <- nanoarrow::as_nanoarrow_array_stream(x, ...)
  arrow::as_arrow_table(stream)
}

#' @exportS3Method geoarrow::infer_geoarrow_schema duckspatial_df
infer_geoarrow_schema.duckspatial_df <- function(x, ...) {
  dots <- list(...)
  wkb_args <- dots[names(dots) %in% c("edges")]
  nanoarrow::as_nanoarrow_schema(do.call(geoarrow::geoarrow_wkb, c(list(crs = ddbs_crs(x)), wkb_args)))
}

#' @exportS3Method arrow::as_record_batch_reader duckspatial_df
as_record_batch_reader.duckspatial_df <- function(x, ...) {
  # Delegate to nanoarrow which handles the DuckDB/Arrow interface and metadata injection
  stream <- nanoarrow::as_nanoarrow_array_stream(x, ...)
  # Use the arrow package's importer to ensure C-stream metadata is respected
  arrow::RecordBatchReader$import_from_c(stream)
}

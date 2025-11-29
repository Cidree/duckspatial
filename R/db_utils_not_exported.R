
## dbConnCheck

#' Check if a supported DuckDB connection
#'
#' @template conn
#'
#' @keywords internal
#' @returns TRUE (invisibly) for successful import
dbConnCheck <- function(conn) {
    if (inherits(conn, "duckdb_connection")) {
        return(invisible(TRUE))

    } else if (is.null(conn)) { return(invisible(FALSE))

    } else {
        cli::cli_abort("'conn' must be connection object: <duckdb_connection> from `duckdb`")
    }
}

#' Get column names in a DuckDB database
#'
#' @template conn
#' @param x name of the table
#' @param rest whether to return geometry column name, of the rest of the columns
#'
#' @keywords internal
#' @returns name of the geometry column of a table
get_geom_name <- function(conn, x, rest = FALSE) {
    info_tbl <- DBI::dbGetQuery(conn, glue::glue("PRAGMA table_info('{x}');"))
    if (rest) info_tbl[!info_tbl$type == "GEOMETRY", "name"] else info_tbl[info_tbl$type == "GEOMETRY", "name"]
}


#' Get names for the query
#'
#' @param name table name
#'
#' @keywords internal
#' @returns list with fixed names
get_query_name <- function(name) {
    if (length(name) == 2) {
        table_name <- name[2]
        schema_name <- name[1]
        query_name <- paste0(name, collapse = ".")
    } else {
        table_name   <- name
        schema_name <- "main"
        query_name <- name
    }
    list(
        table_name = table_name,
        schema_name = schema_name,
        query_name = query_name
    )
}




#' Converts from data frame to sf
#'
#' Converts a table that has been read from DuckDB into an sf object
#'
#' @param data a tibble or data frame
#' @template crs
#' @param x_geom name of geometry
#'
#' @keywords internal
#' @returns sf
convert_to_sf <- function(data, crs, crs_column, x_geom) {
    if (is.null(crs)) {
        if (is.null(crs_column)) {
            data_sf <- data |>
                sf::st_as_sf(wkt = x_geom)
        } else {
            if (crs_column %in% names(data)) {
                data_sf <- data |>
                    sf::st_as_sf(wkt = x_geom, crs = data[1, crs_column])
                data_sf <- data_sf[, -which(names(data_sf) == crs_column)]
            } else {
                cli::cli_alert_warning("No CRS found for the imported table.")
                data_sf <- data |>
                    sf::st_as_sf(wkt = x_geom)
            }
        }

    } else {
        data_sf <- data |>
            sf::st_as_sf(wkt = x_geom, crs = crs)
    }

}





#' Gets predicate name
#'
#' Gets a full predicate name from the shorter version
#'
#' @template predicate
#'
#' @keywords internal
#' @returns character
get_st_predicate <- function(predicate) {
    switch(predicate,
      "intersects"   = "ST_Intersects",
      "touches"      = "ST_Touches",
      "contains"     = "ST_Contains",
      "within"       = "ST_Within",  ## TODO -> add distance argument
      "disjoint"     = "ST_Disjoint",
      "equals"       = "ST_Equals",
      "overlaps"     = "ST_Overlaps",
      "crosses"      = "ST_Crosses",
      "intersects_extent" = "ST_Intersects_Extent",
      cli::cli_abort(
          "Predicate should be one of <intersects>, <touches>, <contains>,
          <within>, <disjoin>, <equals>, <overlaps>, <crosses>, or <intersects_extent>"
        )
      )
}






#' Converts from data frame to sf using native geoarrow
#'
#' Converts a table that has been read from DuckDB into an sf object.
#' Optimized to handle Arrow-native binary streams using wk and geoarrow.
#'
#' @param data a tibble or data frame
#' @template crs
#' @param x_geom name of geometry column
#'
#' @keywords internal
#' @returns sf
convert_to_sf_native_geoarrow <- function(data, crs, crs_column, x_geom) {
  
  # 1. Resolve CRS
  # If CRS is passed explicitly, use it.
  # Otherwise, try to find it in the dataframe column 'crs_column'
  target_crs <- crs
  if (is.null(target_crs)) {
    if (!is.null(crs_column) && crs_column %in% names(data)) {
      # Assume CRS is consistent across the table, take first non-NA
      val <- stats::na.omit(data[[crs_column]])[1]
      if (!is.na(val)) target_crs <- as.character(val)
      
      # Remove the CRS column from output
      data[[crs_column]] <- NULL
    }
  }
  
  # 2. Check Geometry Type and Convert
  geom_data <- data[[x_geom]]
  
  if (inherits(geom_data, "blob") || is.list(geom_data)) {
    # --- FAST PATH: Binary/Arrow Data ---
    # This handles WKB blobs from DuckDB (Native GeoArrow or ST_AsWKB)
    
    # Strip attributes (like 'arrow_binary' or 'blob') to ensure it's a clean list for wk
    attributes(geom_data) <- NULL
    
    # Verify it's not empty and contains raw vectors (WKB)
    # If it's a list of raw vectors, use wk::new_wk_wkb -> geoarrow
    if (length(geom_data) > 0 && is.raw(geom_data[[1]])) {
      # Wrap as WKB
      wkb_obj <- wk::new_wk_wkb(geom_data)
      # Convert to GeoArrow Vector (Zero-copy optimized)
      ga_vctr <- geoarrow::as_geoarrow_vctr(wkb_obj)
      # Materialize as SFC (Simple Feature Column)
      data[[x_geom]] <- sf::st_as_sfc(ga_vctr)
    } else {
      # Fallback: Try converting directly (e.g., if it's already a geoarrow list structure)
      # This handles cases where DuckDB sends native arrow geometry structures
      tryCatch({
        ga_vctr <- geoarrow::as_geoarrow_vctr(geom_data)
        data[[x_geom]] <- sf::st_as_sfc(ga_vctr)
      }, error = function(e) {
        # Final fallback: if all else fails, try standard sf blob reading
        data[[x_geom]] <- sf::st_as_sfc(structure(geom_data, class = "WKB"))
      })
    }
    
  } else if (is.character(geom_data)) {
    # --- SLOW PATH: WKT Strings ---
    # Used if the query explicitly used ST_AsText() or older DuckDB versions
    data[[x_geom]] <- sf::st_as_sfc(geom_data)
  }
  
  # 3. Construct SF Object
  # Use st_as_sf with the pre-converted geometry column
  # We explicitly set the geometry column name to handle cases where x_geom isn't "geometry"
  sf_obj <- sf::st_as_sf(data, sf_column_name = x_geom)
  
  # 4. Assign CRS if found
  if (!is.null(target_crs)) {
    sf::st_crs(sf_obj) <- sf::st_crs(target_crs)
  }
  
  return(sf_obj)
}
  
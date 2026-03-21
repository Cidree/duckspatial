
#' Check CRS of spatial objects or database tables
#'
#' This is an S3 generic that extracts CRS information from various spatial objects.
#'
#' @param x An object containing spatial data. Can be:
#'   - \code{duckspatial_df}: Lazy spatial data frame (CRS from attributes)
#'   - \code{sf}: sf object (CRS from sf metadata)
#'   - \code{character}: Name of table in database (requires \code{conn})
#' @param ... Additional arguments passed to methods
#'
#' @returns CRS object from \code{sf} package
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' # Method 1: duckspatial_df objects
#' nc <- ddbs_open_dataset(system.file("shape/nc.shp", package = "sf"))
#' ddbs_crs(nc)
#'
#' # Method 2: sf objects
#' nc_sf <- st_read(system.file("shape/nc.shp", package = "sf"))
#' ddbs_crs(nc_sf)
#'
#' # Method 3: table name in database
#' conn <- ddbs_create_conn(dbdir = "memory")
#' ddbs_write_table(conn, nc_sf, "nc_table")
#' ddbs_crs(conn, "nc_table")
#' ddbs_stop_conn(conn)
#' }
ddbs_crs <- function(x, ...) {
  UseMethod("ddbs_crs")
}

#' @export
#' @rdname ddbs_crs
ddbs_crs.duckspatial_df <- function(x, ...) {
  crs <- attr(x, "crs")
  if (is.null(crs)) {
    return(sf::st_crs(NA))
  }
  crs
}

#' @export
#' @rdname ddbs_crs
ddbs_crs.sf <- function(x, ...) {
  sf::st_crs(x)
}

#' @export
#' @rdname ddbs_crs
ddbs_crs.tbl_duckdb_connection <- function(x, ...) {
  # Try to auto-detect CRS from view SQL (for duckdbfs::open_dataset and similar)
  conn <- dbplyr::remote_con(x)
  
  # Strategy 1: Try to get view SQL from duckdb_views()
  view_sql <- tryCatch({
    table_name <- dbplyr::remote_name(x)
    
    if (!is.null(table_name) && !inherits(table_name, "sql")) {
      result <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT sql FROM duckdb_views() WHERE view_name = '{table_name}'"
      ))
      if (nrow(result) > 0) result$sql else NULL
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  # Strategy 2: If no view found, render the query SQL
  if (is.null(view_sql)) {
    view_sql <- tryCatch({
      as.character(dbplyr::sql_render(x, con = conn))
    }, error = function(e) NULL)
  }
  
  # Extract file path from ST_Read() calls
  if (!is.null(view_sql)) {
    # Look for ST_Read('path') or st_read("path") patterns
    path_match <- regexpr("(?:ST_Read|st_read)\\s*\\(\\s*['\"]([^'\"]+)['\"]", 
                          view_sql, 
                          perl = TRUE, 
                          ignore.case = TRUE)
    
    if (path_match[1] > 0) {
      # Extract the captured group (the path)
      start <- attr(path_match, "capture.start")[1,1]
      length <- attr(path_match, "capture.length")[1,1]
      file_path <- substr(view_sql, start, start + length - 1)
      
      # Use st_read_meta to get CRS
      crs <- tryCatch({
        get_file_crs(file_path, conn)
      }, error = function(e) NULL)
      
      if (!is.null(crs)) {
        return(crs)
      }
    }
  }
  
  # Fallback: return NA CRS
  cli::cli_warn(c(
    "Could not auto-detect CRS for {.cls tbl_duckdb_connection} object.",
    "i" = "The object may not be a view created from a spatial file.",
    "i" = "Use {.code as_duckspatial_df(x, crs = ...)} to set CRS explicitly."
  ))
  sf::st_crs(NA)
}

#' @export
#' @rdname ddbs_crs
#' @param conn A DuckDB connection (required for character method)
ddbs_crs.character <- function(x, conn, ...) {

    # 0. Check if x is in AUTH:CODE format (e.g., "EPSG:4326")
    if (length(x) == 1 && grepl("^[A-Z]+:[0-9]+$", x)) {
        return(sf::st_crs(x))
    }
  
    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    
    name <- x
    
    ## convenient names of table and/or schema.table
    x_list <- get_query_list(x, conn)

    ## Check if table name exists in Tables OR Arrow Views
    # Use SQL check to catch temporary views which might not show up in dbListTables
    check_query <- glue::glue("
      SELECT 1 FROM information_schema.tables 
      WHERE table_name = '{x_list$query_name}' 
      UNION 
      SELECT 1 FROM duckdb_views() 
      WHERE view_name = '{x_list$query_name}'
      LIMIT 1
    ")
    
    table_exists <- tryCatch({
      nrow(DBI::dbGetQuery(conn, check_query)) > 0
    }, error = function(e) FALSE)
    
    arrow_exists <- FALSE

    if (!table_exists) {
        arrow_list <- try(duckdb::duckdb_list_arrow(conn), silent = TRUE)
        if (!inherits(arrow_list, "try-error") && x_list$table_name %in% arrow_list) {
            arrow_exists <- TRUE
        }
    }

    if (!table_exists && !arrow_exists) {
        cli::cli_abort("The provided name is not present in the database.")
    }
    
    ## Get CRS from the table
    crs_data <- tryCatch({
      geom_name <- get_geom_name(conn, x_list$query_name)
      DBI::dbGetQuery(
        conn, glue::glue("SELECT ST_CRS({geom_name}) AS crs FROM {x_list$query_name} LIMIT 1;")
      ) |> as.character()
    }, error = function(e) {
      NULL
    })
  
    ## TODO - Review below for duckdb 1.5 - is this necessary with ST_CRS()?    
    if (is.null(crs_data)) {
      # Fallback: Try to auto-detect from view definition (like tbl_duckdb_connection method)
      view_sql <- tryCatch({
        result <- DBI::dbGetQuery(conn, glue::glue(
          "SELECT sql FROM duckdb_views() WHERE view_name = '{x_list$table_name}'"
        ))
        if (nrow(result) > 0) result$sql else NULL
      }, error = function(e) NULL)
      
      if (!is.null(view_sql)) {
         path_match <- regexpr("(?:ST_Read|st_read)\\s*\\(\\s*['\"]([^'\"]+)['\"]", 
                              view_sql, perl = TRUE, ignore.case = TRUE)
         if (path_match[1] > 0) {
            start <- attr(path_match, "capture.start")[1,1]
            length <- attr(path_match, "capture.length")[1,1]
            file_path <- substr(view_sql, start, start + length - 1)
            
            crs <- tryCatch({ get_file_crs(file_path, conn) }, error = function(e) NULL)
            if (!is.null(crs)) return(crs)
         }
      }
    
      cli::cli_warn("CRS could not be auto-detected.")
      return(sf::st_crs(NA))
    }

    # 2. Return CRS
    return(sf::st_crs(crs_data))
}

#' @export
#' @rdname ddbs_crs
#' @param name Table name (for backward compatibility when first arg is connection)
ddbs_crs.duckdb_connection <- function(x, name, ...) {
  # Backward compatibility: ddbs_crs(conn, name)
  if (missing(name)) {
    cli::cli_abort("Must provide {.arg name} when calling {.fun ddbs_crs} with a connection.")
  }
  ddbs_crs.character(name, conn = x, ...)
}


#' @export
#' @rdname ddbs_crs
ddbs_crs.numeric <- function(x, ...) {
  # Convert numeric EPSG code to CRS
  # Assumes EPSG authority by default
  if (length(x) != 1) {
      cli::cli_abort("Numeric CRS input must be a single value (EPSG code).")
  }
  
  if (x < 1 || x != as.integer(x)) {
      cli::cli_abort("CRS code must be a positive integer.")
  }

  # Extract the CRS
  crs_x <- sf::st_crs(as.integer(x))
  
  # If the CRS doesn't exist, the previous function returns NA
  if (is.na(crs_x)) {
    cli::cli_abort("CRS code wasn't found.")
  } else {
    return(crs_x)
  }

}


#' @export
#' @rdname ddbs_crs
ddbs_crs.crs <- function(x, ...) {
  return(x)
}


#' @export
#' @rdname ddbs_crs
ddbs_crs.data.frame <- function(x, ...) {
  return(sf::st_crs(NA))
}


#' @export
#' @rdname ddbs_crs
ddbs_crs.default <- function(x, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a duckspatial_df, sf object, tbl_duckdb_connection, or character table name.",
    "i" = "You provided an object of class: {.cls {class(x)}}"
  ))
}

#' Get or set global duckspatial options
#'
#' @param output_type Character string. Controls the default return type for [ddbs_collect].
#'   Must be one of:
#'   \itemize{
#'     \item \code{"sf"} (default): Eagerly collected \code{sf} object (in-memory).
#'     \item \code{"tibble"}: Eagerly collected \code{tibble} without geometry.
#'     \item \code{"raw"}: Eagerly collected \code{tibble} with geometry as raw WKB bytes.
#'     \item \code{"geoarrow"}: Eagerly collected \code{tibble} with geometry as \code{geoarrow_vctr}.
#'   }
#'   If \code{NULL} (the default), the existing option is not changed.
#' @param mode Character. Controls the return type. Options:
#'   \itemize{
#'     \item \code{"duckspatial"} (default): Lazy spatial data frame backed by dbplyr/DuckDB
#'     \item \code{"sf"}: Eagerly collected sf object (uses memory)
#'   }
#'   If \code{NULL} (the default), the existing option is not changed.
#'
#' @return Invisibly returns a list containing the currently set options.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set default mode to geoarrow
#' ddbs_options(mode = "geoarrow")
#' 
#' # Set default output to tibble
#' ddbs_options(output_type = "tibble")
#'
#' # Check current settings
#' ddbs_options()
#' }
ddbs_options <- function(output_type = NULL, mode = NULL) {
  
  # 1. SETTER logic
  if (!is.null(output_type)) {
    if (!output_type %in% c("sf", "tibble", "raw", "geoarrow")) {
      cli::cli_abort(paste0(
        "Invalid output_type: {.val {output_type}}. ",
        "Must be one of {.val sf}, {.val tibble}, {.val raw}, or {.val geoarrow}."
      ))
    }
    options(duckspatial.output_type = output_type)
  }

  if (!is.null(mode)) {
    if (!mode %in% c("duckspatial", "sf")) {
      cli::cli_abort(paste0(
        "Invalid mode: {.val {mode}}. ",
        "Must be one of {.val duckspatial}, or {.val sf}."
      ))
    }
    options(duckspatial.mode = mode)
  }
  
  # 2. GETTER logic (Always retrieve current state)
  op <- list(
    duckspatial.output_type = getOption("duckspatial.output_type"),
    duckspatial.mode = getOption("duckspatial.mode")
  )
  
  # If we set a value, return invisibly. If just checking, return visibly.
  if (all(is.null(output_type), is.null(mode))) {
    op
  } else {
    invisible(op)
  }
}


#' Report duckspatial configuration status
#'
#' Displays useful information about the current configuration, including global options
#' and the status of the default DuckDB connection.
#'
#' @return Invisibly returns a list with the current status configuration.
#' @export
#'
#' @examples
#' ddbs_sitrep()
ddbs_sitrep <- function() {
  
  cli::cli_h1("duckspatial Status Report")
  
  # 1. Global Options
  cli::cli_h2("Global Options")
  
  out_type <- getOption("duckspatial.output_type", "duckspatial_df (default)")
  mode     <- getOption("duckspatial.mode", "duckspatial (default)")
  cli::cli_ul()
  cli::cli_li("Output Type: {.val {out_type}}")
  cli::cli_li("Mode: {.val {mode}}")
  cli::cli_end()
  
  # 2. Connection Status
  cli::cli_h2("Default Connection")
  
  conn <- getOption("duckspatial_conn", NULL)
  
  if (is.null(conn)) {
    cli::cli_alert_warning("No default connection active (will be created on demand).")
    conn_status <- "NULL"
  } else {
    is_valid <- DBI::dbIsValid(conn)
    if (is_valid) {
      cli::cli_alert_success("Active DuckDB connection found.")
      
      # Check if memory or file
      # Generic way to check might be difficult without specific duckdb info, 
      # but dbGetInfo might reveal it or simple "Active" is enough.
      # duckdb connections don't easily reveal their path via public API usually?
      # We can try inspection if needed, but "Active" is good enough.
      conn_status <- "Active"
    } else {
      cli::cli_alert_danger("Connection object exists but is invalid (disconnected).")
      conn_status <- "Invalid"
    }
  }
  
  invisible(list(
    output_type = out_type,
    connection_status = conn_status
  ))
}

#' Get or set connection resources
#'
#' Configure technical system settings for a DuckDB connection, such as memory limits
#' and CPU threads.
#'
#' @template conn
#' @template threads
#' @template memory_limit_gb
#'
#' @return For \code{ddbs_set_resources()}, invisibly returns a list containing the current system settings; for \code{ddbs_get_resources()}, visibly returns the same list for direct inspection.
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a connection
#' conn <- ddbs_create_conn()
#'
#' # Set resources: 1 thread and 4GB
#' ddbs_set_resources(conn, threads = 1, memory_limit_gb = 4)
#'
#' # Check current settings
#' ddbs_get_resources(conn)
#'
#' ddbs_stop_conn(conn)
#' }
ddbs_set_resources <- function(conn, threads = NULL, memory_limit_gb = NULL) {
  # 1. Checks
  dbConnCheck(conn)

  assert_threads(threads)
  assert_memory_limit_gb(memory_limit_gb)

  # 2. SETTER logic
  if (!is.null(threads)) {
    DBI::dbExecute(conn, sprintf("SET threads = %d;", as.integer(threads)))
  }

  if (!is.null(memory_limit_gb)) {
    DBI::dbExecute(conn, sprintf("SET memory_limit = '%.1fGB';", as.numeric(memory_limit_gb)))
  }

  # 3. Return current state
  invisible(ddbs_get_resources(conn))
}

#' @rdname ddbs_set_resources
#' @export
ddbs_get_resources <- function(conn) {
  # 1. Checks
  dbConnCheck(conn)

  # 2. Query settings
  settings <- DBI::dbGetQuery(conn, "
    SELECT name, value 
    FROM duckdb_settings() 
    WHERE name IN ('threads', 'memory_limit');
  ")

  # Format result
  res <- as.list(stats::setNames(settings$value, settings$name))
  
  # Parse numeric values for programmatic use
  if (!is.null(res$threads)) res$threads <- as.integer(res$threads)
  if (!is.null(res$memory_limit)) {
    res$memory_limit_gb <- parse_memory_limit_gb(res$memory_limit)
  }
  
  return(res)
}

#' Parse DuckDB memory limit string to numeric GB
#' 
#' @param mem_str Character string from DuckDB (e.g., "4GB", "3.7 GiB", "953.6 MiB")
#' @return Numeric value in GB
#' @noRd
parse_memory_limit_gb <- function(mem_str) {
  if (is.null(mem_str) || is.na(mem_str)) return(NA_real_)
  mem_str <- trimws(mem_str)
  
  # Extract numeric part and unit
  num_part <- as.numeric(sub("^([0-9.]+).*", "\\1", mem_str))
  unit_part <- toupper(sub("^[0-9.]+\\s*", "", mem_str))
  
  if (is.na(num_part)) return(NA_real_)
  
  # Conversion factors to GB (10^9 bytes)
  # DuckDB: KB/MB/GB/TB = 1000 base; KiB/MiB/GiB/TiB = 1024 base
  multipliers <- c(
    "TIB" = 1024^4 / 10^9, "TB" = 1000,
    "GIB" = 1024^3 / 10^9, "GB" = 1,
    "MIB" = 1024^2 / 10^9, "MB" = 1/1000,
    "KIB" = 1024 / 10^9,   "KB" = 1/10^6,
    "B"   = 1/10^9
  )
  
  m <- multipliers[unit_part]
  if (is.na(m)) m <- 1 # Default to GB if unit not found
  
  num_part * m
}

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
  
  # Try to parse numeric values where possible
  if (!is.null(res$threads)) res$threads <- as.integer(res$threads)
  
  return(res)
}

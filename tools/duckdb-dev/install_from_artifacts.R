#!/usr/bin/env Rscript

# =============================================================================
# Install DuckDB + Spatial Extension from GitHub Actions Artifacts (R Wrapper)
# =============================================================================
#
# Usage (from R console):
#   source("tools/duckdb-dev/install_from_artifacts.R")
#   install_artifacts()                     # Auto-detect latest, with confirmation
#   install_artifacts(force = TRUE)         # Skip confirmation prompt
#   install_artifacts(12345678)             # Specific run ID
#   install_artifacts(12345678, force = TRUE)
#
# Or from command line:
#   Rscript tools/duckdb-dev/install_from_artifacts.R
#   Rscript tools/duckdb-dev/install_from_artifacts.R 12345678
#
# =============================================================================

# Get the directory where this script is located
get_script_dir <- function() {
  # Try different methods to find the script location
  
  # Method 1: When run via Rscript command line
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  

  # Method 2: When sourced interactively
  if (sys.nframe() > 0) {
    for (i in seq_len(sys.nframe())) {
      if (!is.null(sys.frame(i)$ofile)) {
        return(dirname(normalizePath(sys.frame(i)$ofile)))
      }
    }
  }
  
  # Method 3: Fall back to working directory + expected path
  fallback <- file.path(getwd(), "tools", "duckdb-dev")
  if (dir.exists(fallback)) {
    return(fallback)
  }
  
  stop("Cannot determine script location. Please run from the repo root or provide the full path.")
}

install_artifacts <- function(run_id_or_url = NULL, force = FALSE) {
  script_dir <- get_script_dir()
  os_type <- Sys.info()["sysname"]
  
  # Check if duckdb is already installed and prompt for confirmation
  if (!force && interactive()) {
    tryCatch({
      if (requireNamespace("duckdb", quietly = TRUE)) {
        cat("==========================================\n")
        cat("Currently installed:\n")
        cat("==========================================\n")
        
        con <- DBI::dbConnect(duckdb::duckdb(config = list(allow_unsigned_extensions = "true")))
        # Don't use on.exit() here as it accumulates across the function execution
        
        v <- DBI::dbGetQuery(con, "PRAGMA version")
        cat("DuckDB R Package:\n")
        cat("  Version:   ", v$library_version, "\n")
        cat("  Source ID: ", v$source_id, "\n")
        
        tryCatch({
          DBI::dbExecute(con, "LOAD spatial")
          ext <- DBI::dbGetQuery(con, "SELECT extension_version FROM duckdb_extensions() WHERE extension_name='spatial'")
          if (nrow(ext) > 0) {
            cat("\nSpatial Extension:\n")
            cat("  Version:   ", ext$extension_version, "\n")
          }
        }, error = function(e) {
          cat("\nSpatial Extension: not installed\n")
        })
        
        cat("==========================================\n\n")
        
        DBI::dbDisconnect(con, shutdown = TRUE)
        
        confirm <- readline("Override with dev build? [y/N]: ")
        if (!tolower(confirm) %in% c("y", "yes")) {
          cat("Aborted.\n")
          return(invisible(0))
        }
        cat("\n")
      }
    }, error = function(e) {
      # duckdb not installed, no need to prompt
    })
  }
  
  # Build arguments - always pass force since we handled the prompt in R
  args_list <- c(if (os_type == "Windows") "-Force" else "--force")
  
  # Add run ID if provided
  if (!is.null(run_id_or_url)) {
    args_list <- c(args_list, as.character(run_id_or_url))
  }
  
  # Run the script and stream output to console (stdout = "" means pass through)
  exit_code <- if (os_type == "Windows") {
    script_path <- file.path(script_dir, "install_from_artifacts.ps1")
    if (!file.exists(script_path)) {
      stop("Cannot find install_from_artifacts.ps1 at: ", script_path)
    }
    system2("powershell", args = c("-ExecutionPolicy", "Bypass", "-File", script_path, args_list),
            stdout = "", stderr = "")
  } else {
    script_path <- file.path(script_dir, "install_from_artifacts.sh")
    if (!file.exists(script_path)) {
      stop("Cannot find install_from_artifacts.sh at: ", script_path)
    }
    system2("bash", args = c(script_path, args_list),
            stdout = "", stderr = "")
  }
  
  # Only print version info if installation was successful
  if (exit_code != 0) {
    cat("\nInstallation failed or no builds found. See error messages above.\n")
    return(invisible(exit_code))
  }
  
  # Print version information
  cat("\n")
  cat("==========================================\n")
  cat("Installed Versions\n")
  cat("==========================================\n")
  
  tryCatch({
    # Use a fresh R process to check the installed version
    # This avoids the issue where the current session has the old DLL loaded
    cmd <- paste0(
      "library(duckdb, quietly=TRUE); ",
      "con <- DBI::dbConnect(duckdb::duckdb(config=list(allow_unsigned_extensions='true'))); ",
      "v <- DBI::dbGetQuery(con, 'PRAGMA version'); ",
      "cat('DuckDB R Package:\\n'); ",
      "cat('  Version:   ', v$library_version, '\\n'); ",
      "cat('  Source ID: ', v$source_id, '\\n'); ",
      "tryCatch({ ",
      "  DBI::dbExecute(con, 'LOAD spatial'); ",
      "  ext <- DBI::dbGetQuery(con, \"SELECT extension_name, extension_version, install_mode FROM duckdb_extensions() WHERE extension_name = 'spatial'\"); ",
      "  if (nrow(ext) > 0) { ",
      "    cat('\\nSpatial Extension:\\n'); ",
      "    cat('  Version:   ', ext$extension_version, '\\n'); ",
      "    cat('  Mode:      ', ext$install_mode, '\\n'); ",
      "  } ",
      "}, error = function(e) { ",
      "  cat('\\nSpatial Extension: not installed (or load failed)\\n'); ",
      "}); ",
      "DBI::dbDisconnect(con, shutdown=TRUE);"
    )
    
    system2("Rscript", args = c("-e", shQuote(cmd)), stdout = "", stderr = "")
    
  }, error = function(e) {
    cat("Could not retrieve version info: ", conditionMessage(e), "\n")
  })
  
  cat("\nIMPORTANT: Restart your R session to use the new version.\n")
  cat("Every time you restart R session, you must enable unsigned extensions to use this dev build:\n")
  cat("  con <- DBI::dbConnect(duckdb::duckdb(config = list(allow_unsigned_extensions = \"true\")))\n")
  cat("  DBI::dbExecute(con, \"LOAD spatial\")\n")
}

# If run from command line
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    install_artifacts()
  } else {
    install_artifacts(args[1])
  }
}

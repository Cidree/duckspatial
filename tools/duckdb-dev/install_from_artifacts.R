#!/usr/bin/env Rscript

# =============================================================================
# Install DuckDB + Spatial Extension from GitHub Actions Artifacts (R Wrapper)
# =============================================================================
#
# Usage (from R console):
#   source("tools/duckdb-dev/install_from_artifacts.R")
#   install_dev_duckdb_spatial()                     # Auto-detect latest, with confirmation
#   install_dev_duckdb_spatial(force = TRUE)         # Skip confirmation prompt
#   install_dev_duckdb_spatial(12345678)             # Specific run ID
#   install_dev_duckdb_spatial(12345678, force = TRUE)
#
#   install_cran_duckdb_spatial()                    # Revert to CRAN version
#   check_duckdb_spatial_version()                   # Check installed versions
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

check_duckdb_spatial_version <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    cat("DuckDB R Package: Not installed\n")
    return(invisible(NULL))
  }

  tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(config = list(allow_unsigned_extensions = "true")))
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    
    v <- DBI::dbGetQuery(con, "PRAGMA version")
    cat("DuckDB R Package:\n")
    cat("  Version:   ", v$library_version, "\n")
    cat("  Source ID: ", v$source_id, "\n")
    
    tryCatch({
      DBI::dbExecute(con, "LOAD spatial")
      ext <- DBI::dbGetQuery(con, "SELECT extension_version, install_mode FROM duckdb_extensions() WHERE extension_name='spatial'")
      if (nrow(ext) > 0) {
        cat("\nSpatial Extension:\n")
        cat("  Version:   ", ext$extension_version, "\n")
        if (!is.null(ext$install_mode)) {
          cat("  Mode:      ", ext$install_mode, "\n")
        }
      }
    }, error = function(e) {
      cat("\nSpatial Extension: not installed (or load failed)\n")
    })
    
  }, error = function(e) {
    cat("Error checking version: ", conditionMessage(e), "\n")
  })
}

install_dev_duckdb_spatial <- function(run_id_or_url = NULL, force = FALSE) {
  script_dir <- get_script_dir()
  os_type <- Sys.info()["sysname"]
  
  # Check if duckdb is already installed and prompt for confirmation
  if (!force && interactive()) {
    tryCatch({
      if (requireNamespace("duckdb", quietly = TRUE)) {
        cat("==========================================\n")
        cat("Currently installed:\n")
        cat("==========================================\n")
        check_duckdb_spatial_version()
        cat("==========================================\n\n")
        
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
  
  # Use a fresh R process to verify
  # We assume this script is available at its relative path since we are in the project
  # If we can't find it easily for the child process, we might need a fallback.
  # Using the absolute path of THIS script file is safest.
  this_script_path <- normalizePath(file.path(script_dir, "install_from_artifacts.R"), mustWork = FALSE)
  
  cmd <- sprintf("source('%s'); check_duckdb_spatial_version()", this_script_path)
  system2("Rscript", args = c("-e", shQuote(cmd)), stdout = "", stderr = "")
   
  cat("==========================================\n")
  cat("\nIMPORTANT: Restart your R session to use the new version.\n")
  cat("Every time you restart R session, you must enable unsigned extensions to use this dev build:\n")
  cat("  con <- DBI::dbConnect(duckdb::duckdb(config = list(allow_unsigned_extensions = \"true\")))\n")
  cat("  DBI::dbExecute(con, \"LOAD spatial\")\n")
}

install_cran_duckdb_spatial <- function(force = FALSE) {
  # Check if duckdb is already installed and prompt for confirmation
  if (!force && interactive()) {
    tryCatch({
      if (requireNamespace("duckdb", quietly = TRUE)) {
        cat("==========================================\n")
        cat("Currently installed:\n")
        cat("==========================================\n")
        check_duckdb_spatial_version()
        cat("==========================================\n\n")
        
        confirm <- readline("Revert to CRAN version? [y/N]: ")
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

  cat("Installing DuckDB from CRAN...\n")
  install.packages("duckdb", repos = "https://cloud.r-project.org")
  
  cat("\nInstalling Spatial Extension...\n")
  
  # Use a fresh R process to verify and install spatial extension (ensuring clean state)
  cmd <- paste0(
      "if (!requireNamespace('duckdb', quietly=TRUE)) stop('DuckDB not found after install'); ",
      "con <- DBI::dbConnect(duckdb::duckdb(), dbdir=':memory:'); ",
      "tryCatch({ ",
      "  DBI::dbExecute(con, 'FORCE INSTALL spatial; LOAD spatial;'); ",
      "  DBI::dbDisconnect(con, shutdown=TRUE); ", # Close first
      "  cat('\\n'); ",
      "}, error = function(e) { ",
      "  cat('Error installing/loading spatial extension: ', conditionMessage(e), '\\n'); ",
      "});"
  )
  system2("Rscript", args = c("-e", shQuote(cmd)), stdout = "", stderr = "")

  # Check versions using our new function
  cat("\n==========================================\n")
  cat("Installed Versions\n")
  cat("==========================================\n")
  
  script_dir <- get_script_dir()
  this_script_path <- normalizePath(file.path(script_dir, "install_from_artifacts.R"), mustWork = FALSE)
  cmd_check <- sprintf("source('%s'); check_duckdb_spatial_version()", this_script_path)
  system2("Rscript", args = c("-e", shQuote(cmd_check)), stdout = "", stderr = "")
  
  cat("==========================================\n")
  cat("\nRestart your R session to use the new version.\n")
}

# If run from command line
# If run from command line directly (and not sourced)
if (!interactive()) {
  # Check if this script matches the --file argument to ensure we are the main script
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg)
    # Check if the script path ends with our filename (robustness check)
    if (grepl("install_from_artifacts.R$", script_path)) {
      args <- commandArgs(trailingOnly = TRUE)
      
      if (length(args) == 0) {
        install_dev_duckdb_spatial()
      } else {
        install_dev_duckdb_spatial(args[1])
      }
    }
  }
}

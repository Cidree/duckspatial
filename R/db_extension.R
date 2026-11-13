
#' Checks and installs the Spatial extension
#'
#' Checks if a spatial extension is available, and installs it in a DuckDB database
#'
#' @template conn
#' @param upgrade if TRUE, it upgrades the DuckDB extension to the latest version
#' @template quiet
#' @param extension name of the extension to install, default is "spatial"
#' @param repos optional character string naming the repository to install the
#' extension from (e.g. \code{"core"}, \code{"core_nightly"}, or
#' \code{"community"}); a URL or path can also be supplied. If \code{NULL}
#' (default), core extensions known to DuckDB are installed from \code{core}
#' and any other extension from \code{community}, falling back to the other
#' repository if that fails.
#' Switching an already-installed extension to a different repository requires
#' \code{upgrade = TRUE}. See
#' \url{https://duckdb.org/docs/stable/extensions/installing_extensions}.
#'
#' @returns TRUE (invisibly) for successful installation
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#' library(duckdb)
#'
#' # connect to in memory database
#' conn <- duckdb::dbConnect(duckdb::duckdb())
#'
#' # install the spatial extension
#' ddbs_install(conn)
#'
#' # disconnect from db
#' duckdb::dbDisconnect(conn)
#'
#' # install the h3 community extension
#' conn <- duckdb::dbConnect(duckdb::duckdb())
#' ddbs_install(conn, extension = "h3")
#' duckdb::dbDisconnect(conn)
#' }
ddbs_install <- function(
    conn,
    upgrade = FALSE,
    quiet = FALSE,
    extension = "spatial",
    repos = NULL
) {

    # 1. Check connection
    dbConnCheck(conn)

    # 2. Get extensions list
    ext <- DBI::dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    ## 2.1. Other checks
    if (!is.null(repos)) assert_character_scalar(repos, "repos")
    ## 2.2. Check if it's installed. When already installed and not upgrading,
    ## there is nothing to do; with `upgrade = TRUE` we fall through to a
    ## (FORCE) INSTALL. DuckDB's `duckdb_extensions()` exposes no reliable
    ## "needs upgrade" flag, so we always re-install on upgrade.
    target_ext <- ext[ext$extension_name == extension, ]
    if (nrow(target_ext) == 1 && target_ext$installed && !upgrade) {
        if (isFALSE(quiet)) {
            cli::cli_alert_info(
                "{extension} extension version {.val {target_ext$extension_version}} is already installed. Use {.code upgrade = TRUE} to upgrade."
            )
        }
        return(invisible(TRUE))
    }

    # Extension cannot be upgraded if it's already loaded. It will fail
    if (isTRUE(target_ext$loaded) && isTRUE(upgrade)) {
        cli::cli_abort("{extension} is already loaded in the connection. Upgrading the version is only allowed in non-loaded connections.")
    }

    # 3. Install/upgrade extension
    install_kw <- if (upgrade) "FORCE INSTALL" else "INSTALL"

    if (!is.null(repos)) {

        ## 3a. Explicit repository chosen by the user (no fallback). Named repos
        ## (core, core_nightly, community, ...) are bare identifiers; URLs/paths
        ## must be single-quoted.
        repos_sql <- if (grepl("^[A-Za-z0-9_]+$", repos)) repos else ddbs_quote_sql_string(conn, repos)

        installed <- tryCatch({
            suppressMessages(DBI::dbExecute(conn, glue::glue("{install_kw} {extension} FROM {repos_sql};")))
            repos
        }, error = function(e) {
            cli::cli_abort(c(
                "Failed to {if (upgrade) 'upgrade' else 'install'} the {extension} extension from the {.val {repos}} repository.",
                "x" = conditionMessage(e),
                "i" = "If {extension} is already installed from a different repository, set {.code upgrade = TRUE} to switch.",
                "i" = "Check the repository name: {.url https://duckdb.org/docs/stable/extensions/installing_extensions}"
            ))
        })

    } else {

        ## 3b. Default: try the likely repository first, then the other one.
        ## `duckdb_extensions()` lists every core extension known to this DuckDB
        ## build, installed or not, so an extension missing from it (or installed
        ## from community) is tried in community first. This avoids a download
        ## that is bound to fail: with duckdb 2.0.0 on Windows, a failed download
        ## can crash the R session later on.
        installed_from <- if ("installed_from" %in% names(target_ext)) target_ext$installed_from else character(0)
        is_core <- nrow(target_ext) == 1 && !identical(installed_from, "community")
        repos_order <- if (is_core) c("core", "community") else c("community", "core")

        errs <- list()
        installed <- NULL
        for (r in repos_order) {
            from_sql <- if (r == "core") "" else glue::glue(" FROM {r}")
            ok <- tryCatch({
                suppressMessages(DBI::dbExecute(conn, glue::glue("{install_kw} {extension}{from_sql};")))
                TRUE
            }, error = function(e) {
                errs[[r]] <<- conditionMessage(e)
                FALSE
            })
            if (ok) {
                installed <- r
                break
            }
        }

        if (is.null(installed)) {
            cli::cli_abort(c(
                "Failed to {if (upgrade) 'upgrade' else 'install'} the {extension} extension.",
                "x" = "core: {errs[['core']]}",
                "x" = "community: {errs[['community']]}",
                "i" = "It might not be available for this version of DuckDB, or the install location may not be writable.",
                "i" = "Check that the extension name is correct: {.url https://duckdb.org/docs/extensions/overview}"
            ))
        }
    }

    if (isFALSE(quiet)) {
        action <- if (upgrade) "upgraded" else "installed"
        repo_note <- if (!identical(installed, "core")) glue::glue(" (from {installed} repository)") else ""
        cli::cli_alert_success("{extension} extension {action}{repo_note}")
    }

    return(invisible(TRUE))
}


#' Loads the Spatial extension
#'
#' Checks if a spatial extension is installed, and loads it in a DuckDB database
#'
#' @template conn
#' @template quiet
#' @param extension name of the extension to load, default is "spatial"
#' @param create_macros if TRUE (default), it creates macros that allow
#'   some functions to be used within dplyr pipelines
#'
#' @returns TRUE (invisibly) for successful installation
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#' library(duckdb)
#'
#' ## connect to in memory database
#' conn <- duckdb::dbConnect(duckdb::duckdb())
#'
#' ## install the spatial exntesion
#' ddbs_install(conn)
#' ddbs_load(conn)
#'
#' ## disconnect from db
#' duckdb::dbDisconnect(conn)
#' }
ddbs_load <- function(
    conn, 
    quiet = FALSE, 
    extension = "spatial",
    create_macros = TRUE
) {

    # 1. Checks

    ## 1.1. Check connection
    dbConnCheck(conn)

    ## 1.2. Get extensions list
    ext <- DBI::dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    ## 1.3. Check if extension is installed or already loaded (bundled extensions
    ##      may show installed = FALSE but are still loadable/loaded)
    target_ext <- ext[ext$extension_name == extension, ]
    if (!isTRUE(target_ext$installed) && !isTRUE(target_ext$loaded))
        cli::cli_abort("{extension} extension is not installed, please use `ddbs_install(extension = '{extension}')`")

    
    # 2. Setup extension

    ## 2.1. Load the extension
    if (isFALSE(target_ext$loaded)) {
        tryCatch({
            suppressMessages(DBI::dbExecute(conn, glue::glue("LOAD {extension};")))
        }, error = function(e) {
            cli::cli_abort(c(
                "Failed to load the {extension} extension.",
                "x" = conditionMessage(e),
                "i" = "The binary may be corrupt or incompatible with this platform/DuckDB version.",
                "i" = "Try reinstalling with {.code ddbs_install(extension = '{extension}', upgrade = TRUE)}"
            ))
        })
    }

    ## 2.2. Activate macros
    if (isTRUE(create_macros)) create_duckspatial_macros(conn)
  
    ## 1.4. For DuckDB v2.0.0
    DBI::dbExecute(conn, "SET geometry_always_xy = true")

    ## 2.3. Message
    if (isFALSE(quiet)) {
        cli::cli_alert_success("{extension} extension loaded")
    }

}



#' Glimpse the status of a DuckDB extension
#'
#' Retrieves the row from DuckDB's \code{duckdb_extensions()} catalog for a given
#' extension (the spatial extension by default) and prints a transposed
#' \code{\link[dplyr]{glimpse}} of it: whether it is installed and loaded, its
#' version, install path, description, and so on.
#'
#' @template conn_null
#' @param extension name of the extension to inspect, default is "spatial"
#'
#' @returns A one-row \code{tibble} with the extension's metadata (invisibly).
#' Called mainly for the glimpse printed as a side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # inspect the spatial extension on the default connection
#' ddbs_extension_info()
#'
#' # or pass an explicit connection
#' conn <- ddbs_create_conn()
#' ddbs_extension_info(conn)
#' ddbs_stop_conn(conn)
#' }
ddbs_extension_info <- function(conn = NULL, extension = "spatial") {

    # 1. Resolve and validate inputs
    conn <- conn %||% ddbs_default_conn()
    dbConnCheck(conn)
    assert_character_scalar(extension, "extension")

    # 2. Query the extension catalog
    info <- DBI::dbGetQuery(
        conn,
        "SELECT * FROM duckdb_extensions() WHERE extension_name = ?",
        params = list(extension)
    )

    if (nrow(info) == 0) {
        cli::cli_abort(
            "Extension {.val {extension}} was not found in {.fn duckdb_extensions}."
        )
    }

    # 3. Glimpse and return the row invisibly
    dplyr::glimpse(tibble::as_tibble(info))

}





#' Get or create default DuckDB connection with spatial extension installed and loaded.
#'
#' @param create Logical. If TRUE and no connection exists, create one.
#'   Default is TRUE.
#' @param ... Additional parameters to pass to `ddbs_create_conn()`
#'
#' @returns A `duckdb_connection` or NULL if no connection exists and
#'   create = FALSE
#' 
#' @details
#' The connection is created internally when the first ckspatial  function is run. Every
#' function of the package runs on this connection when the argument `conn` is `NULL`.
#' 
#'
#' @export
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # get/create the default connection
#' conn <- ddbs_default_conn()
#'
#' }
ddbs_default_conn <- function(create = TRUE, ...) {
  conn <- getOption("duckspatial_conn", NULL)

  # Check if existing connection is still valid

  if (!is.null(conn)) {
    if (!DBI::dbIsValid(conn)) {
      options(duckspatial_conn = NULL)
      conn <- NULL
    }
  }

  # Create new connection if needed
  if (is.null(conn) && create) {
    conn <- ddbs_create_conn(dbdir = "memory", ...)
    options(duckspatial_conn = conn)
  }

  conn
}

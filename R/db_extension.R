
#' Checks and installs the Spatial extension
#'
#' Checks if a spatial extension is available, and installs it in a DuckDB database
#'
#' @template conn
#' @param upgrade if TRUE, it upgrades the DuckDB extension to the latest version
#' @template quiet
#' @param extension name of the extension to install, default is "spatial"
#'
#' @returns TRUE (invisibly) for successful installation
#' @export
#'
#' @examples
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
#' \dontrun{
#' # install the h3 community extension (requires network access)
#' conn <- duckdb::dbConnect(duckdb::duckdb())
#' ddbs_install(conn, extension = "h3")
#' duckdb::dbDisconnect(conn)
#' }
ddbs_install <- function(
    conn, 
    upgrade = FALSE, 
    quiet = FALSE, 
    extension = "spatial"
) {

    # 1. Get extensions list
    ext <- DBI::dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    # 2. Checks
    ## 2.1. Check connection
    dbConnCheck(conn)
    ## 2.2. Check if it's installed / needs upgrade
    target_ext <- ext[ext$extension_name == extension, ]
    if (nrow(target_ext) == 1 && target_ext$installed) {
        if (!upgrade) {
            if (isFALSE(quiet)) {
                cli::cli_alert_info(
                    "{extension} extension version {.val {target_ext$extension_version}} is already installed. Use {.code upgrade = TRUE} to upgrade."
                )
            }
            return(invisible(TRUE))
        }

        # upgrade = TRUE: check if already on latest before forcing
        latest <- tryCatch({
            result <- DBI::dbGetQuery(conn, glue::glue(
                "SELECT * FROM duckdb_extensions() WHERE extension_name = '{extension}';"
            ))
            # DuckDB >=0.10 exposes `install_mode` and whether it needs update
            # If extension_version matches across local and remote, skip
            isTRUE(result$install_mode == "repository" && !result$requires_version_upgrade)
        }, error = function(e) FALSE)

        if (isTRUE(latest)) {
            if (isFALSE(quiet)) {
                cli::cli_alert_info(
                    "{extension} extension version {.val {target_ext$extension_version}} is already the latest version."
                )
            }
            return(invisible(TRUE))
        }
    }

    # Extension cannot be upgraded if it's already loaded. It will fail
    if (isTRUE(target_ext$loaded) && isTRUE(upgrade)) {
        cli::cli_abort("{extension} is already loaded in the connection. Upgrading the version is only allowed in non-loaded connections.")
    }

    # 3. Install/upgrade extension - try core, then community, then error
    install_sql <- if (upgrade) "FORCE INSTALL {extension};" else "INSTALL {extension};"
    community_sql <- if (upgrade) "FORCE INSTALL {extension} FROM community;" else "INSTALL {extension} FROM community;"

    installed <- tryCatch({
        suppressMessages(DBI::dbExecute(conn, glue::glue(install_sql)))
        "core"
    }, error = function(e) {
        tryCatch({
            suppressMessages(DBI::dbExecute(conn, glue::glue(community_sql)))
            "community"
        }, error = function(e2) {
            NULL
        })
    })

    if (is.null(installed)) {
        cli::cli_abort(c(
            "Failed to {if (upgrade) 'upgrade' else 'install'} the {extension} extension.",
            "i" = "It could not be found in the core or community repositories.",
            "i" = "It might not be available for this version of DuckDB",
            "i" = "Check that the extension name is correct: {.url https://duckdb.org/docs/extensions/overview}"
        ))
    }

    if (isFALSE(quiet)) {
        action <- if (upgrade) "upgraded" else "installed"
        repo_note <- if (installed == "community") " (from community repository)" else ""
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

    ## 1.1. Get extensions list
    ext <- DBI::dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    ## 1.2. Check connection
    dbConnCheck(conn)

    ## 1.3. Check if extension is installed
    target_ext <- ext[ext$extension_name == extension, ]
    if (!target_ext$installed)
        cli::cli_abort("{extension} extension is not installed, please use `ddbs_install(extension = '{extension}')`")

    
    # 2. Setup extension

    ## 2.1. Load the extension
    if (isFALSE(target_ext$loaded)) suppressMessages(DBI::dbExecute(conn, glue::glue("LOAD {extension};")))

    ## 2.2. Activate macros
    if (isTRUE(create_macros)) create_duckspatial_macros(conn)

    ## 2.3. Message
    if (isFALSE(quiet)) {
        cli::cli_alert_success("{extension} extension loaded")
    }

}

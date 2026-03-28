
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
#' # install the h3 community extension
#' ddbs_install(conn, extension = "h3")
#'
#' # disconnect from db
#' duckdb::dbDisconnect(conn)
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
ddbs_load <- function(conn, quiet = FALSE, extension = "spatial") {

    # 1. Get extensions list
    ext <- DBI::dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    # 2. Checks
    ## 2.1. Check connection
    dbConnCheck(conn)
    ## 2.2. Check if extension is installed
    target_ext <- ext[ext$extension_name == extension, ]
    if (!target_ext$installed)
        cli::cli_abort("{extension} extension is not installed, please use `ddbs_install(extension = '{extension}')`")

    # 3. Load extension
    if (isFALSE(target_ext$loaded)) suppressMessages(DBI::dbExecute(conn, glue::glue("LOAD {extension};")))


    if (isFALSE(quiet)) {
        cli::cli_alert_success("{extension} extension loaded")
    }

}

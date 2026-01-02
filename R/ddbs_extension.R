
#' Checks and installs the Spatial extension
#'
#' Checks if a spatial extension is available, and installs it in a DuckDB database
#'
#' @template conn
#' @param upgrade if TRUE, it upgrades the DuckDB extension to the latest version
#' @template quiet
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
ddbs_install <- function(conn, upgrade = FALSE, quiet = FALSE, extension = "spatial") {

    # 1. Get extensions list
    ext <- DBI::dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    # 2. Checks
    ## 2.1. Check connection
    dbConnCheck(conn)
    ## 2.2. Check if extension is available
    if (!(extension %in% ext$extension_name))
        cli::cli_abort("{extension} extension is not available")
    ## 2.3. Check if it's installed
    target_ext <- ext[ext$extension_name == extension, ]
    if (target_ext$installed && !upgrade) {

        if (isFALSE(quiet)) {
            cli::cli_alert_info("{extension} extension version <{target_ext$extension_version}> is already installed in this database")
        }

        return(invisible(TRUE))
    }

    # 3. Install extension
    suppressMessages(DBI::dbExecute(conn, glue::glue("INSTALL {extension};")))

    if (isFALSE(quiet)) {
        cli::cli_alert_success("{extension} extension installed")
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
#' @examplesIf interactive()
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

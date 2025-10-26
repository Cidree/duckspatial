
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
#' library(duckdb)
#' library(duckspatial)
#'
#' ## connect to in memory database
#' conn <- dbConnect(duckdb::duckdb())
#'
#' ## install the spatial exntesion
#' ddbs_install(conn)
#'
#' ## disconnect from db
#' dbDisconnect(conn)
ddbs_install <- function(conn, upgrade = FALSE, quiet = FALSE) {

    # 1. Get extensions list
    ext <- DBI::dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    # 2. Checks
    ## 2.1. Check connection
    dbConnCheck(conn)
    ## 2.2. Check if spatial extension is available
    if (!("spatial" %in% ext$extension_name))
        cli::cli_abort("spatial extension is not available")
    ## 2.3. Check if it's installed
    spatial_ext <- ext[ext$extension_name == "spatial", ]
    if (spatial_ext$installed & !upgrade) {

        if (isFALSE(quiet)) {
            cli::cli_alert_info("spatial extension version <{spatial_ext$extension_version}> is already installed in this database")
        }

        return(invisible(TRUE))
    }

    # 3. Install extension
    suppressMessages(DBI::dbExecute(conn, "INSTALL spatial;"))

    if (isFALSE(quiet)) {
        cli::cli_alert_success("Spatial extension installed")
    }

    return(invisible(TRUE))


}


#' Loads the Spatial extension
#'
#' Checks if a spatial extension is installed, and loads it in a DuckDB database
#'
#' @template conn
#' @template quiet
#'
#' @returns TRUE (invisibly) for successful installation
#' @export
#'
#' @examplesIf interactive()
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#'
#' ## connect to in memory database
#' conn <- dbConnect(duckdb::duckdb())
#'
#' ## install the spatial exntesion
#' ddbs_install(conn)
#' ddbs_load(conn)
#'
#' ## disconnect from db
#' dbDisconnect(conn)
ddbs_load <- function(conn, quiet = FALSE) {

    # 1. Get extensions list
    ext <- DBI::dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    # 2. Checks
    ## 2.1. Check connection
    dbConnCheck(conn)
    ## 2.2. Check if spatial extension is installed
    spatial_ext <- ext[ext$extension_name == "spatial", ]
    if (!spatial_ext$installed)
        cli::cli_abort("spatial extension is not installed, please use `ddbs_install()`")

    # 3. Load spatial extension
    suppressMessages(DBI::dbExecute(conn, "LOAD spatial;"))

    if (isFALSE(quiet)) {
        cli::cli_alert_success("Spatial extension loaded")
    }

}

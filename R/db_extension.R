
ddbs_install <- function(conn) {

    # 1. Get extensions list
    ext <- dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    # 2. Checks
    ## 2.1. Check connection
    dbConnCheck(conn)
    ## 2.2. Check if spatial extension is available
    if (!("spatial" %in% ext$extension_name))
        stop("spatial extension is not available")
    ## 2.3. Check if it's installed
    spatial_ext <- ext[ext$extension_name == "spatial", ]
    if (spatial_ext$installed)
        stop(glue::glue("spatial extension version {spatial_ext$extension_version} is already installed in this database"))

    # 3. Install extension
    dbExecute(conn, "INSTALL spatial;")


}

ddbs_load <- function(conn) {

    # 1. Get extensions list
    ext <- dbGetQuery(conn, "SELECT * FROM duckdb_extensions();")

    # 2. Checks
    ## 2.1. Check connection
    dbConnCheck(conn)
    ## 2.2. Check if spatial extension is installed
    spatial_ext <- ext[ext$extension_name == "spatial", ]
    if (!spatial_ext$installed)
        stop("spatial extension is not installed, please use `ddbs_install()`")

    # 3. Load spatial extension
    dbExecute(conn, "LOAD spatial;")

}

#' Check and create schema
#'
#' @template conn
#' @param name A character string with the name of the schema to be created
#' @template quiet
#'
#' @returns TRUE (invisibly) for successful schema creation
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
#' ## create a new schema
#' ddbs_create_schema(conn, "new_schema")
#'
#' ## check schemas
#' dbGetQuery(conn, "SELECT * FROM information_schema.schemata;")
#'
#' ## disconnect from db
#' dbDisconnect(conn)
#'
ddbs_create_schema <- function(conn, name, quiet = FALSE) {

    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    ## Check if schema already exists
    namechar  <- DBI::dbQuoteString(conn,name)
    tmp.query <- paste0("SELECT EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = ",
                        namechar, ");")
    schema    <- DBI::dbGetQuery(conn, tmp.query)[1, 1]
    ## If it exists return TRUE, otherwise, create the schema
    if (schema) {
        cli::cli_abort("Schema <{name}> already exists.")
    } else {
        DBI::dbExecute(
            conn,
            glue::glue("CREATE SCHEMA {name};")
        )

        if (isFALSE(quiet)) {
            cli::cli_alert_success("Schema {name} created")
        }
    }
    return(invisible(TRUE))

}




#' Check CRS of a table
#'
#' @template conn
#' @param name A character string of length one specifying the name of the table,
#'        or a character string of length two specifying the schema and table
#'        names.
#' @param crs_column a character string of length one specifying the column
#' storing the CRS (created automatically by \code{\link{ddbs_write_vector}})
#'
#' @returns CRS object
#' @export
#'
#' @examplesIf interactive()
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, countries_sf, "countries")
#'
#' ## check CRS
#' ddbs_crs(conn, "countries")
ddbs_crs <- function(conn, name, crs_column = "crs_duckspatial") {

    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    ## convenient names of table and/or schema.table
    if (length(name) == 2) {
        table_name <- name[2]
        schema_name <- name[1]
        query_name <- paste0(name, collapse = ".")
    } else {
        table_name   <- name
        schema_name <- "main"
        query_name <- name
    }
    ## Check if table name exists
    if (!table_name %in% DBI::dbListTables(conn))
        cli::cli_abort("The provided name is not present in the database.")
    ## check if geometry column is present
    crs_data  <- DBI::dbGetQuery(
        conn, glue::glue("SELECT {crs_column} FROM {query_name} LIMIT 1;")
    ) |> as.character()

    # 2. Return CRS
    return(sf::st_crs(crs_data))
}





#' Check tables and schemas inside a database
#'
#' @template conn
#'
#' @returns `data.frame`
#' @export
#'
#' @examplesIf interactive()
#' ## TODO
ddbs_list_tables <- function(conn) {
  DBI::dbGetQuery(conn, "
      SELECT table_schema, table_name, table_type
      FROM information_schema.tables
    ")
}





#' Check first rows of the data
#'
#' @template conn
#' @param name A character string of length one specifying the name of the table,
#'        or a character string of length two specifying the schema and table
#'        names.
#' @template crs
#' @template quiet
#'
#' @returns `sf` object
#' @export
#'
#' @examplesIf interactive()
#' ## TODO
ddbs_glimpse <- function(conn,
                         name,
                         crs = NULL,
                         crs_column = "crs_duckspatial",
                         quiet = FALSE) {

    ## 1. check conn
    dbConnCheck(conn)

    ## 2. get column names
    ## convenient names of table and/or schema.table
    name_list <- get_query_name(name)
    ## get column names
    geom_name    <- get_geom_name(conn, name_list$query_name)
    no_geom_cols <- get_geom_name(conn, name_list$query_name, rest = TRUE) |> paste(collapse = ", ")

    # 3. Get data
    ## get data as table
    data_tbl <- DBI::dbGetQuery(conn, glue::glue("
      SELECT
      {no_geom_cols},
      ST_AsText({geom_name}) AS {geom_name}
      FROM {name}
      LIMIT 10;
  "))
    ## Convert to sf
    if (is.null(crs)) {
        if (is.null(crs_column)) {
            data_sf <- data_tbl |>
                sf::st_as_sf(wkt = geom_name)
        } else {
            data_sf <- data_tbl |>
                sf::st_as_sf(wkt = geom_name, crs = data_tbl[1, crs_column])
            data_sf <- data_sf[, -which(names(data_sf) == crs_column)]
        }

    } else {
        data_sf <- data_tbl |>
            sf::st_as_sf(wkt = geom_name, crs = crs)
    }

    if (isFALSE(quiet)) {
        cli::cli_alert_success("Showing first 10 rows of the data")
    }

    return(data_sf)

}



#' Create a duckdb connection
#'
#' @param dbdir String. Either `"tempdir"` or `"memory"`. Defaults to `"memory"`.
#'
#' @returns A `duckdb_connection`
#' @export
#'
#' @examplesIf interactive()
#' # load packages
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' # create a duckdb database in disk  (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "tempdir")
#'
ddbs_create_conn <- function(dbdir = "memory"){

    # 0. Handle errors
    if (!dbdir %in% c("tempdir","memory")) {
            cli::cli_abort("dbdir should be one of <'tempdir'>, <'memory'>")
        }


    # this creates a local database which allows DuckDB to
    # perform **larger-than-memory** workloads
    if(dbdir == 'tempdir'){

        db_path <- tempfile(pattern = 'duckspatial', fileext = '.duckdb')
        conn <- duckdb::dbConnect(
             duckdb::duckdb(
                 dbdir = db_path
                 #, bigint = "integer64" ## in case the data includes big int
                 )
            )
        }

    if(dbdir == 'memory'){

        conn <- duckdb::dbConnect(
            duckdb::duckdb(
                dbdir = ":memory:"
                #, bigint = "integer64" ## in case the data includes big int
                )
            )
    }

    # Checks and installs the Spatial extension
    duckspatial::ddbs_install(conn, quiet = TRUE)
    duckspatial::ddbs_load(conn, quiet = TRUE)


        # # Set Number of cores for parallel operation
        # if (is.null(n_cores)) {
        #     n_cores <- parallel::detectCores()
        #     n_cores <- n_cores - 1
        #     if (n_cores<1) {n_cores <- 1}
        # }
        #
        # DBI::dbExecute(con, sprintf("SET threads = %s;", n_cores))

        # Set Memory limit
        # DBI::dbExecute(con, "SET memory_limit = '8GB'")

        # DBI::dbExecute(con, "INSTALL arrow FROM community; LOAD arrow;")
        # DBI::dbExecute(con, "LOAD arrow;")

        return(conn)
    }





#' Get list of GDAL drivers and file formats
#'
#' @template conn
#'
#' @returns `data.frame`
#' @export
#'
#' @examplesIf interactive()
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#'
#' ## database setup
#' conn <- dbConnect(duckdb())
#' ddbs_install(conn)
#' ddbs_load(conn)
#'
#' ## check drivers
#' ddbs_drivers(conn)
ddbs_drivers <- function(conn) {
  DBI::dbGetQuery(conn, "
      SELECT * FROM ST_Drivers()
    ")
}

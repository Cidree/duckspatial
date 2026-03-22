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
#' \dontrun{
#' library(duckspatial)
#' library(duckdb)
#'
#' ## connect to in memory database
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## create a new schema
#' ddbs_create_schema(conn, "new_schema")
#'
#' ## check schemas
#' dbGetQuery(conn, "SELECT * FROM information_schema.schemata;")
#'
#' ## disconnect from db
#' ddbs_stop_conn(conn)
#' }
ddbs_create_schema <- function(conn, name, quiet = FALSE) {

    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    assert_name(name)
    assert_logic(quiet, "quiet")
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





#' Check tables and schemas inside a database
#'
#' @template conn
#'
#' @returns `data.frame`
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#' 
#' ## create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read some data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## insert into the database
#' ddbs_write_table(conn, argentina_ddbs, "argentina")
#' ddbs_write_table(conn, countries_ddbs, "countries")
#' 
#' ## list tables in the database
#' ddbs_list_tables(conn)
#' }
ddbs_list_tables <- function(conn) {
  DBI::dbGetQuery(conn, "
      SELECT table_schema, table_name, table_type
      FROM information_schema.tables
    ")
}





#' Check first rows of the data 
#' 
#' Prints a transposed table of the first rows of a DuckDB table, similarly
#' as the S3 [dplyr::glimpse] method.
#'
#' @template conn
#' @template name
#' @template quiet
#'
#' @returns Invisibly `duckspatial_df` object
#' @export
#'
#' @examples
#' \dontrun{
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_sf <- ddbs_open_dataset(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_table(conn, argentina_sf, "argentina")
#'
#' ## glimpse the inserted table
#' ddbs_glimpse(conn, "argentina")
#' }
ddbs_glimpse <- function(
  conn,
  name,
  quiet = FALSE) {

  
  ## 1. Handle errors
  dbConnCheck(conn)
  assert_name(name)
  assert_logic(quiet, "quiet")


  # 2. Prepare parameters for the query

  ## 2.1. Convenient names of table and/or schema.table
  name_list <- get_query_name(name)

  ## 2.2. Get column names
  x_geom    <- get_geom_name(conn, name_list$query_name)
  no_geom_cols <- get_geom_name(conn, name_list$query_name, rest = TRUE, collapse = TRUE)

  ## 2.3. Get the CRS
  crs <- ddbs_crs(name, conn)

  # 4. Get data

  ## 4.1. Build the query and retrieve the results
  data_tbl <- DBI::dbGetQuery(conn, glue::glue("
    SELECT
    {no_geom_cols}
    ST_AsWKB({x_geom}) AS {x_geom}
    FROM {name}
    LIMIT 10;
  "))

  ## 4.2. Convert to sf
  data_sf <- convert_to_sf_wkb(
      data   = data_tbl,
      crs    = crs,
      x_geom = x_geom
  )
  
  ## 4.4. Convert sf to duckspatial_df
  result <- as_duckspatial_df(
    x        = data_sf,
    conn     = conn,
    crs      = crs,
    geom_col = x_geom
  )
  
  ## 4.5. Return glimpse.duckspatial.df() and the result
  glimpse(result)
  return(invisible(result))

}



#' Create a DuckDB connection with spatial extension
#'
#' It creates a DuckDB connection, and then it installs and loads the
#' spatial extension
#'
#' @param dbdir String. Either `"tempdir"`, `"memory"`, or file path with
#' `.duckdb` or `.db` extension. Defaults to `"memory"`.
#' @template threads
#' @template memory_limit_gb
#'
#' @returns A `duckdb_connection`
#' @export
#'
#' @examples
#' \dontrun{
#' # load packages
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' # create a duckdb database in disk  (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "tempdir")
#'
#' # create a connection with 1 thread and 2GB memory limit
#' conn <- ddbs_create_conn(threads = 1, memory_limit_gb = 2)
#' ddbs_stop_conn(conn)
#' }
ddbs_create_conn <- function(dbdir = "memory", threads = NULL, memory_limit_gb = NULL){

    # 0. Handle errors
    if (!dbdir %in% c("tempdir","memory")) {
      ## get file extension
      db_extension <- tools::file_ext(dbdir)
      if (!db_extension %in% c("", "duckdb", "db"))
        cli::cli_abort("dbdir should be <'tempdir'>, <'memory'>, or have <'.duckdb'> or <'.db'> extension.")
    }

    assert_threads(threads)
    assert_memory_limit_gb(memory_limit_gb)

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
    } else if (dbdir == 'memory') {
      conn <- duckdb::dbConnect(
        duckdb::duckdb(
          dbdir = ":memory:"
          #, bigint = "integer64" ## in case the data includes big int
        )
      )
    } else {
      conn <- duckdb::dbConnect(
        duckdb::duckdb(dbdir = dbdir)
      )
    }

    # Checks and installs the Spatial extension
    ddbs_install(conn, upgrade = FALSE, quiet = TRUE)
    ddbs_load(conn, quiet = TRUE)

    # Configure resources if requested
    ddbs_set_resources(conn, threads = threads, memory_limit_gb = memory_limit_gb)

    return(conn)
}





#' Get list of GDAL drivers and file formats
#'
#' @template conn_default
#'
#' @returns `data.frame`
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' ## database setup
#' conn <- ddbs_create_conn()
#'
#' ## check drivers
#' ddbs_drivers(conn)
#' }
ddbs_drivers <- function(conn = NULL) {
  if (is.null(conn)) {
    conn <- ddbs_default_conn()
    if (is.null(conn)) {
       conn <- ddbs_create_conn(dbdir = "memory")
       on.exit(ddbs_stop_conn(conn), add = TRUE)
    }
  }
  DBI::dbGetQuery(conn, "
      SELECT * FROM ST_Drivers()
    ")
}

#' Close a DuckDB connection
#'
#' @template conn
#'
#' @returns TRUE (invisibly) for successful disconnection
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#'
#' ## create an in-memory duckdb database
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## close the connection
#' ddbs_stop_conn(conn)
#' }
ddbs_stop_conn <- function(conn) {
    # Check if connection is correct
    dbConnCheck(conn)

    # Disconnect from database
    DBI::dbDisconnect(conn)

    return(invisible(TRUE))
}

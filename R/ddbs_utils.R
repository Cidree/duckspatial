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




#' Check CRS of spatial objects or database tables
#'
#' This is an S3 generic that extracts CRS information from various spatial objects.
#'
#' @param x An object containing spatial data. Can be:
#'   - \code{duckspatial_df}: Lazy spatial data frame (CRS from attributes)
#'   - \code{sf}: sf object (CRS from sf metadata)
#'   - \code{character}: Name of table in database (requires \code{conn})
#' @param ... Additional arguments passed to methods
#'
#' @returns CRS object from \code{sf} package
#' @export
#'
#' @examplesIf interactive()
#' ## load packages
#' library(duckdb)
#' library(duckspatial)
#' library(sf)
#'
#' # Method 1: duckspatial_df objects
#' nc <- ddbs_open_dataset(system.file("shape/nc.shp", package = "sf"))
#' ddbs_crs(nc)
#'
#' # Method 2: sf objects
#' nc_sf <- st_read(system.file("shape/nc.shp", package = "sf"))
#' ddbs_crs(nc_sf)
#'
#' # Method 3: table name in database
#' conn <- ddbs_create_conn(dbdir = "memory")
#' ddbs_write_vector(conn, nc_sf, "nc_table")
#' ddbs_crs(conn, "nc_table")
#' ddbs_stop_conn(conn)
ddbs_crs <- function(x, ...) {
  UseMethod("ddbs_crs")
}

#' @export
#' @rdname ddbs_crs
ddbs_crs.duckspatial_df <- function(x, ...) {
  crs <- attr(x, "crs")
  if (is.null(crs)) {
    return(sf::st_crs(NA))
  }
  crs
}

#' @export
#' @rdname ddbs_crs
ddbs_crs.sf <- function(x, ...) {
  sf::st_crs(x)
}

#' @export
#' @rdname ddbs_crs
ddbs_crs.tbl_duckdb_connection <- function(x, ...) {
  # Try to auto-detect CRS from view SQL (for duckdbfs::open_dataset and similar)
  conn <- dbplyr::remote_con(x)
  
  # Strategy 1: Try to get view SQL from duckdb_views()
  view_sql <- tryCatch({
    table_name <- dbplyr::remote_name(x)
    
    if (!is.null(table_name) && !inherits(table_name, "sql")) {
      result <- DBI::dbGetQuery(conn, glue::glue(
        "SELECT sql FROM duckdb_views() WHERE view_name = '{table_name}'"
      ))
      if (nrow(result) > 0) result$sql else NULL
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  # Strategy 2: If no view found, render the query SQL
  if (is.null(view_sql)) {
    view_sql <- tryCatch({
      as.character(dbplyr::sql_render(x, con = conn))
    }, error = function(e) NULL)
  }
  
  # Extract file path from ST_Read() calls
  if (!is.null(view_sql)) {
    # Look for ST_Read('path') or st_read("path") patterns
    path_match <- regexpr("(?:ST_Read|st_read)\\s*\\(\\s*['\"]([^'\"]+)['\"]", 
                          view_sql, 
                          perl = TRUE, 
                          ignore.case = TRUE)
    
    if (path_match[1] > 0) {
      # Extract the captured group (the path)
      start <- attr(path_match, "capture.start")[1,1]
      length <- attr(path_match, "capture.length")[1,1]
      file_path <- substr(view_sql, start, start + length - 1)
      
      # Use st_read_meta to get CRS
      crs <- tryCatch({
        get_file_crs(file_path, conn)
      }, error = function(e) NULL)
      
      if (!is.null(crs)) {
        return(crs)
      }
    }
  }
  
  # Fallback: return NA CRS
  cli::cli_warn(c(
    "Could not auto-detect CRS for {.cls tbl_duckdb_connection} object.",
    "i" = "The object may not be a view created from a spatial file.",
    "i" = "Use {.code as_duckspatial_df(x, crs = ...)} to set CRS explicitly."
  ))
  sf::st_crs(NA)
}

#' @export
#' @rdname ddbs_crs
#' @param conn A DuckDB connection (required for character method)
#' @param crs_column Column name storing CRS info (default: "crs_duckspatial")
ddbs_crs.character <- function(x, conn, crs_column = "crs_duckspatial", ...) {
    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    
    name <- x
    
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
    ## Check if table name exists in Tables OR Arrow Views
    table_exists <- table_name %in% DBI::dbListTables(conn)
    arrow_exists <- FALSE

    if (!table_exists) {
        arrow_list <- try(duckdb::duckdb_list_arrow(conn), silent = TRUE)
        if (!inherits(arrow_list, "try-error") && table_name %in% arrow_list) {
            arrow_exists <- TRUE
        }
    }

    if (!table_exists && !arrow_exists) {
        cli::cli_abort("The provided name is not present in the database.")
    }
    
    ## check if crs_column exists and get CRS
    ## check if crs_column exists and get CRS
    crs_data <- tryCatch({
      DBI::dbGetQuery(
        conn, glue::glue("SELECT {crs_column} FROM {query_name} LIMIT 1;")
      ) |> as.character()
    }, error = function(e) {
      NULL
    })
    
    if (is.null(crs_data)) {
      # Fallback: Try to auto-detect from view definition (like tbl_duckdb_connection method)
      view_sql <- tryCatch({
        result <- DBI::dbGetQuery(conn, glue::glue(
          "SELECT sql FROM duckdb_views() WHERE view_name = '{table_name}'"
        ))
        if (nrow(result) > 0) result$sql else NULL
      }, error = function(e) NULL)
      
      if (!is.null(view_sql)) {
         path_match <- regexpr("(?:ST_Read|st_read)\\s*\\(\\s*['\"]([^'\"]+)['\"]", 
                              view_sql, perl = TRUE, ignore.case = TRUE)
         if (path_match[1] > 0) {
            start <- attr(path_match, "capture.start")[1,1]
            length <- attr(path_match, "capture.length")[1,1]
            file_path <- substr(view_sql, start, start + length - 1)
            
            crs <- tryCatch({ get_file_crs(file_path, conn) }, error = function(e) NULL)
            if (!is.null(crs)) return(crs)
         }
      }
    
      cli::cli_warn("CRS column '{crs_column}' not found in table '{query_name}' and could not be auto-detected.")
      return(sf::st_crs(NA))
    }

    # 2. Return CRS
    return(sf::st_crs(crs_data))
}

#' @export
#' @rdname ddbs_crs
#' @param name Table name (for backward compatibility when first arg is connection)
ddbs_crs.duckdb_connection <- function(x, name, ...) {
  # Backward compatibility: ddbs_crs(conn, name)
  if (missing(name)) {
    cli::cli_abort("Must provide {.arg name} when calling {.fun ddbs_crs} with a connection.")
  }
  ddbs_crs.character(name, conn = x, ...)
}

#' @export
#' @rdname ddbs_crs
ddbs_crs.default <- function(x, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a duckspatial_df, sf object, tbl_duckdb_connection, or character table name.",
    "i" = "You provided an object of class: {.cls {class(x)}}"
  ))
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
#' 2+2
#'
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
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## read data
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_sf, "argentina")
#'
#' ddbs_glimpse(conn, "argentina")
#'
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
    x_geom    <- get_geom_name(conn, name_list$query_name)
    no_geom_cols <- get_geom_name(conn, name_list$query_name, rest = TRUE, collapse = TRUE)

    # 3. Get data
    ## get data as table
    data_tbl <- DBI::dbGetQuery(conn, glue::glue("
      SELECT
      {no_geom_cols},
      ST_AsWKB({x_geom}) AS {x_geom}
      FROM {name}
      LIMIT 10;
  "))
    ## Convert to sf
    data_sf <- convert_to_sf_wkb(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    if (isFALSE(quiet)) {
        cli::cli_alert_success("Showing first 10 rows of the data")
    }

    return(data_sf)

}



#' Create a DuckDB connection with spatial extension
#'
#' It creates a DuckDB connection, and then it installs and loads the
#' spatial extension
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
    duckspatial::ddbs_install(conn, upgrade = TRUE, quiet = TRUE)
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

#' Close a duckdb connection
#'
#' @template conn
#'
#' @returns TRUE (invisibly) for successful disconnection
#' @export
#'
#' @examplesIf interactive()
#' ## load packages
#' library(duckspatial)
#'
#' ## create an in-memory duckdb database
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## close the connection
#' ddbs_stop_conn(conn)
#'
ddbs_stop_conn <- function(conn) {
    # Check if connection is correct
    dbConnCheck(conn)

    # Disconnect from database
    DBI::dbDisconnect(conn)

    return(invisible(TRUE))
}

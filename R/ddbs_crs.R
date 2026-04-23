
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
#' @examples
#' \dontrun{
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
#' ddbs_write_table(conn, nc_sf, "nc_table")
#' ddbs_crs(conn, "nc_table")
#' ddbs_stop_conn(conn)
#' }
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
ddbs_crs.character <- function(x, conn, ...) {

    # 0. Check if x is in AUTH:CODE format (e.g., "EPSG:4326")
    if (length(x) == 1 && grepl("^[A-Z]+:[0-9]+$", x)) {
        return(sf::st_crs(x))
    }
  
    # 1. Checks
    ## Check if connection is correct
    dbConnCheck(conn)
    
    name <- x
    
    ## convenient names of table and/or schema.table
    x_list <- get_query_list(x, conn)

    ## Check if table name exists in Tables OR Arrow Views
    # Use SQL check to catch temporary views which might not show up in dbListTables
    check_query <- glue::glue("
      SELECT 1 FROM information_schema.tables 
      WHERE table_name = '{x_list$query_name}' 
      UNION 
      SELECT 1 FROM duckdb_views() 
      WHERE view_name = '{x_list$query_name}'
      LIMIT 1
    ")
    
    table_exists <- tryCatch({
      nrow(DBI::dbGetQuery(conn, check_query)) > 0
    }, error = function(e) FALSE)
    
    arrow_exists <- FALSE

    if (!table_exists) {
        arrow_list <- try(duckdb::duckdb_list_arrow(conn), silent = TRUE)
        if (!inherits(arrow_list, "try-error") && x_list$table_name %in% arrow_list) {
            arrow_exists <- TRUE
        }
    }

    if (!table_exists && !arrow_exists) {
        cli::cli_abort("The provided name is not present in the database.")
    }
    
    ## Get CRS from the table
    crs_data <- tryCatch({
      geom_name <- get_geom_name(conn, x_list$query_name)
      DBI::dbGetQuery(
        conn, glue::glue("SELECT ST_CRS({geom_name}) AS crs FROM {x_list$query_name} LIMIT 1;")
      ) |> as.character()
    }, error = function(e) {
      NULL
    })
  
    ## TODO - Review below for duckdb 1.5 - is this necessary with ST_CRS()?    
    if (is.null(crs_data)) {
      # Fallback: Try to auto-detect from view definition (like tbl_duckdb_connection method)
      view_sql <- tryCatch({
        result <- DBI::dbGetQuery(conn, glue::glue(
          "SELECT sql FROM duckdb_views() WHERE view_name = '{x_list$table_name}'"
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
    
      cli::cli_warn("CRS could not be auto-detected.")
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
ddbs_crs.numeric <- function(x, ...) {
  # Convert numeric EPSG code to CRS
  # Assumes EPSG authority by default
  if (length(x) != 1) {
      cli::cli_abort("Numeric CRS input must be a single value (EPSG code).")
  }
  
  if (x < 1 || x != as.integer(x)) {
      cli::cli_abort("CRS code must be a positive integer.")
  }

  # Extract the CRS
  crs_x <- sf::st_crs(as.integer(x))
  
  # If the CRS doesn't exist, the previous function returns NA
  if (is.na(crs_x)) {
    cli::cli_abort("CRS code wasn't found.")
  } else {
    return(crs_x)
  }

}


#' @export
#' @rdname ddbs_crs
ddbs_crs.crs <- function(x, ...) {
  return(x)
}


#' @export
#' @rdname ddbs_crs
ddbs_crs.data.frame <- function(x, ...) {
  return(sf::st_crs(NA))
}


#' @export
#' @rdname ddbs_crs
ddbs_crs.default <- function(x, ...) {
  cli::cli_abort(c(
    "{.arg x} must be a duckspatial_df, sf object, tbl_duckdb_connection, or character table name.",
    "i" = "You provided an object of class: {.cls {class(x)}}"
  ))
}






#' Transform the coordinate reference system of geometries
#'
#' Converts geometries to a different coordinate reference system (CRS), updating 
#' their coordinates accordingly.
#'
#' @template x
#' @param y Target CRS. Can be:
#'   \itemize{
#'     \item A character string with EPSG code (e.g., "EPSG:4326")
#'     \item A \code{crs} object created with [sf::st_crs]
#'     \item An \code{sf} object (uses its CRS)
#'     \item Name of a DuckDB table (uses its CRS)
#'   }
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read data
#' argentina_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/argentina.geojson", 
#'   package = "duckspatial")
#' )
#' 
#' ## store in duckdb
#' ddbs_write_vector(conn, argentina_ddbs, "argentina")
#' 
#' ## transform to different CRS using EPSG code
#' ddbs_transform("argentina", "EPSG:3857", conn)
#' 
#' ## transform to match CRS of another object
#' argentina_3857_ddbs <- ddbs_transform(argentina_ddbs, "EPSG:3857")
#' ddbs_write_vector(conn, argentina_3857_ddbs, "argentina_3857")
#' ddbs_transform("argentina", argentina_3857_ddbs, conn)
#' 
#' ## transform to match CRS of another DuckDB table
#' ddbs_transform("argentina", "argentina_3857", conn)
#' 
#' ## transform without using a connection
#' ddbs_transform(argentina_ddbs, "EPSG:3857")
#' }
ddbs_transform <- function(
    x,
    y,
    conn = NULL,
    conn_x = NULL,
    conn_y = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet     = FALSE) {

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_name(name)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x <- if (is.null(conn_x)) ddbs_crs(x, conn) else ddbs_crs(x, conn_x)
    crs_y <- if (is.null(conn_y)) ddbs_crs(y, conn) else ddbs_crs(y, conn_y)
    sf_col_x <- attr(x, "sf_column")
    sf_col_y <- attr(y, "sf_column")

    ## 1.2. Resolve conn_x/conn_y defaults from 'conn' for character inputs
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

    ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn_x)
    try(y <- normalize_spatial_input(y, conn_y), silent = TRUE)

    ## 1.4. Get mode - If it's NULL, it will use the duckspatial.mode option
    mode <- get_mode(mode, name)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y, conn, conn_x, conn_y, quiet = quiet)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    y            <- resolve_conn$y
    ## register cleanup of the connection
    if (any(is.null(conn_x), is.null(conn_y))) {
        on.exit(resolve_conn$cleanup(), add = TRUE)   
    }

    ## 2.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)
    y_list <- get_query_list(y, target_conn)
    on.exit(y_list$cleanup(), add = TRUE)

    ## if CRS wasn't guessed earlier
    if (is.null(crs_x)) crs_x <- ddbs_crs(x_list$query_name, target_conn)
    if (is.null(crs_y)) {
        ## try to get from `y`. if it fails, it's not sf, nor duckspatial_df
        ## therefore, it might be a CRS or character string with CRS
        try(crs_y <- ddbs_crs(y_list$query_name, target_conn), silent = TRUE)

        if (is.null(crs_y)) crs_y <- sf::st_crs(y)
    }

    ## warn if the crs is the same
    if (crs_x$input == crs_y$input) cli::cli_warn("The CRS of `x` and `y` is the same.")
    
    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)

    ## 3.2. Build the base query
    ## always_xy assumes [northing, easting]
    st_function <- glue::glue("ST_Transform({x_geom}, '{crs_x$input}', '{crs_y$input}', always_xy := true)")
    base.query <- glue::glue("
        SELECT *
        REPLACE ({build_geom_query(st_function, name, crs_y, mode)} AS {x_geom})
        FROM 
            {x_list$query_name};
    ")

    # 4. if name is not NULL (i.e. no SF returned)
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create query (no st_as_text)
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            {base.query};
        ")
        ## execute intersection query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    }

    # 5. Apply geospatial operation
    result <- ddbs_handle_query(
        query      = base.query,
        conn       = target_conn,
        mode       = mode,
        crs        = crs_y,
        x_geom     = x_geom
    )

    return(result)
}





#' Set the coordinate reference system of geometries
#'
#' Assigns or replaces the coordinate reference system (CRS) of geometries
#' without transforming their coordinates. This is useful when the CRS is
#' missing or incorrectly defined.
#'
#' @template x
#' @param y Target CRS. Can be:
#'   \itemize{
#'     \item A character string with EPSG code (e.g., "EPSG:4326")
#'     \item A \code{crs} object created with [sf::st_crs]
#'   }
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load package
#' library(duckspatial)
#' library(sf)
#'
#' ## read data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson",
#'   package = "duckspatial")
#' )
#' 
#' ## Remove CRS
#' rivers_no_crs_ddbs <- ddbs_set_crs(rivers_ddbs, st_crs(NA))
#'
#' ## Set the CRS back
#' ddbs_set_crs(rivers_no_crs_ddbs, "EPSG:3035")
#' }
ddbs_set_crs <- function(
    x,
    y,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet     = FALSE) {

    # 0. Validate inputs
    assert_xy(x, "x")
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")


    # 1. Prepare inputs

    ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names)
    x <- normalize_spatial_input(x, conn)

    ## 1.2. Pre-extract attributes
    crs_x    <- ddbs_crs(x, conn)
    sf_col_x <- attr(x, "sf_column")
    mode     <- get_mode(mode, name)

    ## 1.3. Resolve spatial connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 1.4. Get list with query names for the input data
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)
  
    ## 1.5. Format CRS as "AUTH:CODE"
    y <- ddbs_crs(y)$input


    # 2. Prepare the query

    ## 2.1. Get the geometry column name (try to extract from attributes, if not 
    ## available get it from the database)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)


    ## 2.4. Build the base query (depends on the output type - sf, duckspatial_df, table)
    st_function <- glue::glue("ST_SetCRS({x_geom}, '{y}')")
  
    if (is.null(name) && mode == "sf") {
        ## If not creating a table, fallback to BLOB
        st_function <- glue::glue("ST_AsWKB({st_function})")
    }
  
    base.query <- glue::glue("
        SELECT *
        REPLACE ({st_function} AS {x_geom})
        FROM {x_list$query_name};
    ")

    # 3. Table creation if name is provided, or 
    # create duckspatial_df or sf object if name is NULL
    if (!is.null(name)) {
        create_duckdb_table(
            conn      = target_conn,
            name      = name,
            query     = base.query,
            overwrite = overwrite,
            quiet     = quiet
        )
    } else {
        ddbs_handle_query(
            query  = base.query,
            conn   = target_conn,
            mode   = mode,
            crs    = y,
            x_geom = x_geom
        )
    }

}

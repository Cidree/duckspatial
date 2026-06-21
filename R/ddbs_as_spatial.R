
#' Create point geometries from coordinate vectors
#'
#' Constructs POINT geometries from numeric coordinate vectors, optionally
#' including Z (elevation) and M (measure) dimensions and extra attribute
#' columns.
#'
#' @param x Numeric vector of X (longitude) coordinates.
#' @param y Numeric vector of Y (latitude) coordinates.
#' @param z Optional numeric vector of Z (elevation) coordinates.
#' @param m Optional numeric vector of M (measure) coordinates. Requires
#'   \code{z}.
#' @param ... Named vectors of additional attribute columns to include in the
#'   output. Each must have the same length as \code{x}.
#' @param crs Character or numeric CRS specification (e.g. \code{"EPSG:4326"}
#'   or \code{4326}). Defaults to \code{NULL} (no CRS assigned).
#' @param geom_col Name of the geometry column in the output. Defaults to
#'   \code{"geometry"}.
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
#' library(duckspatial)
#'
#' ## 2D points
#' ddbs_point(
#'   x = c(-58.38, -64.18, -60.64),
#'   y = c(-34.60, -31.42, -32.95),
#'   crs = 4326
#' )
#'
#' ## 3D points with extra columns
#' ddbs_point(
#'   x   = c(0, 1, 2),
#'   y   = c(0, 1, 2),
#'   z   = c(10, 20, 30),
#'   id  = 1:3,
#'   crs = 4326
#' )
#' }
ddbs_point <- function(
    x,
    y,
    z = NULL,
    m = NULL,
    ...,
    crs = NULL,
    geom_col = "geometry",
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {

    # 0. Validate inputs
    assert_name(name)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")

    if (!is.numeric(x)) cli::cli_abort("{.arg x} must be a numeric vector.")
    if (!is.numeric(y)) cli::cli_abort("{.arg y} must be a numeric vector.")

    n <- length(x)
    if (length(y) != n) {
        cli::cli_abort("{.arg x} and {.arg y} must have the same length.")
    }

    if (!is.null(m) && is.null(z)) {
        cli::cli_abort("{.arg m} requires {.arg z} to also be provided.")
    }
    if (!is.null(z)) {
        if (!is.numeric(z)) cli::cli_abort("{.arg z} must be a numeric vector.")
        if (length(z) != n) {
            cli::cli_abort("{.arg z} must have the same length as {.arg x} ({n}).")
        }
    }
    if (!is.null(m)) {
        if (!is.numeric(m)) cli::cli_abort("{.arg m} must be a numeric vector.")
        if (length(m) != n) {
            cli::cli_abort("{.arg m} must have the same length as {.arg x} ({n}).")
        }
    }

    dots <- list(...)
    if (length(dots) > 0) {
        if (is.null(names(dots)) || any(names(dots) == "")) {
            cli::cli_abort("All arguments passed via {.arg ...} must be named.")
        }
        reserved <- c("x", "y", "z", "m", geom_col)
        conflict <- intersect(names(dots), reserved)
        if (length(conflict) > 0) {
            cli::cli_abort(
                "Column name{?s} {.val {conflict}} in {.arg ...} conflict with reserved coordinate or geometry column names."
            )
        }
        bad_len <- names(dots)[vapply(dots, length, integer(1)) != n]
        if (length(bad_len) > 0) {
            cli::cli_abort(
                "Extra column{?s} {.val {bad_len}} must have the same length as {.arg x} ({n})."
            )
        }
    }

    if (!is.null(name) && is.null(conn)) {
        cli::cli_abort(
            "If {.arg name} is provided, {.arg conn} must be a valid DuckDB connection."
        )
    }

    # 1. Resolve connection and mode
    target_conn <- conn %||% ddbs_default_conn()
    mode        <- get_mode(mode, name)

    # 2. Build data frame and write to a temp table
    df <- data.frame(x = x, y = y, check.names = FALSE, stringsAsFactors = FALSE)
    if (!is.null(z)) df[["z"]] <- z
    if (!is.null(m)) df[["m"]] <- m
    for (nm in names(dots)) df[[nm]] <- dots[[nm]]

    tmp_name <- ddbs_temp_table_name()
    DBI::dbWriteTable(target_conn, tmp_name, df, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(target_conn, tmp_name), add = TRUE)

    # 3. Build ST_MakePoint expression (cast to GEOMETRY so build_geom_query
    #    can further annotate with CRS without hitting POINT_2D cast limits)
    st_fun <- if (!is.null(m)) {
        '(ST_MakePoint("x", "y", "z", "m")::GEOMETRY)'
    } else if (!is.null(z)) {
        '(ST_MakePoint("x", "y", "z")::GEOMETRY)'
    } else {
        '(ST_MakePoint("x", "y")::GEOMETRY)'
    }

    # 4. Build SELECT clause for extra columns
    extra_select <- if (length(dots) > 0) {
        paste0(paste0('"', names(dots), '"', collapse = ", "), ", ")
    } else {
        ""
    }

    geom_expr  <- build_geom_query(st_fun, name, crs, mode)
    base_query <- glue::glue(
        'SELECT {extra_select}{geom_expr} AS "{geom_col}" FROM "{tmp_name}";'
    )

    # 5. Handle output
    if (!is.null(name)) {
        create_duckdb_table(
            conn      = target_conn,
            name      = name,
            query     = base_query,
            overwrite = overwrite,
            quiet     = quiet
        )
    } else {
        ddbs_handle_query(
            query  = base_query,
            conn   = target_conn,
            mode   = mode,
            crs    = crs,
            x_geom = geom_col
        )
    }
}


#' Generate point geometries from coordinates
#'
#' Converts a data frame with coordinate columns into spatial point geometries.
#'
#' @template x
#' @param coords Character vector of length 2 specifying the names of the
#'        longitude and latitude columns (or X and Y coordinates). Defaults to
#'        \code{c("lon", "lat")}.
#' @param crs Character or numeric. The Coordinate Reference System (CRS) of the
#'        input coordinates. Can be specified as an EPSG code (e.g., \code{"EPSG:4326"}
#'        or \code{4326}) or a WKT string. Defaults to \code{"EPSG:4326"} (WGS84
#'        longitude/latitude).
#' @param remove Logical. If \code{TRUE} (default), the coordinate columns
#'        specified in \code{coords} are removed from the output.
#' @param na.fail Logical. If \code{TRUE} (default), the function errors if
#'        any missing values (NAs) are found in the coordinate columns.
#' @template conn_null
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#' @param ... Additional arguments. Currently supports \code{geom_col} to
#'        specify the name of the geometry column in the output.
#'
#' @template returns_mode
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#'
#' ## create sample data with coordinates
#' cities_df <- data.frame(
#'   city = c("Buenos Aires", "Córdoba", "Rosario"),
#'   lon = c(-58.3816, -64.1811, -60.6393),
#'   lat = c(-34.6037, -31.4201, -32.9468),
#'   population = c(3075000, 1391000, 1193605)
#' )
#'
#' # option 1: convert data frame to sf object
#' cities_ddbs <- ddbs_as_points(cities_df)
#'
#' # specify custom coordinate column names and keep them in output
#' cities_df2 <- data.frame(
#'   city = c("Mendoza", "Tucumán"),
#'   longitude = c(-68.8272, -65.2226),
#'   latitude = c(-32.8895, -26.8241)
#' )
#'
#' ddbs_as_points(cities_df2, coords = c("longitude", "latitude"), remove = FALSE)
#'
#'
#' ## option 2: convert table in duckdb to spatial table
#'
#' # create a duckdb connection and write data
#' conn <- duckspatial::ddbs_create_conn()
#' DBI::dbWriteTable(conn, "cities_tbl", cities_df, overwrite = TRUE)
#'
#' # convert to spatial table in database
#' ddbs_as_points(
#'     x = "cities_tbl",
#'     conn = conn,
#'     name = "cities_spatial",
#'     overwrite = TRUE
#' )
#'
#' # read the spatial table
#' ddbs_read_table(conn, "cities_spatial")
#' }
ddbs_as_points <- function(
    x,
    coords = c("lon", "lat"),
    crs = "EPSG:4326",
    remove = TRUE,
    na.fail = TRUE,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE,
    ...) {
        
    dots <- list(...)
    
    # 0. Validate inputs
    assert_name(name)
    assert_conn_x_name(conn, x, name)
    assert_conn_character(conn, x)
    assert_name(mode, "mode")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
    assert_logic(remove, "remove")
    assert_logic(na.fail, "na.fail")

    if (length(coords) != 2) {
        cli::cli_abort("{.arg coords} must be a character vector of length 2.")
    }
  

    # Respect geom_col
    target_geom <- dots$geom_col
  
    ## 1.1. Normalize inputs (coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names)
    x <- normalize_spatial_input(x, conn, geom_col = target_geom)

    ## 1.2. Pre-extract attributes
    mode <- get_mode(mode, name)

    ## 1.3. Resolve spatial connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 1.4. Get list with query names for the input data
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 2. Prepare the query

    ## 2.1. Validate coordinates existence and NAs
    # Use double quotes for column names to handle spaces/special characters
    coords_quoted <- paste0('"', coords, '"')
    
    # Check if columns exist
    tryCatch({
      DBI::dbExecute(target_conn, glue::glue("SELECT {paste0(coords_quoted, collapse = ', ')} FROM {x_list$query_name} WHERE 1=0"))
    }, error = function(e) {
      cli::cli_abort("Coordinate columns {.val {coords}} not found in input.")
    })

    # na.fail check
    if (na.fail) {
      null_check_sql <- glue::glue(
        "SELECT COUNT(*) as null_count FROM {x_list$query_name} 
         WHERE {paste0(coords_quoted, ' IS NULL', collapse = ' OR ')}"
      )
      null_count <- DBI::dbGetQuery(target_conn, null_check_sql)$null_count
      if (null_count > 0) {
        cli::cli_abort("Missing values found in coordinate columns {.val {coords}}. Set {.code na.fail = FALSE} to ignore.")
      }
    }

    ## 2.2. Coords as character string for ST_Point
    coords_str <- paste0(coords_quoted, collapse = ", ")
  
    ## 2.3. Build the base query (depends on the output type - sf, duckspatial_df, table)
    st_function <- glue::glue("ST_Point({coords_str})")
    
    # Handle 'remove' using DuckDB EXCLUDE
    select_clause <- if (remove) {
      glue::glue("SELECT * EXCLUDE ({coords_str}),")
    } else {
      "SELECT *,"
    }

    # Respect geom_col for query building
    query_geom <- target_geom %||% "geom"

    base.query <- glue::glue("
      {select_clause}
      {build_geom_query(st_function, name, crs, mode)} as {query_geom}
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
            crs    = crs,
            x_geom = query_geom
        )
    }
}


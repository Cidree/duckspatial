
#' Union and combine geometries
#'
#' @description
#' Perform union and combine operations on spatial geometries in DuckDB.
#'
#' * `ddbs_union()` - Union all geometries into one, or perform pairwise union between two datasets
#' * `ddbs_union_agg()` - Union geometries grouped by one or more columns
#' * `ddbs_combine()` - Combine geometries into a MULTI-geometry without dissolving boundaries
#'
#' @template x
#' @param y Input spatial data. Can be:
#'   \itemize{
#'    \item \code{NULL} (default): performs only the union of `x`
#'     \item A \code{duckspatial_df} object (lazy spatial data frame via dbplyr)
#'     \item An \code{sf} object
#'     \item A \code{tbl_lazy} from dbplyr
#'     \item A character string naming a table/view in \code{conn}
#'   }
#' @param by_feature Logical. When `y` is provided:
#'   * `FALSE` (default) - Union all geometries from both `x` and `y` into a single geometry
#'   * `TRUE` - Perform row-by-row union between matching features from `x` and `y` (requires same number of rows)
#' @param by Character vector specifying one or more column names to
#' group by when computing unions. Geometries will be unioned within each group.
#' Default is \code{NULL}
#' @template conn_null
#' @template conn_x_conn_y
#' @template name
#' @template mode
#' @template overwrite
#' @template quiet
#'
#' @details
#' ## ddbs_union(x, y, by_feature)
#' Performs geometric union operations that dissolve internal boundaries:
#' * When `y = NULL`: Unions all geometries in `x` into a single geometry
#' * When `y != NULL` and `by_feature = FALSE`: Unions all geometries from both `x` and `y` into a single geometry
#' * When `y != NULL` and `by_feature = TRUE`: Performs row-wise union, pairing the first geometry from `x` with the first from `y`, second with second, etc.
#'
#' ## ddbs_union_agg(x, by)
#' Groups geometries by one or more columns, then unions geometries within each group.
#' Useful for dissolving boundaries between features that share common attributes.
#'
#' ## ddbs_combine(x)
#' Combines all geometries into a single MULTI-geometry (e.g., MULTIPOLYGON, MULTILINESTRING)
#' without dissolving shared boundaries. This is faster than union but preserves all
#' original geometry boundaries.
#'
#' @template returns_mode
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(dplyr)
#' library(duckspatial)
#' 
#' ## create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#' 
#' ## read data
#' countries_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/countries.geojson", 
#'   package = "duckspatial")
#' ) |> 
#'   filter(ISO3_CODE != "ATA")
#' 
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson", 
#'   package = "duckspatial")
#' ) |> 
#'  ddbs_transform("EPSG:4326")
#' 
#' ## combine countries into a single MULTI-geometry
#' ## (without solving boundaries)
#' combined_countries_ddbs <- ddbs_combine(countries_ddbs)
#' 
#' ## combine countries into a single MULTI-geometry
#' ## (solving boundaries)
#' union_countries_ddbs <- ddbs_union(countries_ddbs)
#' 
#' ## union of geometries of two objects, into 1 geometry
#' union_countries_rivers_ddbs <- ddbs_union(countries_ddbs, rivers_ddbs)
#' }
#'
#' @name ddbs_union_funs
#' @rdname ddbs_union_funs
NULL



#' @rdname ddbs_union_funs
#' @export
ddbs_union <- function(
  x,
  y           = NULL,
  by_feature  = FALSE,
  conn        = NULL,
  conn_x      = NULL,
  conn_y      = NULL,
  name        = NULL,
  mode        = NULL,
  overwrite   = FALSE,
  quiet       = FALSE) {

  ## 0. Validate inputs
  assert_xy(x, "x")
  assert_logic(by_feature, "by_feature")
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  if (isTRUE(by_feature) && is.null(y)) {
    cli::cli_warn("When {.arg y} is NULL, {.arg by_feature = TRUE} is ignored.")
  }

  ## Pre-extract `x` attributes (CRS and geometry column name)
  crs_x    <- if (is.null(conn_x)) ddbs_crs(x, conn) else ddbs_crs(x, conn_x)
  sf_col_x <- attr(x, "sf_column")


  # ------------------------------------------------------------------
  # 1. Pairwise union: ST_Union(x, y)
  # ------------------------------------------------------------------
  if (!is.null(y)) {

    ## Validate y
    assert_xy(y, "y")
    assert_conn_character(conn, y)

    ## Pre-extract `y` attributes
    crs_y    <- if (is.null(conn_y)) ddbs_crs(y, conn) else ddbs_crs(y, conn_y)
    sf_col_y <- attr(y, "sf_column")

    ## Resolve conn_x/conn_y defaults from conn for character inputs
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn

    ## Normalize inputs
    x <- normalize_spatial_input(x, conn_x)
    y <- normalize_spatial_input(y, conn_y)

    ## Resolve connections
    resolve_conn <- resolve_spatial_connections(x, y, conn, conn_x, conn_y, quiet = quiet)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    y            <- resolve_conn$y
    ## register cleanup of the connection
    if (any(is.null(conn_x), is.null(conn_y))) {
        on.exit(resolve_conn$cleanup(), add = TRUE)   
    }

    ## Get query names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)
    y_list <- get_query_list(y, target_conn)
    on.exit(y_list$cleanup(), add = TRUE)

    ## CRS check
    if (!is.null(crs_x) && !is.null(crs_y)) {
      if (!crs_equal(crs_x, crs_y)) {
        cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
      }
    } else {
      assert_crs(target_conn, x_list$query_name, y_list$query_name)
    }

    ## Geometry column names
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)

    ## Get mode - If it's NULL, it will use the duckspatial.mode option
    mode <- get_mode(mode, name)

    ## Named table: write and return
    if (!is.null(name)) {
      name_list <- get_query_name(name)
      overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

      tmp.query <- build_union_query(
        by_feature = by_feature,
        mode       = "duckspatial",
        name       = name,
        crs        = crs_x,
        name_query = name_list$query_name,
        x_geom     = x_geom,
        y_geom     = y_geom,
        x_query    = x_list$query_name,
        y_query    = y_list$query_name
      )

      DBI::dbExecute(target_conn, tmp.query)
      feedback_query(quiet)
      return(invisible(TRUE))
    }

    ## Create the base query
    base.query <- build_union_query(
      by_feature = by_feature,
      name       = name,
      crs        = crs_x,
      mode       = mode,
      name_query = NULL,
      x_geom     = x_geom,
      y_geom     = y_geom,
      x_query    = x_list$query_name,
      y_query    = y_list$query_name
    )

    result <- ddbs_handle_query(
        query  = base.query,
        conn   = target_conn,
        mode   = mode,
        crs    = crs_x,
        x_geom = x_geom
    )

    return(result)

  }


  # ------------------------------------------------------------------
  # 2. Aggregate union: ST_Union(x)
  # ------------------------------------------------------------------

  ## Normalize input
  x <- normalize_spatial_input(x, conn)

  ## Resolve connection
  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn, quiet = quiet)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  on.exit(resolve_conn$cleanup(), add = TRUE)

  ## Get query name
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)

  ## Geometry column name
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  ## Resolve mode
  mode <- get_mode(mode, name)

  ## Named table: write and return
  if (!is.null(name)) {
    name_list <- get_query_name(name)
    overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

    tmp.query <- build_union_query(
      by_feature = FALSE,
      name       = name,
      crs        = crs_x,
      mode       = "duckspatial",
      name_query = name_list$query_name,
      x_geom     = x_geom,
      x_query    = x_list$query_name
    )

    DBI::dbExecute(target_conn, tmp.query)
    feedback_query(quiet)
    return(invisible(TRUE))
  }

  ## Create the base query

  base.query <- build_union_query(
    by_feature = FALSE,
    name       = name,
    crs        = crs_x,
    mode       = mode,
    name_query = NULL,
    x_geom     = x_geom,
    x_query    = x_list$query_name
  )

  result <- ddbs_handle_query(
      query      = base.query,
      conn       = target_conn,
      mode       = mode,
      crs        = crs_x,
      x_geom     = x_geom
  )

  return(result)
  
}




#' @rdname ddbs_union_funs
#' @export
ddbs_combine <- function(
    x,
    conn = NULL,
    name = NULL,
    mode = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    

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


    # 2. Prepare the query

    ## 2.1. Get the geometry column name (try to extract from attributes, if not 
    ## available get it from the database)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)
  
    ## 2.2. Build the base query (depends on the output type - sf, duckspatial_df, table)
    st_function <- glue::glue("ST_Collect(LIST({x_geom}))")
    base.query <- glue::glue("
      SELECT
        {build_geom_query(st_function, name, crs_x, mode)} AS {x_geom}
      FROM
        {x_list$query_name};
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
        crs    = crs_x,
        x_geom = x_geom
      )
    }
  
}



#' @rdname ddbs_union_funs
#' @export
ddbs_union_agg <- function(
  x,
  by,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  

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


  # 2. Prepare the query

  ## 2.1. Get the geometry column name (try to extract from attributes, if not 
  ## available get it from the database)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  ## 2.2. Get names of the rest of the groupping columns
  by_cols <- paste0(by, collapse = ", ")

  ## 2.3. Build the base query (depends on the output type - sf, duckspatial_df, table)
  st_function <- glue::glue("ST_Union_Agg({x_geom})")
  base.query <- glue::glue("
    SELECT 
      {by_cols},
      {build_geom_query(st_function, name, crs_x, mode)} AS {x_geom}
    FROM 
      {x_list$query_name}
    GROUP BY 
      {by_cols};
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
      crs    = crs_x,
      x_geom = x_geom
    )
  }
}





#' Dumps geometries into their component parts
#'
#' Decomposes multi-part or complex geometries into individual simple geometry
#' components, returning one row per component geometry
#'
#' @template x
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
#'
#' ## read data
#' rivers_ddbs <- ddbs_open_dataset(
#'   system.file("spatial/rivers.geojson",
#'   package = "duckspatial")
#' )
#'
#' ## aggregate rivers by name
#' rivers_agg_ddbs <- ddbs_union_agg(rivers_ddbs, by = "RIVER_NAME")
#'
#' ## dump into individual geometries
#' ddbs_dump(rivers_agg_ddbs)
#' }
ddbs_dump <- function(
  x,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
) {

  # 0. Validate inputs
  assert_xy(x, "x")
  assert_conn_x_name(conn, x, name)
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")

  # Warn if geometry type is not multi
  geom_type <- ddbs_geometry_type(x, by_feature = FALSE, conn)
  if (all(geom_type %in% c("POINT", "LINESTRING", "POLYGON")) & !quiet) {
    cli::cli_warn(c("The geometry type of {.arg x} is {.val {geom_type}}",
      "*" = "{.fun ddbs_dump()} is typically used for multi-part geometries." ,
      "*" = "With simple geometries it doesn't have any effect.")
  )}


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


  # 2. Prepare the query

  ## 2.1. Get the geometry column name (try to extract from attributes, if not 
  ## available get it from the database)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)


  ## 2.4. Build the base query (depends on the output type - sf, duckspatial_df, table)
  st_function <- "dump.geom"
  base.query <- glue::glue("
    WITH dumped AS (
    SELECT * EXCLUDE {x_geom},
          UNNEST(ST_Dump({x_geom})) AS dump
    FROM {x_list$query_name}
    )
    SELECT * EXCLUDE dump,
          /* dump.path AS path, */
          {build_geom_query(st_function, name, crs_x, mode)} AS {x_geom}
    FROM dumped;
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
      crs    = crs_x,
      x_geom = x_geom
    )
  }
  
}

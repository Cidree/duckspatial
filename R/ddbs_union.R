
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
#' @template crs
#' @template output
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
#' @template returns_output
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
#' @name ddbs_union
#' @rdname ddbs_union
NULL



#' @rdname ddbs_union
#' @export
ddbs_union <- function(
  x,
  y = NULL,
  by_feature = FALSE,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  deprecate_crs(crs_column, crs)

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_logic(by_feature, "by_feature")
  assert_name(name)
  assert_name(output, "output")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  if (isTRUE(by_feature) & is.null(y)) cli::cli_warn("When {.arg y} is NULL, {.arg by_feature = TRUE} is ignored.")

  ## Pre-extract `x` attributes (CRS and geometry column name)
  crs_x <- detect_crs(x)
  sf_col_x <- attr(x, "sf_column")


  # 1. Handle ST_Union(x, y) - pairwise union of two geometries
  if (!is.null(y)) {
    
    ## Check y
    assert_xy(y, "y")
    assert_conn_character(conn, y)
  
    ## 1.1. Pre-extract `y` attributes
    crs_y    <- detect_crs(y)
    sf_col_y <- attr(y, "sf_column")
  
    ## 1.2. Resolve conn_x/conn_y defaults from 'conn' for character inputs
    if (is.null(conn_x) && !is.null(conn) && is.character(x)) conn_x <- conn
    if (is.null(conn_y) && !is.null(conn) && is.character(y)) conn_y <- conn
  
    ## 1.3. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn_x)
    y <- normalize_spatial_input(y, conn_y)
  
    ## 1.4. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y, conn, conn_x, conn_y)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    y            <- resolve_conn$y
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)
  
    ## 1.5. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)
    y_list <- get_query_list(y, target_conn)
    on.exit(y_list$cleanup(), add = TRUE)
  
    ## 1.6. CRS already extracted at start of function
    if (!is.null(crs_x) && !is.null(crs_y)) {
        if (!crs_equal(crs_x, crs_y)) {
            cli::cli_abort("The Coordinates Reference System of {.arg x} and {.arg y} is different.")
        }
    } else {
        assert_crs(target_conn, x_list$query_name, y_list$query_name)
    }
    
    ## 1.7. Get names of geometry columns (use saved sf_col_x from before 
    ## transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    y_geom <- sf_col_y %||% get_geom_name(target_conn, y_list$query_name)
    assert_geometry_column(x_geom, x_list)
    assert_geometry_column(y_geom, y_list)
    
    ## 1.8. Get names of the rest of the columns
    x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE, table_id = "v1")
    y_rest <- get_geom_name(target_conn, y_list$query_name, rest = TRUE, collapse = TRUE, table_id = "v2")
    
    ## 1.9. if name is not NULL
    if (!is.null(name)) {
      ## convenient names of table and/or schema.table
      name_list <- get_query_name(name)

      ## handle overwrite
      overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

      ## create query for pairwise union
      ## assuming row-wise union based on row number
      if (isTRUE(by_feature)) {
        tmp.query <- glue::glue("
          SELECT
            ROW_NUMBER() OVER () as row_id,
            v1.{crs_column},
            ST_Union(v1.{x_geom}, v2.{y_geom}) as {x_geom}
          FROM
            (SELECT ROW_NUMBER() OVER () as rn, * FROM {x_list$query_name}) v1
          JOIN
            (SELECT ROW_NUMBER() OVER () as rn, * FROM {y_list$query_name}) v2
          ON v1.rn = v2.rn;
        ")
      } else {
        # Aggregate union - dissolves all geometries into one
        tmp.query <- glue::glue("
          SELECT
            1 as row_id,
            v1.{crs_column},
            ST_Union_Agg(geom) as {x_geom}
          FROM (
            SELECT {crs_column}, {x_geom} as geom FROM {x_list$query_name}
            UNION ALL
            SELECT {crs_column}, {y_geom} as geom FROM {y_list$query_name}
          ) v1
          GROUP BY v1.{crs_column};
        ")
      }
      
      ## execute union query
      DBI::dbExecute(target_conn, glue::glue("CREATE TABLE {name_list$query_name} AS {tmp.query}"))
      feedback_query(quiet)
      return(invisible(TRUE))

    }
    
    ## 1.10. Create the base query with ST_AsWKB
    if (isTRUE(by_feature)) {
      tmp.query <- glue::glue("
        SELECT
          ROW_NUMBER() OVER () as row_id,
          v1.{crs_column},
          ST_AsWKB(ST_Union(v1.{x_geom}, v2.{y_geom})) as {x_geom}
        FROM
          (SELECT ROW_NUMBER() OVER () as rn, * FROM {x_list$query_name}) v1
        JOIN
          (SELECT ROW_NUMBER() OVER () as rn, * FROM {y_list$query_name}) v2
        ON v1.rn = v2.rn;
      ")
    } else {
      # Aggregate union - dissolves all geometries into one
      tmp.query <- glue::glue("
        SELECT
          1 as row_id,
          v1.{crs_column},
          ST_AsWKB(ST_Union_Agg(geom)) as {x_geom}
        FROM (
          SELECT {crs_column}, {x_geom} as geom FROM {x_list$query_name}
          UNION ALL
          SELECT {crs_column}, {y_geom} as geom FROM {y_list$query_name}
        ) v1
        GROUP BY v1.{crs_column};
      ")
    }

    ## send the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

    ## 1.11. convert to SF and return result
    data_sf <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
  }


  # 2. Handle ST_Union(x)
  
  ## 2.1. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names
  x <- normalize_spatial_input(x, conn)

  ## 2.2. Resolve connections and handle imports
  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  ## register cleanup of the connection
  on.exit(resolve_conn$cleanup(), add = TRUE)

  ## 2.3. Get query list of table names
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)

  ## 2.4. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  ## 2.5. Get names of the rest of the columns
  x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = FALSE)


  # 3. if name is not NULL (i.e. no SF returned)
  if (!is.null(name)) {
    
    ## convenient names of table and/or schema.table
    name_list <- get_query_name(name)

    ## handle overwrite
    overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

    ## create query
    ## Union all geometries into a single geometry
    tmp.query <- glue::glue("
      CREATE TABLE {name_list$query_name} AS
      SELECT 
        FIRST({crs_column}) as {crs_column}, 
        ST_Union_Agg({x_geom}) as {x_geom}
      FROM 
        {x_list$query_name};
    ")

    ## execute union query
    DBI::dbExecute(target_conn, tmp.query)
    feedback_query(quiet)
    return(invisible(TRUE))

  }


  # 4. Create the base query
  ## if by = NULL, aggregate all geometries together
  tmp.query <- glue::glue("
      SELECT
        FIRST({crs_column}) as {crs_column}, 
        ST_AsWKB(ST_Union_Agg({x_geom})) as {x_geom}
      FROM {x_list$query_name};
  ")

  ## send the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)


  # 5. Convert to SF and return result
  data_sf <- ddbs_handle_output(
      data       = data_tbl,
      conn       = target_conn,
      output     = output,
      crs        = if (!is.null(crs)) crs else crs_x,
      crs_column = crs_column,
      x_geom     = x_geom
  )

  feedback_query(quiet)
  return(data_sf)

}




#' @rdname ddbs_union
#' @export
ddbs_combine <- function(
    x,
    conn = NULL,
    name = NULL,
    crs = NULL,
    crs_column = "crs_duckspatial",
    output = NULL,
    overwrite = FALSE,
    quiet = FALSE) {
    
    deprecate_crs(crs_column, crs)

    ## 0. Handle errors
    assert_xy(x, "x")
    assert_conn_character(conn, x)
    assert_name(name)
    assert_name(output, "output")
    assert_logic(overwrite, "overwrite")
    assert_logic(quiet, "quiet")
  

    # 1. Manage connection to DB

    ## 1.1. Pre-extract attributes (CRS and geometry column name)
    ## this step should be before normalize_spatial_input()
    crs_x    <- detect_crs(x)
    sf_col_x <- attr(x, "sf_column")

    ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
    ## validate character table names
    x <- normalize_spatial_input(x, conn)


    # 2. Manage connection to DB

    ## 2.1. Resolve connections and handle imports
    resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
    target_conn  <- resolve_conn$conn
    x            <- resolve_conn$x
    ## register cleanup of the connection
    on.exit(resolve_conn$cleanup(), add = TRUE)

    ## 2.2. Get query list of table names
    x_list <- get_query_list(x, target_conn)
    on.exit(x_list$cleanup(), add = TRUE)


    # 3. Prepare parameters for the query

    ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
    x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
    assert_geometry_column(x_geom, x_list)


    # 4. if name is not NUL
    if (!is.null(name)) {

        ## convenient names of table and/or schema.table
        name_list <- get_query_name(name)

        ## handle overwrite
        overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

        ## create the query
        tmp.query <- glue::glue("
            CREATE TABLE {name_list$query_name} AS
            SELECT
                ST_Collect(LIST({x_geom})) as {x_geom},
                FIRST({crs_column}) as {crs_column}
            FROM
                {x_list$query_name};
        ")

        ## execute the query
        DBI::dbExecute(target_conn, tmp.query)
        feedback_query(quiet)
        return(invisible(TRUE))
    
    }

  
    # 5. Get data

    ## 5.1. Create the query
    tmp.query <- glue::glue("
        SELECT
            ST_AsWKB(ST_Collect(LIST({x_geom}))) as {x_geom},
            FIRST({crs_column}) as {crs_column}
        FROM
            {x_list$query_name};
    ")

    ## 5.2. Get the query
    data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)

  
    # 6. Convert to SF and return result
    data_sf <- ddbs_handle_output(
        data       = data_tbl,
        conn       = target_conn,
        output     = output,
        crs        = if (!is.null(crs)) crs else crs_x,
        crs_column = crs_column,
        x_geom     = x_geom
    )

    feedback_query(quiet)
    return(data_sf)
}



#' @rdname ddbs_union
#' @export
ddbs_union_agg <- function(
  x,
  by,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = NULL,
  overwrite = FALSE,
  quiet = FALSE) {
  
  deprecate_crs(crs_column, crs)

  ## 0. Handle errors
  assert_xy(x, "x")
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(output, "output")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")


  # 1. Manage connection to DB

  ## 1.1. Pre-extract attributes (CRS and geometry column name)
  ## this step should be before normalize_spatial_input()
  crs_x    <- detect_crs(x)
  sf_col_x <- attr(x, "sf_column")

  ## 1.2. Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, 
  ## validate character table names
  x <- normalize_spatial_input(x, conn)


  # 2. Manage connection to DB

  ## 2.1. Resolve connections and handle imports
  resolve_conn <- resolve_spatial_connections(x, y = NULL, conn = conn)
  target_conn  <- resolve_conn$conn
  x            <- resolve_conn$x
  ## register cleanup of the connection
  on.exit(resolve_conn$cleanup(), add = TRUE)

  ## 2.2. Get query list of table names
  x_list <- get_query_list(x, target_conn)
  on.exit(x_list$cleanup(), add = TRUE)


  # 3. Prepare parameters for the query

  ## 3.1. Get names of geometry columns (use saved sf_col_x from before transformation)
  x_geom <- sf_col_x %||% get_geom_name(target_conn, x_list$query_name)
  assert_geometry_column(x_geom, x_list)

  ## 3.2. Get names of the rest of the groupping columns
  by_cols <- paste0(by, collapse = ", ")


  # 3. if name is not NULL (i.e. no SF returned)
  if (!is.null(name)) {
    
    ## convenient names of table and/or schema.table
    name_list <- get_query_name(name)

    ## handle overwrite
    overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

    ## Union of geometries grouped by specified columns
    tmp.query <- glue::glue("
      CREATE TABLE {name_list$query_name} AS
      SELECT 
        {by_cols}, 
        FIRST({crs_column}) as {crs_column},
        ST_Union_Agg({x_geom}) as {x_geom}
      FROM {x_list$query_name}
      GROUP BY {by_cols};
    ")

    ## execute union query
    DBI::dbExecute(target_conn, tmp.query)
    feedback_query(quiet)
    return(invisible(TRUE))

  }


  # 4. Create the base query
  ## Union of geometries grouped by specified columns
  by_cols <- paste0(by, collapse = ", ")

  ## create the query based on if other_cols has at least one column
  tmp.query <- glue::glue("
    SELECT 
      {by_cols}, 
      FIRST({crs_column}) as {crs_column},
      ST_AsWKB(ST_Union_Agg({x_geom})) as {x_geom}
    FROM {x_list$query_name}
    GROUP BY {by_cols};
  ")          

  ## send the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)


  # 5. Convert to SF and return result
  data_sf <- ddbs_handle_output(
      data       = data_tbl,
      conn       = target_conn,
      output     = output,
      crs        = if (!is.null(crs)) crs else crs_x,
      crs_column = crs_column,
      x_geom     = x_geom
  )

  feedback_query(quiet)
  return(data_sf)
  
}
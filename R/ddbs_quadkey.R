

#' Convert point geometries to QuadKey tiles
#'
#' Transforms point geometries into QuadKey identifiers at a specified zoom level, 
#' a hierarchical spatial indexing system used by mapping services.
#' 
#' @template x
#' @param level An integer specifying the zoom level for QuadKey generation (1-23).
#' Higher values provide finer spatial resolution. Default is 10.
#' @param field Character string specifying the field name for aggregation.
#' @param fun aggregation function for when there are multiple quadkeys (e.g. "mean",
#' "min", "max", "sum").
#' @param background numeric. Default value in raster cells without values. Only used when 
#' \code{output = "raster"}
#' @template conn_null
#' @template name
#' @template crs
#' @param output Character string specifying output format. One of:
#'   \itemize{
#'     \item \code{"polygon"} - Returns QuadKey tile boundaries as `duckspatial_df` (default)
#'     \item \code{"raster"} - Returns QuadKey values as a `SpatRaster`
#'     \item \code{"tilexy"} - Returns tile XY coordinates as a `tibble`
#' }
#' @template overwrite
#' @template quiet
#'
#' @returns Depends on the output argument
#' \itemize{
#'     \item \code{polygon} (default): A lazy spatial data frame backed by dbplyr/DuckDB.
#'     \item \code{raster}: An eagerly collected \code{SpatRaster} object in R memory.
#'     \item \code{tilexy}: An eagerly collected \code{tibble} without geometry in R memory.
#'   }
#' When \code{name} is provided, the result is also written as a table or view in DuckDB and the function returns \code{TRUE} (invisibly).
#'
#' @details
#' QuadKeys divide the world into a hierarchical grid of tiles, where each tile
#' is subdivided into four smaller tiles at the next zoom level. This function
#' wraps DuckDB's ST_QuadKey spatial function to generate these tiles from input geometries.
#' 
#' Note that creating a table inside the connection will generate a non-spatial table, and 
#' therefore, it cannot be read with `ddbs_read_vector()`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#' library(sf)
#' library(terra)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## create random points in Argentina
#' argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#' rand_sf <- st_sample(argentina_sf, 100) |> st_as_sf()
#' rand_sf["var"] <- runif(100)
#'
#' ## store in duckdb
#' ddbs_write_vector(conn, rand_sf, "rand_sf")
#'
#' ## generate QuadKey polygons at zoom level 8
#' qkey_ddbs <- ddbs_quadkey(conn = conn, "rand_sf", level = 8, output = "polygon")
#'
#' ## generate QuadKey raster with custom field name
#' qkey_rast <- ddbs_quadkey(conn = conn, "rand_sf", level = 6, output = "raster", field = "var")
#'
#' ## generate Quadkey XY tiles
#' qkey_tiles_tbl <- ddbs_quadkey(conn = conn, "rand_sf", level = 10, output = "tilexy")
#' }
ddbs_quadkey <- function(
  x,
  level = 10,
  field  = NULL,
  fun    = "mean",
  background = NA,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = "polygon",
  overwrite = FALSE,
  quiet = FALSE
) {

  deprecate_crs(crs_column, crs)
  
  ## 0. Handle errors
  assert_xy(x, "x")
  assert_numeric(level, "level")
  assert_name(field, "field")
  assert_character_scalar(fun, "fun")
  assert_conn_character(conn, x)
  assert_name(name)
  assert_name(output, "output")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")

  ## valid outputs
  if (!output %in% c("polygon", "raster", "tilexy")) cli::cli_abort("{.arg output} must be one of: {.val {c('polygon', 'raster', 'tilexy')}}")

  ## suggested packages
  rlang::check_installed(
    "quadkeyr",
    "to convert quadkeys to sf or raster."
  )

  if (output == "raster") {
    rlang::check_installed(
      "terra",
      "to convert quadkeys to raster."
    )
  }  

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

  ## 3.2. Get names of the rest of the columns
  x_rest <- get_geom_name(target_conn, x_list$query_name, rest = TRUE, collapse = TRUE)
  

  ## 3.3. check CRS (we need EPSG:4326 for quadkeys)
  data_crs <- ddbs_crs(target_conn, x_list$query_name, crs_column = crs_column)
  if (data_crs$input != "EPSG:4326") {
    if (!quiet) cli::cli_alert_info("Transforming {.arg x} crs to {.val EPSG:4326}")
    ## query
    tmp.query <- glue::glue("
      CREATE OR REPLACE TABLE {x_list$query_name} AS
      SELECT {x_rest}
      ST_Transform({x_geom}, '{data_crs$input}', 'EPSG:4326') as {x_geom} 
      FROM {x_list$query_name};
    ")
    ## execute
    DBI::dbExecute(target_conn, tmp.query)
  }


  # 4. if name is not NULL (i.e. no SF returned)
  if (!is.null(name)) {

      ## convenient names of table and/or schema.table
      name_list <- get_query_name(name)

      ## handle overwrite
      overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

      ## create query with optional aggregation
      if (!is.null(field)) {
        tmp.query <- glue::glue("
          CREATE TABLE {name_list$query_name} AS
          SELECT 
            ST_QuadKey({x_geom}, {level}) as quadkey,
            {fun}({field}) as {field},
            {crs_column}
          FROM {x_list$query_name}
          GROUP BY quadkey, {crs_column};
        ")
      } else {
        tmp.query <- glue::glue("
          CREATE TABLE {name_list$query_name} AS
          SELECT {x_rest}
          ST_QuadKey({x_geom}, {level}) as quadkey 
          FROM {x_list$query_name};
        ")
      }
      ## execute intersection query
      DBI::dbExecute(target_conn, tmp.query)
      feedback_query(quiet)
      return(invisible(TRUE))
  }


  # 5. Get data frame with query-level aggregation
  ## 5.1. create query with aggregation if field is specified
  if (!is.null(field)) {
    tmp.query <- glue::glue("
      SELECT 
        ST_QuadKey({x_geom}, {level}) as quadkey,
        {fun}({field}) as {field},
        {crs_column}
      FROM {x_list$query_name}
      GROUP BY quadkey, {crs_column};
    ")
  } else {
    tmp.query <- glue::glue("
      SELECT {x_rest}
      ST_QuadKey({x_geom}, {level}) as quadkey 
      FROM {x_list$query_name};
    ")
  }
  
  ## 5.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(target_conn, tmp.query)
  data_tbl <- dplyr::select(data_tbl, -dplyr::all_of(crs_column))

  ## 6. convert to desired output format
  if (output == "polygon") {

    ## get 1 quadkey per row (already aggregated if field was specified)
    data_sf <- quadkeyr::quadkey_df_to_polygon(data_tbl)

    ## convert to duckspatial_df
    prep_data <- as_duckspatial_df(data_sf)

  } else if (tolower(output) == "tilexy") {

    ## convert to tibble
    prep_data <- dplyr::bind_cols(
      dplyr::bind_rows(lapply(data_tbl$quadkey, quadkeyr::quadkey_to_tileXY)),
      data_tbl |> dplyr::select(-dplyr::all_of("quadkey"))
    )

  } else if (output == "raster") {

    ## get raster grid as polygons
    grid_lst <- quadkeyr::get_regular_polygon_grid(data_tbl)

    ## get field variable as SpatVector points
    pts_sf        <- dplyr::bind_rows(lapply(data_tbl$quadkey, quadkeyr::quadkey_to_latlong))
    pts_sf[field] <- as.vector(data_tbl[field])
    
    ## convert to spatvector to work with terra
    pts_vect  <- terra::vect(pts_sf)
    grid_vect <- terra::vect(grid_lst$data)

    ## create raster template
    grid_rast <- terra::rast(
      grid_vect,
      nrows = grid_lst$num_rows, 
      ncols = grid_lst$num_cols,
      crs   = terra::crs(grid_vect)
    )

    ## convert to raster
    prep_data <- terra::rasterize(
      x          = pts_vect,
      y          = grid_rast,
      field      = field,
      fun        = fun,
      background = background
    )

  }
  
  feedback_query(quiet)
  return(prep_data)

}


#' Convert geometries to QuadKey tiles
#'
#' Converts POINT geometries to QuadKey tile representations at a specified zoom level.
#' QuadKeys are a hierarchical spatial indexing system used by mapping services like Bing Maps.
#'
#' @template x
#' @param level An integer specifying the zoom level for QuadKey generation (1-23).
#' Higher values provide finer spatial resolution. Default is 10.
#' @param output Character string specifying output format. One of:
#'   \itemize{
#'     \item \code{"polygon"} - Returns QuadKey tile boundaries as polygons (default)
#'     \item \code{"raster"} - Returns QuadKey values as a raster grid
#'     \item \code{"tilexy"} - Returns tile XY coordinates
#'   }
#' @param field Character string specifying the field name for raster output.
#' Only used when \code{output = "raster"}
#' @param fun summarizing function for when there are multiple geometries in one cell (e.g. "mean",
#' "min", "max", "sum"). Only used when \code{output = "raster"}
#' @param background numeric. Default value in raster cells without values. Only used when 
#' \code{output = "raster"}
#' @template conn_null
#' @template name
#' @template crs
#' @template overwrite
#' @template quiet
#'
#' @returns An sf object or TRUE (invisibly) for table creation
#'
#' @details
#' QuadKeys divide the world into a hierarchical grid of tiles, where each tile
#' is subdivided into four smaller tiles at the next zoom level. This function
#' wraps DuckDB's ST_QuadKey spatial function to generate these tiles from input geometries.
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
#' qkey_sf <- ddbs_quadkey(conn = conn, "rand_sf", level = 8, output = "polygon")
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
  output = "polygon",
  field  = NULL,
  fun    = "mean",
  background = NA,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  overwrite = FALSE,
  quiet = FALSE
) {
  
  ## 0. Handle errors
  assert_xy(x, "x")
  assert_name(name)
  assert_numeric(level, "level")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  assert_conn_character(conn, x)

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
  ## 1.1. check if connection is provided, otherwise create a temporary connection
  is_duckdb_conn <- dbConnCheck(conn)
  if (isFALSE(is_duckdb_conn)) {
    conn <- duckspatial::ddbs_create_conn()
    on.exit(duckdb::dbDisconnect(conn), add = TRUE)
  }
  ## 1.2. get query list of table names
  x_list <- get_query_list(x, conn)

  ## 2. get name of geometry column
  ## 2.1. get column names
  x_geom <- get_geom_name(conn, x_list$query_name)
  x_rest <- get_geom_name(conn, x_list$query_name, rest = TRUE, collapse = TRUE)
  assert_geometry_column(x_geom, x_list)
  ## 2.2. check CRS (we need EPSG:4326 for quadkeys)
  data_crs <- ddbs_crs(conn, x_list$query_name, crs_column)
  if (data_crs$input != "EPSG:4326") {
    ## query
    tmp.query <- glue::glue("
      CREATE OR REPLACE TABLE {x_list$query_name} AS
      SELECT {paste0(x_rest, collapse = ', ')}, 
      ST_Transform({x_geom}, '{data_crs$input}', 'EPSG:4326') as {x_geom} 
      FROM {x_list$query_name};
    ")
    ## execute
    DBI::dbExecute(conn, tmp.query)
  }

  ## 3. if name is not NULL (i.e. no SF returned)
  if (!is.null(name)) {

      ## convenient names of table and/or schema.table
      name_list <- get_query_name(name)

      ## handle overwrite
      overwrite_table(name_list$query_name, conn, quiet, overwrite)

      ## create query (no st_as_text)
      tmp.query <- glue::glue("
        CREATE TABLE {name_list$query_name} AS
        SELECT {x_rest}
        ST_QuadKey({x_geom}, {level}) as quadkey 
        FROM {x_list$query_name};
      ")
      ## execute intersection query
      DBI::dbExecute(conn, tmp.query)
      feedback_query(quiet)
      return(invisible(TRUE))
  }

  # 4. Get data frame
  ## 4.1. create query
  tmp.query <- glue::glue("
    SELECT {x_rest}
    ST_QuadKey({x_geom}, {level}) as quadkey 
    FROM {x_list$query_name};
  ")
  ## 4.2. retrieve results from the query
  data_tbl <- DBI::dbGetQuery(conn, tmp.query)
  data_tbl <- dplyr::select(data_tbl, -dplyr::all_of(crs_column))

  ## 5. convert to SF and return result
  if (output == "polygon") {

    prep_data <- quadkeyr::quadkey_df_to_polygon(data_tbl)

  } else if (tolower(output) == "tilexy") {

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


#' Areal-Weighted Interpolation using DuckDB
#'
#' @description
#' Transfers attribute data from a source spatial layer to a target spatial layer based
#' on the area of overlap between their geometries. This function executes all spatial
#' calculations within DuckDB, enabling efficient processing of large datasets without
#' loading all geometries into R memory.
#'
#' @details
#' Areal-weighted interpolation is used when the source and target geometries are incongruent (they do not align). It relies on the assumption of **uniform distribution**: values in the source polygons are assumed to be spread evenly across the polygon's area.
#'
#' **Coordinate Systems:**
#' Area calculations are highly sensitive to the Coordinate Reference System (CRS).
#' While the function can run on geographic coordinates (lon/lat), it is strongly recommended
#' to use a **projected CRS** (e.g., EPSG:3857, UTM, or Albers) to ensure accurate area measurements.
#' Use the \code{join_crs} argument to project data on-the-fly during the interpolation.
#'
#' **Extensive vs. Intensive Variables:**
#' \itemize{
#'   \item **Extensive** variables are counts or absolute amounts (e.g., total population,
#'   number of voters). When a source polygon is split, the value is divided proportionally
#'   to the area.
#'   \item **Intensive** variables are ratios, rates, or densities (e.g., population density,
#'   cancer rates). When a source polygon is split, the value remains constant for each piece.
#' }
#'
#' **Mass Preservation (The \code{weight} argument):**
#' For extensive variables, the choice of weight determines the denominator used in calculations:
#' \itemize{
#'   \item \code{"sum"} (default): The denominator is the sum of all overlapping areas
#'   for that source feature. This preserves the "mass" of the variable *relative to the target's coverage*.
#'   If the target polygons do not completely cover a source polygon, some data is technically "lost"
#'   because it falls outside the target area. This matches \code{areal::aw_interpolate(weight="sum")}.
#'   \item \code{"total"}: The denominator is the full geometric area of the source feature.
#'   This assumes the source value is distributed over the entire source polygon. If the target
#'   covers only 50% of the source, only 50% of the value is transferred. This is strictly
#'   mass-preserving relative to the source. This matches \code{sf::st_interpolate_aw(extensive=TRUE)}.
#' }
#' *Note:* Intensive variables are always calculated using the \code{"sum"} logic (averaging
#' based on intersection areas) regardless of this parameter.
#'
#' @param target An \code{sf} object or the name of a persistent table in the DuckDB connection
#'   representing the destination geometries.
#' @param source An \code{sf} object or the name of a persistent table in the DuckDB connection
#'   containing the data to be interpolated.
#' @param tid Character. The name of the column in \code{target} that uniquely identifies features.
#' @param sid Character. The name of the column in \code{source} that uniquely identifies features.
#' @param extensive Character vector. Names of columns in \code{source} to be treated as
#'   spatially extensive (e.g., population counts).
#' @param intensive Character vector. Names of columns in \code{source} to be treated as
#'   spatially intensive (e.g., population density).
#' @param weight Character. Determines the denominator calculation for extensive variables.
#'   Either \code{"sum"} (default) or \code{"total"}. See **Mass Preservation** in Details.
#' @template mode
#' @param keep_NA Logical. If \code{TRUE} (default), returns all features from the target,
#'   even those that do not overlap with the source (values will be NA). If \code{FALSE},
#'   performs an inner join, dropping non-overlapping target features.
#' @param na.rm Logical. If \code{TRUE}, source features with \code{NA} values in the
#'   interpolated variables are completely removed from the calculation (area calculations
#'   will behave as if that polygon did not exist). Defaults to \code{FALSE}.
#' @param join_crs Numeric or Character (optional). EPSG code or WKT for the CRS to use
#'   for area calculations. If provided, both \code{target} and \code{source} are transformed
#'   to this CRS within the database before interpolation.
#' @template conn_null
#' @template name
#' @template overwrite
#' @template quiet
#'
#' @template returns_mode
#'
#' @examples
#' \donttest{
#' library(sf)
#'
#' # 1. Prepare Data
#' # Load NC counties (Source) and project to Albers (EPSG:5070)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' nc <- st_transform(nc, 5070)
#' nc$sid <- seq_len(nrow(nc)) # Create Source ID
#'
#' # Create a target grid
#' g <- st_make_grid(nc, n = c(10, 5))
#' g_sf <- st_as_sf(g)
#' g_sf$tid <- seq_len(nrow(g_sf)) # Create Target ID
#'
#' # 2. Extensive Interpolation (Counts)
#' # Use weight = "total" for strict mass preservation (e.g., total births)
#' res_ext <- ddbs_interpolate_aw(
#'   target = g_sf, source = nc,
#'   tid = "tid", sid = "sid",
#'   extensive = "BIR74",
#'   weight = "total"
#' )
#'
#' # Check mass preservation
#' sum(res_ext$BIR74, na.rm = TRUE) / sum(nc$BIR74) # Should be ~1
#'
#' # 3. Intensive Interpolation (Density/Rates)
#' # Calculates area-weighted average (e.g., assumption of uniform density)
#' res_int <- ddbs_interpolate_aw(
#'   target = g_sf, source = nc,
#'   tid = "tid", sid = "sid",
#'   intensive = "BIR74"
#' )
#'
#' # 4. Quick Visualization
#' par(mfrow = c(1, 2))
#' plot(res_ext["BIR74"], main = "Extensive (Total Count)", border = NA)
#' plot(res_int["BIR74"], main = "Intensive (Weighted Avg)", border = NA)
#' }
#'
#' @seealso
#' \code{\link[areal:aw_interpolate]{areal::aw_interpolate()}} — reference implementation.
#'
#' @references
#' Prener, C. and Revord, C. (2019). \emph{areal: An R package for areal weighted interpolation}.
#' \emph{Journal of Open Source Software}, 4(37), 1221.
#' Available at: \doi{10.21105/joss.01221}
#'
#' @export
ddbs_interpolate_aw <- function(
    target,
    source,
    tid,
    sid,
    extensive = NULL,
    intensive = NULL,
    weight = "sum",
    mode = NULL,
    keep_NA = TRUE,
    na.rm = FALSE,
    join_crs = NULL,
    conn = NULL,
    name = NULL,
    overwrite = FALSE,
    quiet = FALSE
) {

  # 0. Handle inputs and errors
  assert_xy(target, "target")
  assert_xy(source, "source")
  assert_name(name)
  assert_name(mode, "mode")
  assert_logic(overwrite, "overwrite")
  assert_logic(quiet, "quiet")
  assert_logic(keep_NA, "keep_NA")
  assert_logic(na.rm, "na.rm")
  assert_conn_character(conn, target, source)

  # 0. Validation Logic
  if (missing(tid)) cli::cli_abort("{.arg tid} must be provided.")
  if (missing(sid)) cli::cli_abort("{.arg sid} must be provided.")
  if (is.null(extensive) && is.null(intensive)) {
    cli::cli_abort("At least one of {.arg extensive} or {.arg intensive} must be provided.")
  }
  if (!weight %in% c("sum", "total")) {
    cli::cli_abort("{.arg weight} must be either 'sum' or 'total'.")
  }

  # Strict validation: Intensive variables cannot use "total" weight
  # because summing densities across total source areas is mathematically invalid.
  if (!is.null(intensive) && weight == "total") {
    cli::cli_abort("Spatially intensive variables must use {.code weight = 'sum'}.")
  }

  # 2. Normalize inputs

  # Pre-extract CRS and sf_column (before normalize_spatial_input converts types)
  t_geom <- attr(target, "sf_column")
  s_geom <- attr(source, "sf_column")
  t_crs  <- ddbs_crs(target, conn)
  s_crs  <- ddbs_crs(source, conn)

  # Normalize inputs: coerce tbl_duckdb_connection to duckspatial_df, validate character table names
  target <- normalize_spatial_input(target, conn)
  source <- normalize_spatial_input(source, conn)

  ## Get mode - If it's NULL, it will use the duckspatial.mode option
  mode <- get_mode(mode, name)

    
  # 3. Manage connection to DB

  ## 3.1. Resolve connections and handle imports
  resolve_res <- resolve_spatial_connections(target, source, conn)

  target_conn <- resolve_res$conn
  target <- resolve_res$x
  source <- resolve_res$y
  
  # 3.2 Get query list
  t_list <- get_query_list(target, target_conn)
  on.exit(t_list$cleanup(), add = TRUE)
  s_list <- get_query_list(source, target_conn)
  on.exit(s_list$cleanup(), add = TRUE)

  # CRS already extracted at start of function
  if (!is.null(join_crs)) {

    # If we need to reproject (join_crs provided), both inputs MUST have a known CRS.
    if (is.na(t_crs)) cli::cli_abort("Target CRS unknown. Cannot transform to {.arg join_crs}.")
    if (is.na(s_crs)) cli::cli_abort("Source CRS unknown. Cannot transform to {.arg join_crs}.")


    join_crs_sql <- crs_to_sql(join_crs)

    # Convert those objects to SQL literals
    t_crs_sql <- crs_to_sql(t_crs)
    s_crs_sql <- crs_to_sql(s_crs)

    if (t_crs_sql == "NULL") cli::cli_abort("Target CRS value is NULL. Cannot transform to {.arg join_crs}.")
    if (s_crs_sql == "NULL") cli::cli_abort("Source CRS value is NULL. Cannot transform to {.arg join_crs}.")

    t_geom_expr <- glue::glue("ST_Transform({t_geom}, {t_crs_sql}, {join_crs_sql})")
    s_geom_expr <- glue::glue("ST_Transform({s_geom}, {s_crs_sql}, {join_crs_sql})")
  } else {
    # If NO join_crs provided, inputs MUST match.
    if (!is.null(t_crs) && !is.null(s_crs)) {
      if (!crs_equal(t_crs, s_crs)) {
        cli::cli_abort("The Coordinates Reference System of {.arg target} and {.arg source} is different.")
      }
  } else {
      assert_crs(target_conn, t_list$query_name, s_list$query_name)
  }
    # If neither has CRS (t_has_crs=FALSE, s_has_crs=FALSE),
    # we assume they are both planar/NA and proceed without error.
  }
  

  # 4. Prepare parameters for query

  ## 4.1. predicate already validated early (sel_pred above)
  ## 4.2. get names of geometry columns (use saved sf_col_x/y from before transformation)
  t_geom <- t_geom %||% get_geom_name(target_conn, t_list$query_name)
  s_geom <- s_geom %||% get_geom_name(target_conn, s_list$query_name)
  assert_geometry_column(t_geom, t_list)
  assert_geometry_column(s_geom, s_list)

  # 3.3. Get Attribute Columns (target columns to keep)
  t_rest <- get_geom_name(target_conn, t_list$query_name, rest = TRUE, collapse = FALSE)

  # 2.1 Column Conflict Prevention
  # Check if interpolated vars already exist in target (excluding tid)
  interp_vars <- c(extensive, intensive)
  conflicts <- intersect(interp_vars, t_rest)
  conflicts <- setdiff(conflicts, tid) # ignore tid overlap as we join on it

  if (length(conflicts) > 0) {
    cli::cli_warn(c(
      "Column name conflict detected.",
      "i" = "The variables {.val {conflicts}} exist in both the target table and the interpolation list.",
      "!" = "The output will contain the interpolated values, overwriting the original target columns."
    ))
    # We remove conflicts from t_rest so the SQL doesn't select the original target cols
    t_rest <- setdiff(t_rest, conflicts)
  }

  # 3. Validate IDs and Variables exist
  assert_col_exists(target_conn, t_list$query_name, tid, "target")
  assert_col_exists(target_conn, s_list$query_name, sid, "source")

  if (!is.null(extensive)) assert_col_exists(target_conn, s_list$query_name, extensive, "source")
  if (!is.null(intensive)) assert_col_exists(target_conn, s_list$query_name, intensive, "source")

  # 4. Prepare CRS Logic (Projection)
  # Initialize default geometry expressions (used if join_crs is NULL)
  # Default to raw geometry columns (overwritten below if transformation is requested)
  t_geom_expr <- t_geom
  s_geom_expr <- s_geom


  # 5. Build CTE Query
  s_alias <- "s_geom_proj"
  t_alias <- "t_geom_proj"

  # Apply na.rm: Filter source table if requested
  s_source_sql <- s_list$query_name
  if (isTRUE(na.rm)) {
    vars_to_check <- c(extensive, intensive)
    where_clause <- paste(paste0(vars_to_check, " IS NOT NULL"), collapse = " AND ")
    s_source_sql <- glue::glue("(SELECT * FROM {s_list$query_name} WHERE {where_clause})")
  }

  # 5.1 Intersection/Overlap CTE
  overlap_cte <- glue::glue("
    overlap_calc AS (
      SELECT
        s.{sid} AS sid,
        t.{tid} AS tid,
        COALESCE(ST_Area(ST_Intersection({s_alias}, {t_alias})), 0) AS overlap_area
      FROM
        (SELECT *, {s_geom_expr} AS {s_alias} FROM {s_source_sql}) s
      INNER JOIN
        (SELECT *, {t_geom_expr} AS {t_alias} FROM {t_list$query_name}) t
      ON ST_Intersects({s_alias}, {t_alias})
    )
  ")

  # 5.2 Denominators
  denom_ctes <- character()

  if (!is.null(extensive)) {
    if (weight == "sum") {
      # Matches areal::aw_interpolate(weight="sum")
      denom_ctes <- c(denom_ctes, glue::glue("
        denom_extensive AS (
          SELECT sid, SUM(overlap_area) as total_area_sid
          FROM overlap_calc
          GROUP BY sid
        )
      "))
    } else {
      # Matches sf::st_interpolate_aw(extensive=TRUE)
      denom_ctes <- c(denom_ctes, glue::glue("
        denom_extensive AS (
          SELECT {sid} as sid, ST_Area({s_geom_expr}) as total_area_sid
          FROM {s_source_sql}
        )
      "))
    }
  }

  if (!is.null(intensive)) {
    # Matches sf::st_interpolate_aw(extensive=FALSE) logic
    denom_ctes <- c(denom_ctes, glue::glue("
      denom_intensive AS (
        SELECT tid, SUM(overlap_area) as total_area_tid
        FROM overlap_calc
        GROUP BY tid
      )
    "))
  }

  # 5.3 Aggregation Logic
  select_exprs <- character()
  if (!is.null(extensive)) {
    for (v in extensive) {
      # NULLIF protects against division by zero (empty geometry or zero area)
      select_exprs <- c(select_exprs, glue::glue(
        "SUM( (src.{v} * o.overlap_area) / NULLIF(dens.total_area_sid, 0) ) AS {v}"
      ))
    }
  }
  if (!is.null(intensive)) {
    for (v in intensive) {
      select_exprs <- c(select_exprs, glue::glue(
        "SUM( (src.{v} * o.overlap_area) / NULLIF(deni.total_area_tid, 0) ) AS {v}"
      ))
    }
  }

  agg_fields <- paste(select_exprs, collapse = ", ")

  # Join back to filtered source (if na.rm=TRUE) or original
  # Since we might have filtered source in overlap_calc but need attributes here
  src_join_sql <- s_source_sql # Reuse the filtered subquery logic

  joins_sql <- glue::glue("
    FROM overlap_calc o
    JOIN {src_join_sql} src ON o.sid = src.{sid}
  ")

  if (!is.null(extensive)) {
    joins_sql <- paste(joins_sql, "LEFT JOIN denom_extensive dens ON o.sid = dens.sid")
  }
  if (!is.null(intensive)) {
    joins_sql <- paste(joins_sql, "LEFT JOIN denom_intensive deni ON o.tid = deni.tid")
  }

  agg_cte <- glue::glue("
    aggregated_values AS (
      SELECT
        o.tid,
        {agg_fields}
      {joins_sql}
      GROUP BY o.tid
    )
  ")

  # 6. Final Execution
  # Explicitly select target columns to keep attributes
  t_cols_select <- if(length(t_rest) > 0) paste0("tgt.", t_rest, collapse = ", ") else ""
  if (t_cols_select != "") t_cols_select <- paste0(t_cols_select, ", ")

  # Apply keep_NA logic
  # if keep_NA=TRUE, use LEFT JOIN (preserve all targets)
  # if keep_NA=FALSE, use INNER JOIN (keep only targets with interpolated data)
  final_join_type <- if (isTRUE(keep_NA)) "LEFT JOIN" else "INNER JOIN"
  full_ctes <- paste(c(overlap_cte, denom_ctes, agg_cte), collapse = ",\n")

  # 6.1 Handle Output Type: SF vs Tibble (no geometry)
  st_function <- glue::glue("tgt.{t_geom}")
  final_select <- glue::glue("
    SELECT
      {t_cols_select}
      {build_geom_query(st_function, name, t_crs)} as {t_geom},
      av.* EXCLUDE (tid)
    FROM {t_list$query_name} tgt
    {final_join_type} aggregated_values av ON tgt.{tid} = av.tid
  ")
  table_select <- glue::glue("
    SELECT
      {t_cols_select}
      {build_geom_query(st_function, name, t_crs)} as {t_geom},
      av.* EXCLUDE (tid)
    FROM {t_list$query_name} tgt
    {final_join_type} aggregated_values av ON tgt.{tid} = av.tid
  ")

  # 6.2 Execute

  if (!is.null(name)) {
    name_list <- get_query_name(name)
    overwrite_table(name_list$query_name, target_conn, quiet, overwrite)

    # CREATE TABLE must precede WITH for standard CTE usage in DuckDB statements like this
    full_sql <- glue::glue("
      CREATE TABLE {name_list$query_name} AS
      WITH {full_ctes}
      {table_select}
    ")

    DBI::dbExecute(target_conn, full_sql)
    feedback_query(quiet)
    return(invisible(TRUE))
  }

  full_sql <- glue::glue("
    WITH {full_ctes}
    {final_select}
  ")

  result <- ddbs_handle_query(
        query  = full_sql,
        conn   = target_conn,
        mode   = mode,
        crs    = t_crs,
        x_geom = t_geom
    )

  return(result)

}

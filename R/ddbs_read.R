
#' Load spatial vector data from DuckDB into R
#'
#' Retrieves the data from a DuckDB table, view, or Arrow view with a geometry
#' column, and converts it to an R \code{sf} object. This function works with
#' both persistent tables created by \code{ddbs_write_vector} and temporary
#' Arrow views created by \code{ddbs_register_vector}.
#'
#' @template conn
#' @template name
#' @template crs
#' @param clauses character, additional SQL code to modify the query from the
#' table (e.g. "WHERE ...", "ORDER BY...")
#' @template quiet
#'
#' @returns an sf object
#' @export
#'
#' @examples
#' \dontrun{
#' ## load packages
#' library(duckspatial)
#' library(sf)
#'
#' # create a duckdb database in memory (with spatial extension)
#' conn <- ddbs_create_conn(dbdir = "memory")
#'
#' ## create random points
#' random_points <- data.frame(
#'   id = 1:5,
#'   x = runif(5, min = -180, max = 180),
#'   y = runif(5, min = -90, max = 90)
#' )
#'
#' ## convert to sf
#' sf_points <- st_as_sf(random_points, coords = c("x", "y"), crs = 4326)
#'
#' ## Example 1: Write and read persistent table
#' ddbs_write_vector(conn, sf_points, "points")
#' ddbs_read_vector(conn, "points", crs = 4326)
#'
#' ## Example 2: Register and read Arrow view (faster, temporary)
#' ddbs_register_vector(conn, sf_points, "points_view")
#' ddbs_read_vector(conn, "points_view", crs = 4326)
#'
#' ## disconnect from db
#' ddbs_stop_conn(conn)
#' }
ddbs_read_vector <- function(
    conn,
    name,
    crs = NULL,
    crs_column = "crs_duckspatial",
    clauses = NULL,
    quiet = FALSE) {
    
    
    deprecate_crs(crs_column, crs)
    
    # 0. Handle errors
    dbConnCheck(conn)
    assert_name(name)
    assert_name(clauses, "clauses")
    assert_logic(quiet, "quiet")
    

    # 1. Checks
    ## convenient names of table and/or schema.table
    name_list <- get_query_name(name)

    ## Check if table/view name exists in regular tables or Arrow views
    table_exists <- name_list$table_name %in% DBI::dbListTables(conn)
    object_type <- NULL

    if (table_exists) {
        # Determine if it's a table or view
        tables_df <- ddbs_list_tables(conn)
        db_tables <- paste0(tables_df$table_schema, ".", tables_df$table_name) |>
            sub(pattern = "^main\\.", replacement = "")
        match_idx <- which(db_tables == name_list$query_name)[1]
        if (!is.na(match_idx)) {
            table_type <- tables_df$table_type[match_idx]
            object_type <- if (!is.na(table_type) && identical(table_type, "VIEW")) {
                "view"
            } else {
                "table"
            }
        } else {
            object_type <- "table"
        }
    } else {
        # Check if it exists as an Arrow view
        arrow_views <- try(
            duckdb::duckdb_list_arrow(conn),
            silent = TRUE
        )
        arrow_exists <- if (inherits(arrow_views, "try-error")) {
            FALSE
        } else {
            name_list$query_name %in% arrow_views
        }

        if (!arrow_exists) {
            cli::cli_abort("The provided name is not present in the database as a table, view, or Arrow view.")
        } else {
            object_type <- "Arrow view"
        }
    }

    ## get column names and prepare SQL
    if (object_type == "Arrow view") {
        # For Arrow views, PRAGMA table_info doesn't work, so we need to get columns differently
        all_cols <- DBI::dbListFields(conn, name_list$query_name)

        # Dynamically identify geometry column (heuristic: look for standard names)
        candidates <- c("geometry", "geom", "shape", "wkb_geometry")
        geom_name <- intersect(all_cols, candidates)[1]

        # Fallback if standard name not found: find column that's not crs_duckspatial
        # The geometry column is added before crs_duckspatial, so it should be the
        # last column before crs_duckspatial (or last column if excluding crs_duckspatial)
        if (is.na(geom_name)) {
            non_crs_cols <- setdiff(all_cols, crs_column)
            if (length(non_crs_cols) > 0) {
                # Take the LAST non-CRS column (geometry is added last during registration)
                geom_name <- non_crs_cols[length(non_crs_cols)]
            }
        }

        if (is.na(geom_name) || !geom_name %in% all_cols) {
            cli::cli_abort("Geometry column wasn't found in Arrow view <{name_list$query_name}>.")
        }

        no_geom_cols <- setdiff(all_cols, geom_name) 
        no_geom_cols <-  if (length(no_geom_cols) > 0) paste0('"', no_geom_cols, '",', collapse = ' ') else ""

        # For Arrow views: Try ST_AsWKB directly first (geoarrow may already be recognized as GEOMETRY)
        # If that fails, ST_GeomFromWKB will be needed, but geoarrow registration makes it GEOMETRY type
        select_geom_sql <- glue::glue("ST_AsWKB({geom_name}) AS {geom_name}")
    } else {
        # For regular tables and views, use get_geom_name
        geom_name    <- get_geom_name(conn, name_list$query_name)
        no_geom_cols <- get_geom_name(conn, name_list$query_name, rest = TRUE, collapse = TRUE)
        if (length(geom_name) == 0) cli::cli_abort("Geometry column wasn't found in table <{name_list$query_name}>.")

        # For regular tables: already GEOMETRY type
        select_geom_sql <- glue::glue("ST_AsWKB({geom_name}) AS {geom_name}")
    }

    # 2. Retrieve data
    ## Retrieve data as data frame
    tmp.query <- glue::glue(
            "SELECT
            {no_geom_cols}
            {select_geom_sql}
            FROM {name_list$query_name}"
    )
    tmp.query <- paste(tmp.query, clauses)
    data_tbl <- DBI::dbGetQuery(conn, tmp.query)

    ## 5. convert to SF
    data_sf <- convert_to_sf_wkb(
        data       = data_tbl,
        crs        = crs,
        crs_column = crs_column,
        x_geom     = geom_name
    )

    ## return result
    if (isFALSE(quiet)) {
        cli::cli_alert_success("{object_type} {name} successfully imported.")
    }
    return(data_sf)

}

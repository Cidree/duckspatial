#' Create a duckspatial lazy spatial data frame
#'
#' Extends duckplyr_df with spatial metadata (CRS, geometry column).
#' All dplyr verbs work lazily via duckplyr's ALTREP mechanism.
#'
#' @param x Input: duckplyr_df, tbl_lazy, or data.frame
#' @param crs CRS object or string
#' @param geom_col Name of geometry column (default: "geometry")
#' @param prudence duckplyr prudence setting ("lavish", "thrifty", "stingy")
#' @return A duckspatial_df object
#' @export
new_duckspatial_df <- function(x, crs = NULL, geom_col = "geometry", 
                                prudence = "lavish") {
  
  # Convert to duckplyr_df if not already
  if (!inherits(x, "duckplyr_df")) {
    if (inherits(x, "tbl_lazy")) {
      x <- duckplyr::as_duckdb_tibble(x, prudence = prudence)
    } else if (is.data.frame(x)) {
      x <- duckplyr::as_duckdb_tibble(x, prudence = prudence)
    } else {
      cli::cli_abort("Cannot create duckspatial_df from {.cls {class(x)}}")
    }
  }
  
  # Prepend our class so our methods dispatch first
  class(x) <- c("duckspatial_df", class(x))
  
  # Set spatial metadata as attributes

  attr(x, "sf_column") <- geom_col
  attr(x, "crs") <- if (inherits(crs, "crs")) crs else sf::st_crs(crs)
  
  x
}

#' Check if object is a duckspatial_df
#' @param x Object to test
#' @return Logical
#' @export
is_duckspatial_df <- function(x) {
  inherits(x, "duckspatial_df")
}

#' Convert objects to duckspatial_df
#'
#' @param x Object to convert (sf, tbl_lazy, data.frame, or table name)
#' @param conn DuckDB connection (required for character table names)
#' @param crs CRS object or string (auto-detected from sf objects)
#' @param geom_col Geometry column name (default: "geometry")
#' @param prudence duckplyr prudence setting
#' @param ... Additional arguments passed to methods
#' @return A duckspatial_df object
#' @export
as_duckspatial_df <- function(x, conn = NULL, crs = NULL, geom_col = "geometry", 
                               prudence = "lavish", ...) {
  UseMethod("as_duckspatial_df")
}

#' @export
as_duckspatial_df.duckspatial_df <- function(x, conn = NULL, crs = NULL, 
                                              geom_col = NULL, prudence = NULL, ...) {
  # Already a duckspatial_df, return as-is
  x
}

#' @export
as_duckspatial_df.duckplyr_df <- function(x, conn = NULL, crs = NULL, 
                                           geom_col = "geometry", prudence = "lavish", ...) {
  new_duckspatial_df(x, crs = crs, geom_col = geom_col, prudence = prudence)
}

#' @export
as_duckspatial_df.sf <- function(x, conn = NULL, crs = NULL, geom_col = NULL, 
                                  prudence = "lavish", ...) {
  # Get CRS and geom column from sf object
  if (is.null(crs)) crs <- sf::st_crs(x)
  if (is.null(geom_col)) geom_col <- attr(x, "sf_column")
  
  # Get or create connection
  if (is.null(conn)) {
    conn <- ddbs_default_conn()
  }
  
  # Register sf as temp view
  view_name <- ddbs_temp_view_name()
  ddbs_write_vector(
    conn = conn,
    data = x,
    name = view_name,
    quiet = TRUE,
    temp_view = TRUE
  )
  
  # Create lazy table and convert to duckplyr
  lazy_tbl <- dplyr::tbl(conn, view_name)
  duck_tbl <- duckplyr::as_duckdb_tibble(lazy_tbl, prudence = prudence)
  
  new_duckspatial_df(duck_tbl, crs = crs, geom_col = geom_col, prudence = prudence)
}

#' @export
as_duckspatial_df.tbl_lazy <- function(x, conn = NULL, crs = NULL, 
                                        geom_col = "geometry", prudence = "lavish", ...) {
  duck_tbl <- duckplyr::as_duckdb_tibble(x, prudence = prudence)
  new_duckspatial_df(duck_tbl, crs = crs, geom_col = geom_col, prudence = prudence)
}

#' @export
as_duckspatial_df.character <- function(x, conn = NULL, crs = NULL, 
                                         geom_col = "geometry", prudence = "lavish", ...) {
  if (is.null(conn)) {
    conn <- ddbs_default_conn(create = FALSE)
    if (is.null(conn)) {
      cli::cli_abort("{.arg conn} must be provided when using table names as input.")
    }
  }
  
  lazy_tbl <- dplyr::tbl(conn, x)
  duck_tbl <- duckplyr::as_duckdb_tibble(lazy_tbl, prudence = prudence)
  new_duckspatial_df(duck_tbl, crs = crs, geom_col = geom_col, prudence = prudence)
}

#' @export
as_duckspatial_df.data.frame <- function(x, conn = NULL, crs = NULL, 
                                          geom_col = "geometry", prudence = "lavish", ...) {
  duck_tbl <- duckplyr::as_duckdb_tibble(x, prudence = prudence)
  new_duckspatial_df(duck_tbl, crs = crs, geom_col = geom_col, prudence = prudence)
}

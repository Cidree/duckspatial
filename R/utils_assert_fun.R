
assert_logic <- function(arg, ref = "quiet") { # nocov start

    if (!is.logical(arg)) {
        cli::cli_abort(
            "{.arg {ref}} must be either TRUE or FALSE.",
            .frame = parent.frame()
            )
        }
} # nocov end


assert_xy <- function(xy, ref = "x") { # nocov start

    valid_types <- inherits(xy, "sf") || 
                   inherits(xy, "duckspatial_df") ||
                   inherits(xy, "tbl_sql") ||
                   inherits(xy, "tbl_lazy") ||
                   is.character(xy)
    
    if (!valid_types) {
        cli::cli_abort(
            "{.arg {ref}} must be an sf object, duckspatial_df, tbl_lazy, or a table name string.",
            .frame = parent.frame()
        )
    }
} # nocov end

assert_name <- function(name = parent.frame()$name) { # nocov start

    if (!any(is.character(name) | is.null(name))) {
        cli::cli_abort("'name' must be a string character.",
                       .frame = parent.frame()
                       )
        }

    if (length(name) > 1) {
        cli::cli_abort("'name' must be a string character of length one",
                       .frame = parent.frame()
                       )
        }

 } # nocov end

assert_numeric <- function(arg, ref) { # nocov start

    if (!is.numeric(arg) || length(arg) != 1) {
        cli::cli_abort(
            "{.arg {ref}} must be a single numeric value.",
            .frame = parent.frame()
        )
    }
} # nocov end

assert_numeric_interval <- function(arg, minn, maxx, ref) { # nocov start

    if (!is.numeric(arg) || min(arg) < minn || max(arg) > maxx) {
        cli::cli_abort(
            "{.arg {ref}} must be a single numeric value between {minn} and {maxx}.",
            .frame = parent.frame()
        )
    }
} # nocov end


# whether the function takes sf of string as xy input
assert_connflict <- function(conn, xy, ref = "x") { # nocov start

    is_duckdn_conn <- dbConnCheck(conn)


    if (inherits(xy, "sf") && is_duckdn_conn) {
        cli::cli_abort(
            "If you pass a {.arg {ref}}, it must be a string when {.arg conn} is provided.",
            .frame = parent.frame()
        )
    }
} # nocov end


assert_geometry_column <- function(geom, name_list) { # nocov start
    if (length(geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{name_list$query_name}>.")
} # nocov end



## assert crs_column (needed for ddbs_filter and ddbs_join)
## cols can be a character vector or a collapsed string
assert_crs_column <- function(crs_column, cols) { # nocov start
    if (!is.null(crs_column)) {
        # Handle both vector and string inputs
        found <- crs_column %in% cols | any(grepl(crs_column, cols, fixed = TRUE))
        if (!found)
            cli::cli_abort("CRS column <{crs_column}> do not found in the table. If the data do not have CRS column, set the argument `crs_column = NULL`")
    }
} # nocov end


## assert id argument in predicate functions
assert_predicate_id <- function(id, conn, lst) { # nocov start
    if (!is.null(id)) {
        x_rest <- get_geom_name(conn, lst, rest = TRUE, collapse = FALSE)
        if (!id %in% x_rest) cli::cli_abort("<id> must be NULL or a column name of <x>")
    }
} # nocov end


## assert if the CRS of `x` and `y` is the same
assert_crs <- function(conn, x, y) { # nocov start

  ## get CRS using character method
  crs_x <- duckspatial::ddbs_crs(x, conn = conn)
  crs_y <- duckspatial::ddbs_crs(y, conn = conn)

  ## abort if CRS is different (use crs_equal for proper comparison)
  if (!crs_equal(crs_x, crs_y)) {
    cli::cli_abort("The Coordinates Reference System of `x` and `y` is different.")
  }

} # nocov end




## assert that if `x` or `y` are character, connection cannot be NULL
assert_conn_character <- function(conn, ...) { # nocov start

  ## get inputs into a list
  inputs <- list(...)

  ## check if they are character
  inputs_check <- lapply(inputs, is.character) |> as.logical()

  if (any(inputs_check) & is.null(conn)) {
    cli::cli_abort("If a table name is provided in `x` or `y`, the connection cannot be NULL.")
  }

} # nocov end

#' Assert that columns exist in a table
#'
#' @param conn DuckDB connection
#' @param table Table name
#' @param cols Vector of column names to check
#' @param ref Reference name for error message
#' @noRd
#' @returns invisible(TRUE)
assert_col_exists <- function(conn, table, cols, ref) { # nocov start
    avail <- DBI::dbListFields(conn, table)
    missing <- setdiff(cols, avail)
    if (length(missing) > 0) {
        # Rephrased to put the vector first, helping cli resolve pluralization quantity
        cli::cli_abort(
            "{.val {missing}} column{?s} not found in {.arg {ref}} table."
        )
    }
    invisible(TRUE)
} # nocov end

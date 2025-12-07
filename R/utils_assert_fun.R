
assert_logic <- function(arg, ref = "quiet") {

    if (!is.logical(arg)) {
        cli::cli_abort(
            "{.arg {ref}} must be either TRUE or FALSE.",
            .frame = parent.frame()
            )
        }
}


assert_xy <- function(xy, ref = "x") {

    if (!(inherits(xy, "sf") || is.character(xy))) {
        cli::cli_abort(
            "{.arg {ref}} must be either an sf object or a string.",
            .frame = parent.frame()
        )
    }
}

assert_name <- function(name = parent.frame()$name) {

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

 }

assert_numeric <- function(arg, ref) {

    if (!is.numeric(arg) || length(arg) != 1) {
        cli::cli_abort(
            "{.arg {ref}} must be a single numeric value.",
            .frame = parent.frame()
        )
    }
}

assert_numeric_interval <- function(arg, minn, maxx, ref) {

    if (!is.numeric(arg) || min(arg) < minn || max(arg) > maxx) {
        cli::cli_abort(
            "{.arg {ref}} must be a single numeric value between {minn} and {maxx}.",
            .frame = parent.frame()
        )
    }
}


# whether the function takes sf of string as xy input
assert_connflict <- function(conn, xy, ref = "x") {

    is_duckdn_conn <- dbConnCheck(conn)


    if (inherits(xy, "sf") & is_duckdn_conn) {
        cli::cli_abort(
            "If you pass a {.arg {ref}}, it must be a string when {.arg conn} is provided.",
            .frame = parent.frame()
        )
    }
}


assert_geometry_column <- function(geom, name_list) {
    if (length(geom) == 0) cli::cli_abort("Geometry column wasn't found in table <{name_list$query_name}>.")
}



## assert crs_column (needed for ddbs_filter)
assert_crs_column <- function(crs_column, cols) {
    if (!is.null(crs_column))
        if (!crs_column %in% cols)
            cli::cli_abort("CRS column <{crs_column}> do not found in the table. If the data do not have CRS column, set the argument `crs_column = NULL`")

}


## assert id argument in predicate functions
assert_predicate_id <- function(id, conn, lst) {
    if (!is.null(id)) {
        x_rest <- get_geom_name(conn, lst, rest = TRUE)
        if (!id %in% x_rest) cli::cli_abort("<id> must be NULL or a column name of <x>")
    }
}


## assert if the CRS of `x` and `y` is the same
assert_crs <- function(conn, x, y) {

  ## get CRS
  crs_x <- duckspatial::ddbs_crs(conn, x)
  crs_y <- duckspatial::ddbs_crs(conn, y)

  ## abort if CRS is different
  if (crs_x != crs_y) cli::cli_abort("The Coordinates Reference System of `x` and `y` is different.")

}


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

    if (!any(is.character(name) | is.null(name)))
        cli::cli_abort("'name' must be a string character",
                       .frame = parent.frame()
                       )
 }


# whether the function takes sf of string
assert_connflict <- function(conn, xy, ref = "x") {

    is_duckdn_conn <- dbConnCheck(conn)


    if (inherits(xy, "sf") & is_duckdn_conn) {
        cli::cli_abort(
            "If you pass a {.arg {ref}}, it must be a string when {.arg conn} is provided.",
            .frame = parent.frame()
        )
    }
}


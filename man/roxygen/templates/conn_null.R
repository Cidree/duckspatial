#' @param conn A connection object to a DuckDB database. Required when an input
#'        is a table name. If `NULL` (the default), the connection of a
#'        `duckspatial_df` input is used when there is one; otherwise the default
#'        connection from [ddbs_default_conn()] is used.

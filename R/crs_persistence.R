duckspatial_storage_default <- function() {
  getOption("duckspatial.duckdb_storage_version", "v1.5.0")
}

duckspatial_storage_versions <- function() {
  c(duckspatial_storage_default(), "v1.4.0", "v1.3.0", "v1.2.0", "v1.0.0", "latest")
}

match_duckdb_storage_version <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    cli::cli_abort("{.arg duckdb_storage_version} must be a single string.")
  }

  choices <- duckspatial_storage_versions()

  # 1. Exact match in curated list (e.g., 'latest', 'v1.5.0', 'v1.0.0')
  if (x %in% choices) {
    return(x)
  }

  # 2. Pass-through for strings that look like DuckDB version tags (vX.Y.Z)
  # This automatically supports future DuckDB releases without package updates.
  # If the version is unknown to the DuckDB binary, it will throw a descriptive
  # error during connection.
  if (grepl("^v[0-9]+", x)) {
    return(x)
  }

  # 3. Fallback to standard match.arg for suggestions/error reporting
  match.arg(x, choices)
}

is_storage_v15_or_newer <- function(tag) {
  if (is.null(tag) || is.na(tag) || !nzchar(tag)) {
    return(FALSE)
  }

  # DuckDB tags can be "v1.5.0+", "v1.0.0 - v1.1.3", etc.
  # We extract the first semantic version string found.
  version_match <- regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", tag)
  if (version_match == -1) {
    return(FALSE)
  }

  v_str <- regmatches(tag, version_match)
  utils::compareVersion(v_str, "1.5.0") >= 0
}

ddbs_assert_duckdb_crs_support <- function() {
  if (utils::packageVersion("duckdb") < "1.5.0") {
    cli::cli_abort(c(
      "duckspatial requires duckdb >= 1.5.0 to open files written by recent duckspatial versions.",
      "i" = "Installed: {utils::packageVersion('duckdb')}. Upgrade: install.packages('duckdb')."
    ))
  }

  invisible(TRUE)
}

ddbs_storage_is_legacy <- function(conn) {
  identical(attr(conn, "duckspatial_storage_mode"), "legacy")
}

ddbs_input_crs_for_write <- function(data, conn = NULL) {
  tryCatch(
    {
      if (inherits(data, "sf")) {
        return(sf::st_crs(data))
      }
      if (
        inherits(data, c("duckspatial_df", "tbl_duckdb_connection", "tbl_lazy"))
      ) {
        return(ddbs_crs(data))
      }
      if (
        is.character(data) &&
          length(data) == 1 &&
          file.exists(data) &&
          !has_duckdb_file_extension(data) &&
          !is.null(conn)
      ) {
        return(get_file_crs(data, conn))
      }
      NULL
    },
    error = function(e) NULL
  )
}

ddbs_write_legacy_crs_comment_if_needed <- function(
  conn,
  table,
  geom_col = NULL,
  crs = NULL
) {
  if (!ddbs_storage_is_legacy(conn)) {
    return(invisible(FALSE))
  }

  if (is.null(crs) || is.na(sf::st_crs(crs))) {
    return(invisible(FALSE))
  }

  geom_col <- geom_col %||%
    tryCatch(get_geom_name(conn, table), error = function(e) NULL)
  if (is.null(geom_col) || is.na(geom_col)) {
    return(invisible(FALSE))
  }

  write_crs_comment(conn, table, geom_col, crs)
}

ddbs_quote_table <- function(conn, table) {
  if (inherits(table, "SQL")) {
    return(as.character(table))
  }

  if (length(table) == 2) {
    return(as.character(DBI::dbQuoteIdentifier(
      conn,
      DBI::Id(schema = table[[1]], table = table[[2]])
    )))
  }

  table <- as.character(table)
  if (length(table) != 1) {
    cli::cli_abort("{.arg table} must identify one table.")
  }

  parts <- strsplit(table, ".", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    return(as.character(DBI::dbQuoteIdentifier(
      conn,
      DBI::Id(schema = parts[[1]], table = parts[[2]])
    )))
  }

  as.character(DBI::dbQuoteIdentifier(conn, table))
}

crs_to_duckdb_literal <- function(x) {
  if (is.null(x)) {
    return(list(literal = NA_character_, kind = "none"))
  }

  crs <- sf::st_crs(x)
  if (is.na(crs)) {
    return(list(literal = NA_character_, kind = "none"))
  }

  parsed <- sf::st_crs(crs, parameters = TRUE)
  srid <- parsed$srid
  if (!is.null(srid) && length(srid) > 0 && !is.na(srid)) {
    return(list(literal = as.character(srid), kind = "authority"))
  }

  wkt <- crs$wkt
  if (is.null(wkt) || length(wkt) == 0 || is.na(wkt) || !nzchar(wkt)) {
    return(list(literal = NA_character_, kind = "none"))
  }

  list(literal = wkt, kind = "wkt")
}

ddbs_quote_sql_string <- function(conn, x) {
  if (!is.null(conn)) {
    return(as.character(DBI::dbQuoteString(conn, x)))
  }

  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

duckdb_geometry_type <- function(conn, x) {
  lit <- crs_to_duckdb_literal(x)
  if (identical(lit$kind, "none")) {
    return("GEOMETRY")
  }

  paste0("GEOMETRY(", ddbs_quote_sql_string(conn, lit$literal), ")")
}

read_native_crs <- function(conn, table, geom_col) {
  q_geom <- as.character(DBI::dbQuoteIdentifier(conn, geom_col))
  q_table <- ddbs_quote_table(conn, table)
  query <- paste0(
    "SELECT ST_CRS(",
    q_geom,
    ") AS crs ",
    "FROM ",
    q_table,
    " ",
    "WHERE ",
    q_geom,
    " IS NOT NULL ",
    "LIMIT 1"
  )

  res <- tryCatch(DBI::dbGetQuery(conn, query), error = function(e) NULL)
  if (
    is.null(res) ||
      nrow(res) == 0 ||
      is.na(res$crs[[1]]) ||
      identical(res$crs[[1]], "")
  ) {
    return(sf::st_crs(NA))
  }

  sf::st_crs(res$crs[[1]])
}

make_crs_comment <- function(crs, geom_col, existing_payload = NULL) {
  parsed <- sf::st_crs(crs)
  payload <- list(
    duckspatial = list(
      version = 1L,
      geometry_column = geom_col,
      crs = list(
        input = as.character(parsed$input),
        authority = if (!is.null(parsed$authority)) parsed$authority else NULL,
        srid = if (!is.na(parsed$epsg)) as.integer(parsed$epsg) else NULL,
        wkt2_2019 = parsed$wkt
      )
    )
  )

  if (is.list(existing_payload) && !is.null(existing_payload$user_comment)) {
    payload$duckspatial$user_comment <- existing_payload$user_comment
  }

  jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null")
}

parse_crs_comment <- function(comment_text) {
  if (is.null(comment_text) || is.na(comment_text) || !nzchar(comment_text)) {
    return(NULL)
  }

  obj <- tryCatch(
    jsonlite::fromJSON(comment_text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(obj) || is.null(obj$duckspatial)) {
    return(NULL)
  }

  obj$duckspatial
}

get_column_comment <- function(conn, table, geom_col) {
  name_list <- get_query_name(table)
  query <- paste0(
    "SELECT comment FROM duckdb_columns() ",
    "WHERE schema_name = ",
    DBI::dbQuoteString(conn, name_list$schema_name),
    " AND table_name = ",
    DBI::dbQuoteString(conn, name_list$table_name),
    " AND column_name = ",
    DBI::dbQuoteString(conn, geom_col),
    " LIMIT 1"
  )
  res <- DBI::dbGetQuery(conn, query)
  if (nrow(res) == 0) {
    return(NA_character_)
  }

  res$comment[[1]]
}

write_crs_comment <- function(conn, table, geom_col, crs) {
  parsed <- sf::st_crs(crs)
  if (is.na(parsed)) {
    return(invisible(FALSE))
  }

  cur <- get_column_comment(conn, table, geom_col)
  parsed_cur <- parse_crs_comment(cur)
  existing <- NULL
  if (is.null(parsed_cur) && !is.null(cur) && !is.na(cur) && nzchar(cur)) {
    existing <- list(user_comment = cur)
  }

  payload <- make_crs_comment(parsed, geom_col, existing_payload = existing)
  sql <- sprintf(
    "COMMENT ON COLUMN %s.%s IS %s",
    ddbs_quote_table(conn, table),
    DBI::dbQuoteIdentifier(conn, geom_col),
    DBI::dbQuoteString(conn, payload)
  )
  DBI::dbExecute(conn, sql)
  invisible(TRUE)
}

crs_from_comment_payload <- function(payload) {
  if (is.null(payload) || is.null(payload$crs)) {
    return(sf::st_crs(NA))
  }

  pref <- NULL
  if (!is.null(payload$crs$authority) && !is.null(payload$crs$srid)) {
    pref <- paste0(payload$crs$authority, ":", payload$crs$srid)
  }
  pref <- pref %||%
    payload$crs$srid %||%
    payload$crs$wkt2_2019 %||%
    payload$crs$input

  tryCatch(sf::st_crs(pref), error = function(e) sf::st_crs(NA))
}

resolve_crs <- function(
  conn,
  table,
  geom_col,
  explicit = NULL,
  warn_on_disagree = TRUE,
  quiet_unknown = FALSE
) {
  if (!is.null(explicit)) {
    return(sf::st_crs(explicit))
  }

  native <- read_native_crs(conn, table, geom_col)
  comment <- parse_crs_comment(get_column_comment(conn, table, geom_col))
  comment_crs <- crs_from_comment_payload(comment)

  has_native <- !is.na(native)
  has_comment <- !is.na(comment_crs)

  if (
    has_native &&
      has_comment &&
      warn_on_disagree &&
      !crs_equal(native, comment_crs)
  ) {
    cli::cli_warn(
      "CRS disagreement for {.val {table}}.{.val {geom_col}}: using native DuckDB CRS metadata."
    )
  }
  if (has_native) {
    return(native)
  }
  if (has_comment) {
    return(comment_crs)
  }

  if (!quiet_unknown) {
    cli::cli_warn(c(
      "CRS could not be detected for {.val {table}}.{.val {geom_col}}.",
      "i" = "This can happen when reading an older DuckDB storage file that was created before duckspatial persisted CRS metadata, or when the table has no CRS information.",
      "i" = "Use {.arg crs} when opening the dataset, or rewrite the file using {.code ddbs_create_conn(..., storage_version = \"v1.5.0\")} to persist CRS natively."
    ))
  }

  sf::st_crs(NA)
}

ddbs_duckdb_storage_tag <- function(conn) {
  tags <- tryCatch(
    DBI::dbGetQuery(
      conn,
      "SELECT tags FROM duckdb_databases() WHERE database_name = current_database()"
    )$tags[[1]],
    error = function(e) NULL
  )

  if (!is.data.frame(tags) || nrow(tags) == 0) {
    return(NA_character_)
  }

  value <- tags$value[tags$key == "storage_version"]
  if (length(value) == 0) {
    return(NA_character_)
  }

  as.character(value[[1]])
}

ddbs_open_persistent <- function(
  dbdir,
  duckdb_storage_version = duckspatial_storage_default(),
  read_only = FALSE,
  ...
) {
  duckdb_storage_version <- match_duckdb_storage_version(duckdb_storage_version)

  if (dbdir %in% c(":memory:", "memory")) {
    conn <- DBI::dbConnect(
      duckdb::duckdb(dbdir = ":memory:"),
      geometry = "wk",
      ...
    )
    attr(conn, "duckspatial_storage_mode") <- "memory"
    return(conn)
  }

  is_new <- !file.exists(dbdir)
  config <- list()
  if (is_new && !identical(duckdb_storage_version, "v1.0.0")) {
    config$storage_compatibility_version <- duckdb_storage_version
  }

  conn <- DBI::dbConnect(
    duckdb::duckdb(dbdir = dbdir, read_only = read_only, config = config),
    geometry = "wk",
    ...
  )

  actual <- ddbs_duckdb_storage_tag(conn)
  requested <- if (duckdb_storage_version %in% c("v1.0.0", "latest")) {
    NA_character_
  } else {
    paste0(duckdb_storage_version, "+")
  }

  # Fallback to legacy (comment) mode if storage is older than v1.5.0
  legacy_mode <- !is_storage_v15_or_newer(actual)

  if (
    !legacy_mode &&
      !is.na(requested) &&
      !identical(actual, requested) &&
      !isTRUE(read_only)
  ) {
    # If they requested a specific modern version but got a different modern version,
    # we still use native spatial storage but warn.
    cli::cli_warn(
      c(
        "Requested DuckDB storage {.val {duckdb_storage_version}} but file storage is {.val {actual}}.",
        "i" = "Native CRS persistence will be used."
      ),
      class = "duckspatial_storage_mismatch"
    )
  } else if (legacy_mode && !identical(duckdb_storage_version, "v1.0.0") && !isTRUE(read_only)) {
    cli::cli_warn(
      c(
        "Requested DuckDB storage {.val {duckdb_storage_version}} but file storage is {.val {actual}} (Legacy).",
        "i" = "CRS persistence will use duckspatial-managed column-comment metadata for this compatibility connection."
      ),
      class = "duckspatial_storage_mismatch"
    )
  }

  attr(conn, "db_file") <- dbdir
  attr(conn, "duckspatial_storage") <- actual
  attr(conn, "duckspatial_storage_mode") <- if (legacy_mode) {
    "legacy"
  } else {
    duckdb_storage_version
  }
  conn
}

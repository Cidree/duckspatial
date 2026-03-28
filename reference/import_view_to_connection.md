# Import a view/table from one connection to another

Enables cross-connection operations by importing views using one of
three strategies.

## Usage

``` r
import_view_to_connection(
  target_conn,
  source_conn,
  source_object,
  target_name = NULL,
  quiet = FALSE
)
```

## Arguments

- target_conn:

  Target DuckDB connection

- source_conn:

  Source DuckDB connection

- source_object:

  duckspatial_df, tbl_lazy, or tbl_duckdb_connection from source_conn

- target_name:

  Name for view in target connection (auto-generated if NULL)

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

List with imported view name and cleanup function

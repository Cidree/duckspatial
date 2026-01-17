# Get DuckDB connection from an object

Extracts the connection from duckspatial_df, tbl_lazy, or validates a
direct connection.

## Usage

``` r
get_conn_from_input(x)
```

## Arguments

- x:

  A duckspatial_df, tbl_lazy, duckdb_connection, or NULL

## Value

A duckdb_connection or NULL

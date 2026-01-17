# Get or create default DuckDB connection with spatial extension installed and loaded

Get or create default DuckDB connection with spatial extension installed
and loaded

## Usage

``` r
ddbs_default_conn(create = TRUE)
```

## Arguments

- create:

  Logical. If TRUE and no connection exists, create one. Default is
  TRUE.

## Value

A `duckdb_connection` or NULL if no connection exists and create = FALSE

# Glimpse the status of a DuckDB extension

Retrieves the row from DuckDB's `duckdb_extensions()` catalog for a
given extension (the spatial extension by default) and prints a
transposed [`glimpse`](https://pillar.r-lib.org/reference/glimpse.html)
of it: whether it is installed and loaded, its version, install path,
description, and so on.

## Usage

``` r
ddbs_extension_info(conn = NULL, extension = "spatial")
```

## Arguments

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- extension:

  name of the extension to inspect, default is "spatial"

## Value

A one-row `tibble` with the extension's metadata (invisibly). Called
mainly for the glimpse printed as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)

# inspect the spatial extension on the default connection
ddbs_extension_info()

# or pass an explicit connection
conn <- ddbs_create_conn()
ddbs_extension_info(conn)
ddbs_stop_conn(conn)
} # }
```

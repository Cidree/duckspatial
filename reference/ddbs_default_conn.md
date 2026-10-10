# Get or create default DuckDB connection with spatial extension installed and loaded.

Get or create default DuckDB connection with spatial extension installed
and loaded.

## Usage

``` r
ddbs_default_conn(create = TRUE, ...)
```

## Arguments

- create:

  Logical. If TRUE and no connection exists, create one. Default is
  TRUE.

- ...:

  Additional parameters to pass to
  [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md)

## Value

A `duckdb_connection` or NULL if no connection exists and create = FALSE

## Details

The connection is created internally when the first ckspatial function
is run. Every function of the package runs on this connection when the
argument `conn` is `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)

# get/create the default connection
conn <- ddbs_default_conn()

} # }
```

# Get list of GDAL drivers and file formats

Get list of GDAL drivers and file formats

## Usage

``` r
ddbs_drivers(conn = NULL)
```

## Arguments

- conn:

  A `DBIConnection` object to a DuckDB database. If not specified
  (`conn = NULL`), uses the default connection created with
  [`ddbs_default_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_default_conn.md)
  (or a temporary one).

## Value

`data.frame`

## Examples

``` r
if (FALSE) { # interactive()
## load packages
library(duckdb)
library(duckspatial)

## database setup
conn <- dbConnect(duckdb())
ddbs_install(conn)
ddbs_load(conn)

## check drivers
ddbs_drivers(conn)
}
```

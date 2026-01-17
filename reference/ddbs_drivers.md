# Get list of GDAL drivers and file formats

Get list of GDAL drivers and file formats

## Usage

``` r
ddbs_drivers(conn)
```

## Arguments

- conn:

  A connection object to a DuckDB database

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

# Loads the Spatial extension

Checks if a spatial extension is installed, and loads it in a DuckDB
database

## Usage

``` r
ddbs_load(conn, quiet = FALSE)
```

## Arguments

- conn:

  A connection object to a DuckDB database

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

TRUE (invisibly) for successful installation

## Examples

``` r
if (FALSE) { # interactive()
## load packages
library(duckdb)
library(duckspatial)

## connect to in memory database
conn <- dbConnect(duckdb::duckdb())

## install the spatial exntesion
ddbs_install(conn)
ddbs_load(conn)

## disconnect from db
dbDisconnect(conn)
}
```

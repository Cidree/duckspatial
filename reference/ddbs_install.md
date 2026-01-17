# Checks and installs the Spatial extension

Checks if a spatial extension is available, and installs it in a DuckDB
database

## Usage

``` r
ddbs_install(conn, upgrade = FALSE, quiet = FALSE)
```

## Arguments

- conn:

  A connection object to a DuckDB database

- upgrade:

  if TRUE, it upgrades the DuckDB extension to the latest version

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

TRUE (invisibly) for successful installation

## Examples

``` r
## load packages
library(duckspatial)
library(duckdb)
#> Loading required package: DBI

# connect to in memory database
conn <- duckdb::dbConnect(duckdb::duckdb())

# install the spatial extension
ddbs_install(conn)
#> ✔ Spatial extension installed

# disconnect from db
duckdb::dbDisconnect(conn)
```

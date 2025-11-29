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
library(duckdb)
library(duckspatial)

# connect to in memory database
conn <- dbConnect(duckdb::duckdb())

# install the spatial extension
ddbs_install(conn)
#> ℹ spatial extension version <d83faf8> is already installed in this database

# disconnect from db
dbDisconnect(conn)
```

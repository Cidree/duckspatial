# Checks and installs the Spatial extension

Checks if a spatial extension is available, and installs it in a DuckDB
database

## Usage

``` r
ddbs_install(
  conn,
  upgrade = FALSE,
  quiet = FALSE,
  extension = "spatial",
  community = FALSE
)
```

## Arguments

- conn:

  A `DBIConnection` object to a DuckDB database

- upgrade:

  if TRUE, it upgrades the DuckDB extension to the latest version

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

- extension:

  name of the extension to install, default is "spatial"

- community:

  Logical. If TRUE, installs a community extension

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
#> ✔ spatial extension installed

# install the h3 community extension
ddbs_install(conn, extension = "h3", community = TRUE)
#> ✔ h3 extension installed

# disconnect from db
duckdb::dbDisconnect(conn)
```

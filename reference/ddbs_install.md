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
  repos = NULL
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

- repos:

  optional character string naming the repository to install the
  extension from (e.g. `"core"`, `"core_nightly"`, or `"community"`); a
  URL or path can also be supplied. If `NULL` (default), the `core`
  repository is tried first, then `community`. Switching an
  already-installed extension to a different repository requires
  `upgrade = TRUE`. See
  <https://duckdb.org/docs/stable/extensions/installing_extensions>.

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

# disconnect from db
duckdb::dbDisconnect(conn)

if (FALSE) { # \dontrun{
# install the h3 community extension (requires network access)
conn <- duckdb::dbConnect(duckdb::duckdb())
ddbs_install(conn, extension = "h3")
duckdb::dbDisconnect(conn)
} # }
```

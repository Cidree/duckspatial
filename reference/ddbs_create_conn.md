# Create a DuckDB connection with spatial extension

It creates a DuckDB connection, and then it installs and loads the
spatial extension

## Usage

``` r
ddbs_create_conn(dbdir = "memory", threads = NULL, memory_limit_gb = NULL)
```

## Arguments

- dbdir:

  String. Either `"tempdir"`, `"memory"`, or file path with `.duckdb` or
  `.db` extension. Defaults to `"memory"`.

- threads:

  Integer. Number of threads to use. If `NULL` (default), the setting is
  not changed, and DuckDB engine will use all available cores it detects
  (warning, on some shared HPC nodes the detected number of cores might
  be total number of cores on the node, not the per-job allocation).

- memory_limit_gb:

  Numeric. Memory limit in GB. If `NULL` (default), the setting is not
  changed, and DuckDB engine will use 80% of available operating system
  memory it detects (warning, on some shared HPC nodes the detected
  memory might be the full node memory, not the per-job allocation).

## Value

A `duckdb_connection`

## Examples

``` r
if (FALSE) { # \dontrun{
# load packages
library(duckspatial)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

# create a duckdb database in disk  (with spatial extension)
conn <- ddbs_create_conn(dbdir = "tempdir")

# create a connection with 1 thread and 2GB memory limit
conn <- ddbs_create_conn(threads = 1, memory_limit_gb = 2)
ddbs_stop_conn(conn)
} # }
```

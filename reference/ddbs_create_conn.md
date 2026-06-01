# Create a DuckDB connection with spatial extension

It creates a DuckDB connection, and then it installs and loads the
spatial extension

## Usage

``` r
ddbs_create_conn(
  dbdir = "memory",
  threads = NULL,
  memory_limit_gb = NULL,
  upgrade = FALSE,
  ...,
  duckdb_storage_version = duckspatial_storage_default()
)
```

## Arguments

- dbdir:

  String. Either `"tempdir"`, `"memory"`, or a DuckDB database file path
  with `.duckdb`, `.db`, or `.ddb` extension. Defaults to `"memory"`.

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

- upgrade:

  if TRUE, it upgrades the DuckDB extension to the latest version

- ...:

  Additional parameters to be passed to
  [`dbConnect`](https://dbi.r-dbi.org/reference/dbConnect.html)

- duckdb_storage_version:

  Storage compatibility for newly created persistent native DuckDB files
  (`.duckdb`, `.db`, `.ddb`). See
  <https://duckdb.org/docs/internals/storage> for more information on
  DuckDB storage versions and compatibility.

  - `"v1.5.0"` (**Native Spatial Storage**, Default): Preserves CRS
    metadata in native DuckDB `GEOMETRY` columns. Requires DuckDB \>=
    1.5.0 to open the file.

  - `"v1.0.0"` (**Legacy Compatibility**): Creates files readable by
    older DuckDB versions (\>= 1.0.0). Persists CRS metadata in
    duckspatial-managed column comments (a convention not recognized by
    other spatial software).

  - `"latest"`: Use the highest storage version supported by your
    installed DuckDB engine.

  Other major version strings like `"v1.4.0"`, `"v1.3.0"`, etc., are
  also supported.

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

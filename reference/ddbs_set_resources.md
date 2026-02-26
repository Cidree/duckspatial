# Get or set connection resources

Configure technical system settings for a DuckDB connection, such as
memory limits and CPU threads.

## Usage

``` r
ddbs_set_resources(conn, threads = NULL, memory_limit_gb = NULL)

ddbs_get_resources(conn)
```

## Arguments

- conn:

  A `DBIConnection` object to a DuckDB database

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

For `ddbs_set_resources()`, invisibly returns a list containing the
current system settings; for `ddbs_get_resources()`, visibly returns the
same list for direct inspection.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a connection
conn <- ddbs_create_conn()

# Set resources: 1 thread and 4GB
ddbs_set_resources(conn, threads = 1, memory_limit_gb = 4)

# Check current settings
ddbs_get_resources(conn)

ddbs_stop_conn(conn)
} # }
```

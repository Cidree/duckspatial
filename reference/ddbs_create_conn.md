# Create a DuckDB connection with spatial extension

It creates a DuckDB connection, and then it installs and loads the
spatial extension

## Usage

``` r
ddbs_create_conn(dbdir = "memory")
```

## Arguments

- dbdir:

  String. Either `"tempdir"` or `"memory"`. Defaults to `"memory"`.

## Value

A `duckdb_connection`

## Examples

``` r
if (FALSE) { # interactive()
# load packages
library(duckspatial)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

# create a duckdb database in disk  (with spatial extension)
conn <- ddbs_create_conn(dbdir = "tempdir")
}
```

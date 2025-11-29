# Create a duckdb connection

Create a duckdb connection

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

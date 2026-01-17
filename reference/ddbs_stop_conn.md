# Close a duckdb connection

Close a duckdb connection

## Usage

``` r
ddbs_stop_conn(conn)
```

## Arguments

- conn:

  A connection object to a DuckDB database

## Value

TRUE (invisibly) for successful disconnection

## Examples

``` r
if (FALSE) { # interactive()
## load packages
library(duckspatial)

## create an in-memory duckdb database
conn <- ddbs_create_conn(dbdir = "memory")

## close the connection
ddbs_stop_conn(conn)
}
```

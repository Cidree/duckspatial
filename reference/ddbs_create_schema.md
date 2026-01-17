# Check and create schema

Check and create schema

## Usage

``` r
ddbs_create_schema(conn, name, quiet = FALSE)
```

## Arguments

- conn:

  A connection object to a DuckDB database

- name:

  A character string with the name of the schema to be created

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

TRUE (invisibly) for successful schema creation

## Examples

``` r
## load packages
if (FALSE) { # \dontrun{
library(duckspatial)
library(duckdb)

## connect to in memory database
conn <- ddbs_create_conn(dbdir = "memory")

## create a new schema
ddbs_create_schema(conn, "new_schema")

## check schemas
dbGetQuery(conn, "SELECT * FROM information_schema.schemata;")

## disconnect from db
ddbs_stop_conn(conn)
} # }
```

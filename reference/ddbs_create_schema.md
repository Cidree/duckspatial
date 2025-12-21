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
library(duckspatial)
library(duckdb)
#> Loading required package: DBI

## connect to in memory database
conn <- ddbs_create_conn(dbdir = "memory")

## create a new schema
ddbs_create_schema(conn, "new_schema")
#> ✔ Schema new_schema created

## check schemas
dbGetQuery(conn, "SELECT * FROM information_schema.schemata;")
#>   catalog_name        schema_name schema_owner default_character_set_catalog
#> 1       memory               main       duckdb                          <NA>
#> 2       memory         new_schema       duckdb                          <NA>
#> 3       system information_schema       duckdb                          <NA>
#> 4       system               main       duckdb                          <NA>
#> 5       system         pg_catalog       duckdb                          <NA>
#> 6         temp               main       duckdb                          <NA>
#>   default_character_set_schema default_character_set_name sql_path
#> 1                         <NA>                       <NA>     <NA>
#> 2                         <NA>                       <NA>     <NA>
#> 3                         <NA>                       <NA>     <NA>
#> 4                         <NA>                       <NA>     <NA>
#> 5                         <NA>                       <NA>     <NA>
#> 6                         <NA>                       <NA>     <NA>

## disconnect from db
ddbs_stop_conn(conn)
```

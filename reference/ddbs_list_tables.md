# Check tables and schemas inside a database

Check tables and schemas inside a database

## Usage

``` r
ddbs_list_tables(conn)
```

## Arguments

- conn:

  A `DBIConnection` object to a DuckDB database

## Value

`data.frame`

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)

## create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read some data
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
)

argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", 
  package = "duckspatial")
)

## insert into the database
ddbs_write_vector(conn, argentina_ddbs, "argentina")
ddbs_write_vector(conn, countries_ddbs, "countries")

## list tables in the database
ddbs_list_tables(conn)
} # }
```

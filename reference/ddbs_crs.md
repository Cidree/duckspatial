# Check CRS of a table

Check CRS of a table

## Usage

``` r
ddbs_crs(conn, name, crs_column = "crs_duckspatial")
```

## Arguments

- conn:

  A connection object to a DuckDB database

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md))

## Value

CRS object

## Examples

``` r
if (FALSE) { # interactive()
## load packages
library(duckdb)
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))

## store in duckdb
ddbs_write_vector(conn, countries_sf, "countries")

## check CRS
ddbs_crs(conn, "countries")
}
```

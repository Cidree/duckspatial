# Calculates the difference of two geometries

Calculates the geometric difference of two geometries, and returns a
`sf` object or creates a new table

## Usage

``` r
ddbs_difference(
  x,
  y,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- y:

  A table with geometry column within the DuckDB database

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- crs:

  The coordinates reference system of the data. Specify if the data
  doesn't have a `crs_column`, and you know the CRS.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

An sf object or TRUE (invisibly) for table creation

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))

## store in duckdb
ddbs_write_vector(conn, countries_sf, "countries")
ddbs_write_vector(conn, argentina_sf, "argentina")

## difference with a connection
ddbs_difference("countries", "argentina", conn)

## difference without a connection
ddbs_difference(countries_sf, argentina_sf)
} # }
```

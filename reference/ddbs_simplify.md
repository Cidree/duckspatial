# Simplify geometries

Simplifies geometries from a DuckDB table using the Douglas-Peucker
algorithm via the spatial extension. Returns the result as an `sf`
object or creates a new table in the database.

## Usage

``` r
ddbs_simplify(
  x,
  conn = NULL,
  name = NULL,
  tolerance,
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

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- tolerance:

  Tolerance distance for simplification. Larger values result in more
  simplified geometries.

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

an `sf` object with simplified geometries or `TRUE` (invisibly) for
table creation

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

## store in duckdb
ddbs_write_vector(conn, countries_sf, "countries")

## simplify with tolerance of 0.01
ddbs_simplify("countries", conn, tolerance = 0.01)

## simplify without using a connection
ddbs_simplify(countries_sf, tolerance = 0.01)
} # }
```

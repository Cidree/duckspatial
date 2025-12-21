# Shift geometries by X and Y offsets

Shifts (translates) geometries from a `sf` object or a DuckDB table.
Returns the result as an `sf` object or creates a new table in the
database. This function is equivalent to `terra::shift()`.

## Usage

``` r
ddbs_shift(
  x,
  dx = 0,
  dy = 0,
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

- dx:

  numeric value specifying the shift in the X direction
  (longitude/easting)

- dy:

  numeric value specifying the shift in the Y direction
  (latitude/northing)

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

an `sf` object or `TRUE` (invisibly) for table creation

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))

## store in duckdb
ddbs_write_vector(conn, argentina_sf, "argentina")

## shift 10 degrees east and 5 degrees north
ddbs_shift(conn = conn, "argentina", dx = 10, dy = 5)

## shift without using a connection
ddbs_shift(argentina_sf, dx = 10, dy = 5)
} # }
```

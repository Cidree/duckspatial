# Rotate geometries around centroid

Rotates geometries from from a `sf` object or a DuckDB table. Returns
the result as an `sf` object or creates a new table in the database.

## Usage

``` r
ddbs_rotate(
  x,
  angle,
  units = c("degrees", "radians"),
  by_feature = FALSE,
  center_x = NULL,
  center_y = NULL,
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

- angle:

  a numeric value specifying the rotation angle

- units:

  character string specifying angle units: "degrees" (default) or
  "radians"

- by_feature:

  Logical. If `TRUE`, the geometric operation is applied separately to
  each geometry. If `FALSE` (default), the geometric operation is
  applied to the data as a whole.

- center_x:

  numeric value for the X coordinate of rotation center. If NULL,
  rotates around the centroid of each geometry

- center_y:

  numeric value for the Y coordinate of rotation center. If NULL,
  rotates around the centroid of each geometry

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

## rotate 45 degrees
ddbs_rotate(conn = conn, "argentina", angle = 45)

## rotate 90 degrees around a specific point
ddbs_rotate(conn = conn, "argentina", angle = 90, center_x = -64, center_y = -34)

## rotate without using a connection
ddbs_rotate(argentina_sf, angle = 45)
} # }
```

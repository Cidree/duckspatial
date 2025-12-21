# Shear geometries

Applies a shear transformation to geometries from a `sf` object or a
DuckDB table. Returns the result as an `sf` object or creates a new
table in the database. Shearing skews the geometry by shifting
coordinates proportionally.

## Usage

``` r
ddbs_shear(
  x,
  x_shear = 0,
  y_shear = 0,
  by_feature = FALSE,
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

- x_shear:

  numeric value specifying the shear factor in the X direction (default
  = 0). For each unit in Y, X coordinates are shifted by this amount

- y_shear:

  numeric value specifying the shear factor in the Y direction (default
  = 0). For each unit in X, Y coordinates are shifted by this amount

- by_feature:

  Logical. If `TRUE`, the geometric operation is applied separately to
  each geometry. If `FALSE` (default), the geometric operation is
  applied to the data as a whole.

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
countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
  filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))

## store in duckdb
ddbs_write_vector(conn, countries_sf, "countries")

## shear in X direction (creates italic-like effect)
ddbs_shear(conn = conn, "countries", x_shear = 0.3, y_shear = 0)

## shear in Y direction
ddbs_shear(conn = conn, "countries", x_shear = 0, y_shear = 0.3)

## shear in both directions
ddbs_shear(conn = conn, "countries", x_shear = 0.2, y_shear = 0.2)

## shear without using a connection
ddbs_shear(countries_sf, x_shear = 0.3, y_shear = 0)
} # }
```

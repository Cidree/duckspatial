# Scale geometries by X and Y factors

Scales geometries around the centroid of the geometry. Returns the
result as an `sf` object or creates a new table in the database.

## Usage

``` r
ddbs_scale(
  x,
  x_scale = 1,
  y_scale = 1,
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

- x_scale:

  numeric value specifying the scaling factor in the X direction
  (default = 1)

- y_scale:

  numeric value specifying the scaling factor in the Y direction
  (default = 1)

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

## scale to 150% in both directions
ddbs_scale(conn = conn, "countries", x_scale = 1.5, y_scale = 1.5)

## scale to 200% horizontally, 50% vertically
ddbs_scale(conn = conn, "countries", x_scale = 2, y_scale = 0.5)

## scale all features together (default)
ddbs_scale(countries_sf, x_scale = 1.5, y_scale = 1.5, by_feature = FALSE)

## scale each feature independently
ddbs_scale(countries_sf, x_scale = 1.5, y_scale = 1.5, by_feature = TRUE)

} # }
```

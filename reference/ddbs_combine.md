# Combine geometries into a single MULTI-geometry

Combines all geometries from a `sf` object or a DuckDB table into a
single MULTI-geometry using the spatial extension. This is equivalent to
[`sf::st_combine()`](https://r-spatial.github.io/sf/reference/geos_combine.html).
Returns the result as an `sf` object or creates a new table in the
database.

## Usage

``` r
ddbs_combine(
  x,
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

  character string specifying the name of the CRS column. Default is
  `"crs_duckspatial"`

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
# load packages
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

# read data
countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))

# store in duckdb
ddbs_write_vector(conn, countries_sf, "countries")

# combine all geometries into one
ddbs_combine(conn = conn, "countries")

# combine without using a connection
ddbs_combine(countries_sf)

# store result in a new table
ddbs_combine(conn = conn, "countries", name = "countries_combined")
} # }
```

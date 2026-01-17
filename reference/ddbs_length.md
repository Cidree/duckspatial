# Calculates the length of geometries

Calculates the length of geometries from a DuckDB table or a `sf` object
Returns the result as an `sf` object with a length column or creates a
new table in the database. Note: Length units depend on the CRS of the
input geometries (e.g., meters for projected CRS, or degrees for
geographic CRS).

## Usage

``` r
ddbs_length(
  x,
  conn = NULL,
  name = NULL,
  new_column = NULL,
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

- new_column:

  Name of the new column to create on the input data. If NULL, the
  function will return a vector with the result

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
rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))

## store in duckdb
ddbs_write_vector(conn, rivers_sf, "rivers")

## calculate length (returns sf object with length column)
ddbs_length("rivers", conn)

## calculate length with custom column name
ddbs_length("rivers", conn, new_column = "length_meters")

## create a new table with length calculations
ddbs_length("rivers", conn, name = "rivers_with_length")

## calculate length in a sf object (without a connection)
ddbs_length(rivers_sf)
} # }
```

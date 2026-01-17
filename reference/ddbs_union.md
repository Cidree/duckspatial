# Union of geometries

Computes the union of geometries from a `sf` objects or a DuckDB tables
using. This is equivalent to
[`sf::st_union()`](https://r-spatial.github.io/sf/reference/geos_combine.html).
The function supports three modes: (1) union all geometries from a
single object into one geometry, (2) union geometries from a single
object grouped by one or more columns, (3) union geometries from two
different objects. Returns the result as an `sf` object or creates a new
table in the database.

## Usage

``` r
ddbs_union(
  x,
  y = NULL,
  by = NULL,
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

  optional. A second table name, `sf` object, or DuckDB connection to
  compute the pairwise union between geometries in `x` and `y`. Default
  is `NULL`

- by:

  optional. Character vector specifying one or more column names to
  group by when computing unions. Geometries will be unioned within each
  group. Default is `NULL`

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
rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))

# store in duckdb
ddbs_write_vector(conn, rivers_sf, "rivers")

# union all geometries into one
ddbs_union(conn = conn, "rivers")

# union without using a connection
ddbs_union(rivers_sf)

# union geometries grouped by a column
ddbs_union(conn = conn, "rivers", by = "RIVER_NAME")

# store result in a new table
ddbs_union(conn = conn, "rivers", name = "rivers_union")
} # }
```

# Geometry validation functions

Functions to check various geometric properties and validity conditions
of spatial geometries using DuckDB's spatial extension.

## Usage

``` r
ddbs_is_simple(
  x,
  new_column = "is_simple",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_is_valid(
  x,
  new_column = "is_valid",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_is_closed(
  x,
  new_column = "is_closed",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_is_empty(
  x,
  new_column = "is_empty",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_is_ring(
  x,
  new_column = "is_ring",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

  Data is returned from this object.

- new_column:

  Name of the new column to create on the input data. Ignored with
  `mode = "sf"`.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- mode:

  Character. Controls the return type. Options:

  - `"duckspatial"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB

  - `"sf"`: Eagerly collected sf object (uses memory)

  Can be set globally via
  [`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)`(mode = "...")`
  or per-function via this argument. Per-function overrides global
  setting.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

- `mode = "duckspatial"` (default): A `duckspatial_df` (lazy spatial
  data frame) backed by dbplyr/DuckDB.

- `mode = "sf"`: An eagerly collected vector in R memory.

- When `name` is provided: writes the table in the DuckDB connection and
  returns `TRUE` (invisibly).

## Details

These functions provide different types of geometric validation. Note
that by default, the functions add a new column as a logical vector.
This behaviour allows to filter the data within DuckDB without the need
or materializating a vector in R (see details).

- `ddbs_is_valid()` checks if a geometry is valid according to the OGC
  Simple Features specification. Invalid geometries may have issues like
  self-intersections in polygons, duplicate points, or incorrect ring
  orientations.

- `ddbs_is_simple()` determines whether geometries are simple, meaning
  they are free of self-intersections. For example, a linestring that
  crosses itself is not simple.

- `ddbs_is_ring()` checks if a linestring geometry is closed (first and
  last points are identical) and simple (no self-intersections), forming
  a valid ring.

- `ddbs_is_empty()` tests whether a geometry is empty, containing no
  points. Empty geometries are valid but represent the absence of
  spatial information.

- `ddbs_is_closed()` determines if a linestring geometry is closed,
  meaning the first and last coordinates are identical. Unlike
  `ddbs_is_ring()`, this does not check for simplicity.

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)
library(dplyr)

## create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
)

rivers_ddbs <- ddbs_open_dataset(
  system.file("spatial/rivers.geojson", 
  package = "duckspatial")
)

## geometry validation
ddbs_is_valid(countries_ddbs)
ddbs_is_simple(countries_ddbs)
ddbs_is_ring(rivers_ddbs)
ddbs_is_empty(countries_ddbs)
ddbs_is_closed(countries_ddbs)

## filter invalid countries
ddbs_is_valid(countries_ddbs) |> filter(!is_valid)
} # }
```

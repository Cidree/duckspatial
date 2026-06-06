# Count geometry components

Functions to count the number of points or sub-geometries in a geometry

## Usage

``` r
ddbs_get_npoints(
  x,
  new_column = "npoints",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_get_ngeometries(
  x,
  new_column = "ngeometries",
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

Depends on the `mode` argument (or global preference set by
[`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)):

- `duckspatial` (default): A `duckspatial_df` (lazy spatial data frame)
  backed by dbplyr/DuckDB.

- `sf`: An eagerly collected object in R memory, that will return the
  same data type as the `sf` equivalent (e.g. `sf` or `units` vector).

When `name` is provided, the result is also written as a table or view
in DuckDB and the function returns `TRUE` (invisibly).

## Details

These functions query structural properties of geometries:

- `ddbs_get_npoints()` returns the number of points (vertices) in a
  geometry. For LINESTRING geometries this is the vertex count; for
  POLYGON types it includes all vertices of the exterior ring and any
  interior rings.

- `ddbs_get_ngeometries()` returns the number of sub-geometries in a
  GEOMETRYCOLLECTION or MULTI\* geometry (e.g. MULTIPOLYGON,
  MULTILINESTRING). Returns 1 for simple (non-collection) geometry
  types.

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(dplyr)
library(duckspatial)

## read data
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson",
  package = "duckspatial")
)

## count points and sub-geometries
ddbs_get_npoints(countries_ddbs)
ddbs_get_ngeometries(countries_ddbs)
} # }
```

# Remove repeated points from a geometry

Removes duplicate consecutive vertices from geometries, optionally
within a tolerance distance.

## Usage

``` r
ddbs_remove_repeated_points(
  x,
  tolerance = 0,
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

- tolerance:

  A numeric value specifying the minimum distance between consecutive
  vertices. Vertices closer than this threshold are considered repeated
  and removed. Default is `0`, which removes only exact duplicates.

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

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)
library(sf)

## Create a polygon with repeated points
poly <- st_polygon(list(matrix(
  c(0, 0,
    1, 0,
    1, 0,  # repeated point
    1, 1,
    0, 1,
    0, 0),
  ncol = 2, byrow = TRUE
)))

poly_sf <- st_as_sf(st_sfc(poly))

## remove exact duplicate consecutive vertices
ddbs_remove_repeated_points(poly_sf)

## remove vertices within a tolerance of 1 unit
ddbs_remove_repeated_points(poly_sf, tolerance = 1)
} # }
```

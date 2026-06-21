# Create point geometries from coordinate vectors

Constructs POINT geometries from numeric coordinate vectors, optionally
including Z (elevation) and M (measure) dimensions and extra attribute
columns.

## Usage

``` r
ddbs_point(
  x,
  y,
  z = NULL,
  m = NULL,
  ...,
  crs = NULL,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  Numeric vector of X (longitude) coordinates.

- y:

  Numeric vector of Y (latitude) coordinates.

- z:

  Optional numeric vector of Z (elevation) coordinates.

- m:

  Optional numeric vector of M (measure) coordinates. Requires `z`.

- ...:

  Named vectors of additional attribute columns to include in the
  output. Each must have the same length as `x`.

- crs:

  Character or numeric CRS specification (e.g. `"EPSG:4326"` or `4326`).
  Defaults to `NULL` (no CRS assigned).

- geom_col:

  Name of the geometry column in the output. Defaults to `"geometry"`.

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
library(duckspatial)

## 2D points
ddbs_point(
  x = c(-58.38, -64.18, -60.64),
  y = c(-34.60, -31.42, -32.95),
  crs = 4326
)

## 3D points with extra columns
ddbs_point(
  x   = c(0, 1, 2),
  y   = c(0, 1, 2),
  z   = c(10, 20, 30),
  id  = 1:3,
  crs = 4326
)
} # }
```

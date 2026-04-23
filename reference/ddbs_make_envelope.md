# Create a rectangular polygon from bounding coordinates

Creates a rectangular POLYGON geometry from four bounding coordinates.

## Usage

``` r
ddbs_make_envelope(
  xmin,
  ymin,
  xmax,
  ymax,
  crs = "EPSG:4326",
  name = NULL,
  conn = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- xmin:

  A numeric value for the minimum X (longitude) coordinate.

- ymin:

  A numeric value for the minimum Y (latitude) coordinate.

- xmax:

  A numeric value for the maximum X (longitude) coordinate.

- ymax:

  A numeric value for the maximum Y (latitude) coordinate.

- crs:

  A character string specifying the coordinate reference system of the
  output geometry. Default is `"EPSG:4326"`.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

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

## without storing in duckdb
finland_bbox_ddbs <- ddbs_make_envelope(
  xmin = 19.1, ymin = 59.7,
  xmax = 31.6, ymax = 70.1
)
} # }
```

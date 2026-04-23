# Interpolates a point or points along a line geometry

Returns either a single point at a specified position along a line, or
multiple equally-spaced points along a line, depending on the value of
`intervals`. When `intervals = FALSE`, this wraps
`ST_LineInterpolatePoint`; when `intervals = TRUE`, it wraps
`ST_LineInterpolatePoints`.

## Usage

``` r
ddbs_line_interpolate(
  x,
  fraction = 0.5,
  intervals = FALSE,
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

- fraction:

  a numeric value between 0 and 1. When `intervals = FALSE`, specifies
  the position along the line to interpolate, where `0` is the start and
  `1` is the end. When `intervals = TRUE`, specifies the spacing between
  interpolated points as a proportion of the total line length. Defaults
  to `0.5`.

- intervals:

  a logical value. If `FALSE` (default), returns a single `POINT` at the
  position given by `fraction`. If `TRUE`, returns a `MULTIPOINT` of
  equally-spaced points along the line at intervals defined by
  `fraction`.

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

## read data
rivers_ddbs <- ddbs_open_dataset(
  system.file("spatial/rivers.geojson",
  package = "duckspatial")
)

## return the midpoint of a line (default)
ddbs_line_interpolate(rivers_ddbs)

## return the point 25% along the line
ddbs_line_interpolate(rivers_ddbs, fraction = 0.25)

## return equally-spaced points every 10% of the line length
ddbs_line_interpolate(rivers_ddbs, fraction = 0.1, intervals = TRUE)

## return equally-spaced points every 50% of the line length (i.e. midpoint and end)
ddbs_line_interpolate(rivers_ddbs, fraction = 0.5, intervals = TRUE)
} # }
```

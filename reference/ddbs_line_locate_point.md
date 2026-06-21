# Locate a point along a linestring

Returns a value between 0 and 1 representing the position of the closest
point on the line to the reference point, as a fraction of the line's
total length. 0 corresponds to the start of the line and 1 to the end.

## Usage

``` r
ddbs_line_locate_point(
  x,
  y,
  new_column = "line_fraction",
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

- y:

  The reference point. One of:

  - An `sf` object with exactly 1 point feature.

  - A `duckspatial_df` with exactly 1 point feature.

  - A character string naming a DuckDB table that contains exactly 1
    point feature (requires `conn`).

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

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)

## read river data
rivers_ddbs <- ddbs_open_dataset(
  system.file("spatial/rivers.geojson", package = "duckspatial")
)

## define a single reference point (sf with 1 row)
ref_pt <- sf::st_sf(
  geometry = sf::st_sfc(sf::st_point(c(-8, 43)), crs = 4326)
)

## locate the fraction along each river closest to the reference point
ddbs_line_locate_point(rivers_ddbs, y = ref_pt)
} # }
```

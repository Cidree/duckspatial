# Locate geometries at specific M values

Return the geometries at a specific M values or range of M values.

## Usage

``` r
ddbs_locate_along(
  x,
  measure,
  offset = 0,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_locate_between(
  x,
  start_measure,
  end_measure,
  offset = 0,
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

- measure:

  A numeric value specifying the M value at which to locate a point
  along the geometry. Used only by `ddbs_locate_along`.

- offset:

  A numeric value specifying a lateral offset to apply perpendicular to
  the line direction at the located point(s). Default is `0` (no
  offset).

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

- start_measure:

  A numeric value specifying the lower bound of the M range.

- end_measure:

  A numeric value specifying the upper bound of the M range.

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

- `ddbs_locate_along()`: returns a point or multi-point, containing the
  point(s) at the geometry with the given measure

- `ddbs_locate_between()`: returns a geometry or geometry collection
  created by filtering and interpolating vertices within a range of "M"
  values

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)

## read data (must contain M-enabled linestring geometries)
rivers_ddbs <- ddbs_open_dataset(
  system.file("spatial/rivers.geojson",
  package = "duckspatial")
)

## Calculate the length of the rivers
rivers_agg_ddbs <- rivers_ddbs |> 
  ddbs_union_agg("RIVER_NAME") |> 
  ddbs_length()

## Add M dimension to the rivers
rivers_m_ddbs <- rivers_agg_ddbs |> 
  ddbs_force_3d("length", dim = "M")

## Locate rivers with M between 10000 and 20000
ddbs_locate_between(rivers_m_ddbs, 10000, 20000)

} # }
```

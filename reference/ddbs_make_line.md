# Create lines from point geometries

Aggregates point geometries into a single LINESTRING by connecting them
in their original order. Optionally, lines can be created per group
using the `by` argument.

## Usage

``` r
ddbs_make_line(
  x,
  by = NULL,
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

- by:

  A character vector of column names to group by before aggregating
  points into lines. If `NULL` (default), all points are combined into a
  single line.

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

Connects input POINT geometries into a LINESTRING in row order (row 1 →
row 2 → …). To control the connection order, sort the data beforehand
with
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)

## read data
points_ddbs <- ddbs_open_dataset(
  system.file("spatial/points.gpkg", package = "duckspatial")
)

## create a single line from all points
ddbs_make_line(points_ddbs)

## create lines grouped by a column
ddbs_make_line(points_ddbs, by = "type")

## return as sf object
ddbs_make_line(points_ddbs, by = "type", mode = "sf")

## screate lines groupping by 2 columns
ddbs_make_line(points_ddbs, by = c("type", "class"))
} # }
```

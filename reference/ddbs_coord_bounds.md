# Coordinate bounds of geometries

Returns the minimum or maximum value of a specific coordinate axis
across all points of a geometry. When `by_feature = TRUE` (default), a
value is computed per row. When `by_feature = FALSE`, a single global
value is returned for the entire dataset.

## Usage

``` r
ddbs_xmax(
  x,
  new_column = "xmax",
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_xmin(
  x,
  new_column = "xmin",
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_ymax(
  x,
  new_column = "ymax",
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_ymin(
  x,
  new_column = "ymin",
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_zmax(
  x,
  new_column = "zmax",
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_zmin(
  x,
  new_column = "zmin",
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_mmax(
  x,
  new_column = "mmax",
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_mmin(
  x,
  new_column = "mmin",
  by_feature = TRUE,
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

  Name of the new column. Defaults to the lowercase function name (e.g.
  `"xmax"`, `"xmin"`, `"ymax"`, ...).

- by_feature:

  Logical. If `TRUE`, the geometric operation is applied separately to
  each geometry. If `FALSE`, the geometric operation is applied to the
  data as a whole.

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

- `by_feature = TRUE` and `mode = "duckspatial"` (default): A
  `duckspatial_df`.

- `by_feature = TRUE` and `mode = "sf"`: A numeric vector.

- `by_feature = FALSE`: A single `numeric` scalar.

- When `name` is provided: writes the table in DuckDB and returns `TRUE`
  (invisibly).

## Details

- `ddbs_xmax()` / `ddbs_xmin()`: maximum / minimum X coordinate.

- `ddbs_ymax()` / `ddbs_ymin()`: maximum / minimum Y coordinate.

- `ddbs_zmax()` / `ddbs_zmin()`: maximum / minimum Z coordinate.

- `ddbs_mmax()` / `ddbs_mmin()`: maximum / minimum M (measure)
  coordinate.

When `by_feature = FALSE`, the result is always a single `numeric`
scalar representing the global extreme across the entire dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)

argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", package = "duckspatial")
)

## per-feature X extent (default)
ddbs_xmax(argentina_ddbs)
ddbs_xmin(argentina_ddbs)

## global bounding values
ddbs_xmax(argentina_ddbs, by_feature = FALSE)
ddbs_ymin(argentina_ddbs, by_feature = FALSE)
} # }
```

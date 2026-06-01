# Apply an affine transformation to geometries

Applies an affine transformation to geometries using a 2x3 or 3x4
matrix.

## Usage

``` r
ddbs_affine(
  x,
  matrix,
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

- matrix:

  A numeric matrix specifying the transformation:

  - 2x3: 2D transformation with rows `[a, b, xoff]` and `[d, e, yoff]`

  - 3x4: 3D transformation with rows `[a, b, c, xoff]`,
    `[d, e, f, yoff]`, and `[g, h, i, zoff]`

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
argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson",
  package = "duckspatial")
)

## 2D translation (shift x by 1, y by 2)
mat_2d <- matrix(c(1, 0, 1,
                   0, 1, 2), nrow = 2, byrow = TRUE)
ddbs_affine(argentina_ddbs, mat_2d)

## 2D scaling (scale x by 2, y by 3)
mat_scale <- matrix(c(2, 0, 0,
                      0, 3, 0), nrow = 2, byrow = TRUE)
ddbs_affine(argentina_ddbs, mat_scale)

## 3D transformation
mat_3d <- matrix(c(1, 0, 0, 1,
                   0, 1, 0, 2,
                   0, 0, 1, 0), nrow = 3, byrow = TRUE)
ddbs_affine(argentina_ddbs, mat_3d)
} # }
```

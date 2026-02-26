# Build polygon areas from multiple linestrings

Constructs polygon or multipolygon geometries from a collection of
linestrings, handling intersections and creating unified areas. Returns
POLYGON or MULTIPOLYGON (not wrapped in a geometry collection). Requires
MULTILINESTRING input - for single linestrings, use
[`ddbs_make_polygon()`](https://cidree.github.io/duckspatial/reference/ddbs_make_polygon.md).

## Usage

``` r
ddbs_build_area(
  x,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = NULL,
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

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- crs:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) The coordinates
  reference system of the data. Specify if the data doesn't have a
  `crs_column`, and you know the CRS.

- crs_column:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) a character
  string of length one specifying the column storing the CRS (created
  automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- output:

  Character. Controls the return type. Options:

  - `"duckspatial_df"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB

  - `"sf"`: Eagerly collected sf object (uses memory)

  - `"tibble"`: Eagerly collected tibble without geometry

  - `"raw"`: Eagerly collected tibble with WKB geometry (list of raw
    vectors)

  - `"geoarrow"`: Eagerly collected tibble with geoarrow geometry
    (geoarrow_vctr)

  Can be set globally via
  [`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)`(output_type = "...")`
  or per-function via this argument. Per-function overrides global
  setting.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

Depends on the `output` argument (or global preference set by
[`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)):

- `duckspatial_df` (default): A lazy spatial data frame backed by
  dbplyr/DuckDB.

- `sf`: An eagerly collected `sf` object in R memory.

- `tibble`: An eagerly collected `tibble` without geometry in R memory.

- `raw`: An eagerly collected `tibble` with WKB geometry (no
  conversion).

- `geoarrow`: An eagerly collected `tibble` with geometry converted to
  `geoarrow_vctr`.

When `name` is provided, the result is also written as a table or view
in DuckDB and the function returns `TRUE` (invisibly).

## See also

[`ddbs_make_polygon()`](https://cidree.github.io/duckspatial/reference/ddbs_make_polygon.md),
[`ddbs_polygonize()`](https://cidree.github.io/duckspatial/reference/ddbs_polygonize.md)

Other polygon construction:
[`ddbs_make_polygon()`](https://cidree.github.io/duckspatial/reference/ddbs_make_polygon.md),
[`ddbs_polygonize()`](https://cidree.github.io/duckspatial/reference/ddbs_polygonize.md)

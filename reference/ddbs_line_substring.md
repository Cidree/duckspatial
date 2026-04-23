# Extract a substring of a line geometry

Returns the portion of a line between two fractional positions along its
length

## Usage

``` r
ddbs_line_substring(
  x,
  start = 0,
  end = 0.5,
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

- start:

  a numeric value between 0 and 1. Specifies the starting position along
  the line as a proportion of its total length, where `0` is the
  beginning of the line. Defaults to `0`.

- end:

  a numeric value between 0 and 1. Specifies the ending position along
  the line as a proportion of its total length, where `1` is the end of
  the line. Must be greater than or equal to `start`. Defaults to `0.5`.

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

## return the first half of each line (default)
ddbs_line_substring(rivers_ddbs)

## return the middle third of each line
ddbs_line_substring(rivers_ddbs, start = 0.33, end = 0.67)

## return the last quarter of each line
ddbs_line_substring(rivers_ddbs, start = 0.75, end = 1)
} # }
```

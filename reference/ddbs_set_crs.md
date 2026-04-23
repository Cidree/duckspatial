# Set the coordinate reference system of geometries

Assigns or replaces the coordinate reference system (CRS) of geometries
without transforming their coordinates. This is useful when the CRS is
missing or incorrectly defined.

## Usage

``` r
ddbs_set_crs(
  x,
  y,
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

  Target CRS. Can be:

  - A character string with EPSG code (e.g., "EPSG:4326")

  - A `crs` object created with
    [sf::st_crs](https://r-spatial.github.io/sf/reference/st_crs.html)

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
library(sf)

## read data
rivers_ddbs <- ddbs_open_dataset(
  system.file("spatial/rivers.geojson",
  package = "duckspatial")
)

## Remove CRS
rivers_no_crs_ddbs <- ddbs_set_crs(rivers_ddbs, st_crs(NA))

## Set the CRS back
ddbs_set_crs(rivers_no_crs_ddbs, "EPSG:3035")
} # }
```

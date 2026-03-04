# Check geometry dimensions

Functions to check whether geometries have Z (elevation) or M (measure)
dimensions

## Usage

``` r
ddbs_has_z(
  x,
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  new_column = "has_z",
  crs = NULL,
  crs_column = "crs_duckspatial",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_has_m(
  x,
  by_feature = TRUE,
  conn = NULL,
  name = NULL,
  new_column = "has_m",
  crs = NULL,
  crs_column = "crs_duckspatial",
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

- new_column:

  Name of the new column to create on the input data. Ignored with
  `mode = "sf"`.

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

- `mode = "duckspatial"` (default): A `duckspatial_df` (lazy spatial
  data frame) backed by dbplyr/DuckDB.

- `mode = "sf"`: An eagerly collected vector in R memory.

- When `name` is provided: writes the table in the DuckDB connection and
  returns `TRUE` (invisibly).

## Details

These functions check for additional coordinate dimensions beyond X and
Y:

- `ddbs_has_z()` checks if a geometry has Z coordinates
  (elevation/altitude values). Geometries with Z dimension are often
  referred to as 3D geometries and have coordinates in the form (X, Y,
  Z).

- `ddbs_has_m()` checks if a geometry has M coordinates (measure
  values). The M dimension typically represents a measurement along the
  geometry, such as distance or time, and results in coordinates of the
  form (X, Y, M) or (X, Y, Z, M).

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(dplyr)
library(duckspatial)

## create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
) |> 
  filter(ISO3_CODE != "ATA")

## check if it has Z or M
ddbs_has_m(countries_ddbs)
ddbs_has_z(countries_ddbs)
} # }
```

# Force geometry dimensions

Functions to force geometries to have specific coordinate dimensions (X,
Y, Z, M)

## Usage

``` r
ddbs_force_2d(
  x,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_force_3d(
  x,
  var,
  dim = "z",
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_force_4d(
  x,
  var_z,
  var_m,
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

- var:

  A numeric variable in `x` to be converted in the dimension specified
  in the argument `dim`

- dim:

  The dimension to add: either `"z"` (default) for elevation or `"m"`
  for measure values.

- var_z:

  A numeric variable in `x` to be convered in `Z` dimension

- var_m:

  A numeric variable in `x` to be convered in `M` dimension

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

## Details

These functions modify the dimensionality of geometries:

- `ddbs_force_2d()` removes Z and M coordinates from geometries,
  returning only X and Y coordinates. This is useful for simplifying 3D
  or measured geometries to 2D.

- `ddbs_force_3d()` forces geometries to have three dimensions. When
  `dim = "z"` (default), adds or retains Z coordinates (X, Y, Z). When
  `dim = "m"`, adds or retains M coordinates (X, Y, M). Missing values
  are typically set to 0. If the input geometry has a third dimension
  already, it will be replaced by the new one. If the input geometry has
  4 dimensions, it will drop the dimension that wasn't specified.

- `ddbs_force_4d()` forces geometries to have all four dimensions (X, Y,
  Z, M). Missing Z or M values are typically set to 0.

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(dplyr)
library(duckspatial)

## load data and add 2 numeric vars
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
) |> 
  dplyr::filter(ISO3_CODE != "ATA") |> 
  ddbs_area(new_column = "area") |> 
  ddbs_perimeter(new_column = "perim") 

## add a Z dimension
countries_z_ddbs <- ddbs_force_3d(countries_ddbs, "area")
ddbs_has_z(countries_z_ddbs)

## add a M dimension as 3D (removes current Z)
countries_m_ddbs <- ddbs_force_3d(countries_z_ddbs, "area", "m")
ddbs_has_z(countries_m_ddbs)
ddbs_has_m(countries_m_ddbs)

## add both Z and M
countries_zm_ddbs <- ddbs_force_4d(countries_ddbs, "area", "perim")
ddbs_has_z(countries_zm_ddbs)
ddbs_has_m(countries_zm_ddbs)

## drop both ZM
countries_drop_ddbs <- ddbs_force_2d(countries_zm_ddbs)
ddbs_has_z(countries_drop_ddbs)
ddbs_has_m(countries_drop_ddbs)
} # }
```

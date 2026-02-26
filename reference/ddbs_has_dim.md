# Check geometry dimensions

Functions to check whether geometries have Z (elevation) or M (measure)
dimensions

## Usage

``` r
ddbs_has_z(x, by_feature = FALSE, conn = NULL, quiet = FALSE)

ddbs_has_m(x, by_feature = FALSE, conn = NULL, quiet = FALSE)
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

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

A logical vector

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

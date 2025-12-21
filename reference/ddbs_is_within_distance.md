# Within Distance predicate

Tests if geometries in `x` are within a specified distance of `y`.
Returns `TRUE` if geometries are within the distance.

## Usage

``` r
ddbs_is_within_distance(
  x,
  y,
  distance = NULL,
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  quiet = FALSE
)
```

## Arguments

- x:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- y:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`.

- distance:

  a numeric value specifying the distance for ST_DWithin. Units
  correspond to the coordinate system of the geometry (e.g. degrees or
  meters)

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- id_x:

  Character; optional name of the column in `x` whose values will be
  used to name the list elements. If `NULL`, integer row numbers of `x`
  are used.

- id_y:

  Character; optional name of the column in `y` whose values will
  replace the integer indices returned in each element of the list.

- sparse:

  A logical value. If `TRUE`, it returns a sparse index list. If
  `FALSE`, it returns a dense logical matrix.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

A list where each element contains indices (or IDs) of geometries in `y`
that touch the corresponding geometry in `x`. See
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
for details.

## Details

This is a convenience wrapper around
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
with `predicate = "dwithin"`.

## See also

[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
for other spatial predicates.

## Examples

``` r
## load packages
library(dplyr)
library(duckspatial)
library(sf)

## read countries data, and rivers
countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial"))
countries_filter_sf <- countries_sf |> filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))

## check countries within 1 degree of distance
ddbs_is_within_distance(countries_filter_sf, countries_sf, 1)
#> ✔ Query successful
#> [[1]]
#> [1]   5  64  73  85 160 177
#> 
#> [[2]]
#>  [1]   5  19  60  64  71  79  85  90  91  96 173 178 206
#> 
#> [[3]]
#>  [1]   3  10  60  85  86  90  96 120 178 204 231 235
#> 
#> [[4]]
#> [1]  64 160
#> 
```

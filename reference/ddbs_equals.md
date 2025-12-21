# Spatial equals predicate

Tests if geometries in `x` are spatially equal to geometries in `y`.
Returns `TRUE` if geometries are topologically equivalent (same shape
and location).

## Usage

``` r
ddbs_equals(
  x,
  y,
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
that are equal to the corresponding geometry in `x`. See
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
for details.

## Details

This is a convenience wrapper around
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
with `predicate = "equals"`.

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
countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
  filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))

ddbs_equals(countries_sf, countries_sf, id_x = "NAME_ENGL")
#> ✔ Query successful
#> $Spain
#> [1] 1
#> 
#> $France
#> [1] 2
#> 
#> $Italy
#> [1] 3
#> 
#> $Portugal
#> [1] 4
#> 
```

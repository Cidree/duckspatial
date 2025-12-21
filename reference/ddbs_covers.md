# Spatial covers predicate

Tests if geometries in `x` cover geometries in `y`. Returns `TRUE` if
geometry `x` completely covers geometry `y` (no point of `y` lies
outside `x`).

## Usage

``` r
ddbs_covers(
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
that are covered by the corresponding geometry in `x`. See
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
for details.

## Details

This is a convenience wrapper around
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
with `predicate = "covers"`.

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
rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
  st_transform(st_crs(countries_sf))
#> Reading layer `rivers' from data source 
#>   `/home/runner/work/_temp/Library/duckspatial/spatial/rivers.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 100 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 2766878 ymin: 2222357 xmax: 3578648 ymax: 2459939
#> Projected CRS: ETRS89-extended / LAEA Europe

ddbs_covers(countries_sf, rivers_sf, id_x = "NAME_ENGL")
#> ✔ Query successful
#> $Spain
#>  [1]   1   3   5   6   8   9  10  11  12  13  14  15  16  17  19  21  22  23  24
#> [20]  25  26  27  28  30  31  32  33  34  36  37  39  40  41  42  43  44  45  46
#> [39]  47  48  49  50  51  52  53  54  55  56  57  58  60  61  62  63  64  65  66
#> [58]  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85
#> [77]  86  87  88  89  90  91  92  93  94  95  97  98  99 100
#> 
#> $France
#> integer(0)
#> 
#> $Italy
#> integer(0)
#> 
#> $Portugal
#> integer(0)
#> 
```

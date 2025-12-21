# Spatial covered by predicate

Tests if geometries in `x` are covered by geometries in `y`. Returns
`TRUE` if geometry `x` is completely covered by geometry `y` (no point
of `x` lies outside `y`).

## Usage

``` r
ddbs_covered_by(
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
that cover the corresponding geometry in `x`. See
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
for details.

## Details

This is a convenience wrapper around
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
with `predicate = "covered_by"`.

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

ddbs_covered_by(rivers_sf, countries_sf, id_x = "RIVER_NAME", id_y = "NAME_ENGL")
#> ✔ Query successful
#> $`Rio Garona`
#> [1] "Spain" "Italy"
#> 
#> $`Bidasoa Ibaia`
#> [1] "Spain"    "France"   "Portugal"
#> 
#> $`Baztan Ibaia`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Urumea`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Oria Ibaia`
#> [1] "Spain" "Italy"
#> 
#> $`Oria Ibaia`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Urola Ibaia`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Deba Ibaia`
#> [1] "France"   "Italy"    "Portugal"
#> 
#> $`Rio Nervion`
#> [1] "Spain"    "France"   "Portugal"
#> 
#> $`Ibaizabal Ibaia`
#> [1] "Spain"    "Italy"    "Portugal"
#> 
#> $`Rio Agüera O Mayor`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Ason`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Miera`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Pas`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Saja`
#> [1] "Spain"    "France"   "Portugal"
#> 
#> $`Rio Besaya`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Nansa`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Deva`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Cares`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Sella`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Piloña`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Nalon`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Narcea`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Pigüeña`
#> [1] "Spain"  "France" "Italy" 
#> 
#> $`Rio Nora`
#> [1] "Spain"    "France"   "Italy"    "Portugal"
#> 
#> $`Rio Trubia`
#> integer(0)
#> 
#> $`Rio Esva`
#> integer(0)
#> 
#> $`Rio Barcena`
#> integer(0)
#> 
#> $`Rio Navia`
#> integer(0)
#> 
#> $`Rio Navia`
#> integer(0)
#> 
#> $`Rio Navia`
#> integer(0)
#> 
#> $`Rio Navia`
#> integer(0)
#> 
#> $`Rio Navia`
#> integer(0)
#> 
#> $`Rio Agueira`
#> integer(0)
#> 
#> $`Rio Eo`
#> integer(0)
#> 
#> $`Rio Masma`
#> integer(0)
#> 
#> $`Rio Landro`
#> integer(0)
#> 
#> $`Rio Sor`
#> integer(0)
#> 
#> $`Rio Mera`
#> integer(0)
#> 
#> $`Rio Eume`
#> integer(0)
#> 
#> $`Rio Eume`
#> integer(0)
#> 
#> $`Rio Eume`
#> integer(0)
#> 
#> $`Rio Mandeo`
#> integer(0)
#> 
#> $`Rio Mero`
#> integer(0)
#> 
#> $`Rio Anllons`
#> integer(0)
#> 
#> $`Rio Xallas`
#> integer(0)
#> 
#> $`Rio Xallas`
#> integer(0)
#> 
#> $`Rio Xallas`
#> integer(0)
#> 
#> $`Rio Tambre`
#> integer(0)
#> 
#> $`Rio Ulla`
#> integer(0)
#> 
#> $`Rio Ulla`
#> integer(0)
#> 
#> $`Rio Ulla`
#> integer(0)
#> 
#> $`Rio Arnego`
#> integer(0)
#> 
#> $`Rio Arnego`
#> integer(0)
#> 
#> $`Rio Umia`
#> integer(0)
#> 
#> $`Rio Lerez`
#> integer(0)
#> 
#> $`Rio Verdugo`
#> integer(0)
#> 
#> $`Rio Miño`
#> integer(0)
#> 
#> $`Rio Miño`
#> integer(0)
#> 
#> $`Rio Miño`
#> integer(0)
#> 
#> $`Rio Miño`
#> integer(0)
#> 
#> $`Rio Miño`
#> integer(0)
#> 
#> $`Rio Miño`
#> integer(0)
#> 
#> $`Rio Miño`
#> integer(0)
#> 
#> $`Rio Miño`
#> integer(0)
#> 
#> $`Rio Tea`
#> integer(0)
#> 
#> $`Rio Arnoia`
#> integer(0)
#> 
#> $`Rio Avia`
#> integer(0)
#> 
#> $`Rio Avia`
#> integer(0)
#> 
#> $`Rio Avia`
#> integer(0)
#> 
#> $`Rio Sil`
#> integer(0)
#> 
#> $`Rio Sil`
#> integer(0)
#> 
#> $`Rio Sil`
#> integer(0)
#> 
#> $`Rio Sil`
#> integer(0)
#> 
#> $`Rio Sil`
#> integer(0)
#> 
#> $`Rio Cabe`
#> integer(0)
#> 
#> $`Rio Mao`
#> integer(0)
#> 
#> $`Rio Bibei`
#> integer(0)
#> 
#> $`Rio Bibei`
#> integer(0)
#> 
#> $`Rio Bibei`
#> integer(0)
#> 
#> $`Rio Xares`
#> integer(0)
#> 
#> $`Rio Xares`
#> integer(0)
#> 
#> $`Rio Xares`
#> integer(0)
#> 
#> $`Rio Conso`
#> integer(0)
#> 
#> $`Rio Conso`
#> integer(0)
#> 
#> $`Rio Camba`
#> integer(0)
#> 
#> $`Rio Camba`
#> integer(0)
#> 
#> $`Rio Camba`
#> integer(0)
#> 
#> $`Rio Camba`
#> integer(0)
#> 
#> $`Rio Selmo`
#> integer(0)
#> 
#> $`Rio Burbia`
#> integer(0)
#> 
#> $`Rio Boeza`
#> integer(0)
#> 
#> $`Rio Neira`
#> integer(0)
#> 
#> $`Rio Ladra`
#> integer(0)
#> 
#> $`Rio Parga`
#> integer(0)
#> 
#> $`Rio Limia`
#> integer(0)
#> 
#> $`Rio Limia`
#> integer(0)
#> 
#> $`Rio Limia`
#> integer(0)
#> 
#> $`Rio Limia`
#> integer(0)
#> 
#> $`Rio Salas`
#> integer(0)
#> 
```

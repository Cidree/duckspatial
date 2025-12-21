# Spatial contains properly predicate

Tests if geometries in `x` properly contain geometries in `y`. Returns
`TRUE` if geometry `y` is completely inside geometry `x` and does not
touch its boundary.

## Usage

``` r
ddbs_contains_properly(
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
that are properly contained by the corresponding geometry in `x`. See
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
for details.

## Details

This is a convenience wrapper around
[`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
with `predicate = "contains_properly"`.

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

ddbs_contains_properly(countries_sf, rivers_sf, id_x = "NAME_ENGL", id_y = "RIVER_NAME")
#> ✔ Query successful
#> $Spain
#>  [1] "Rio Garona"         "Baztan Ibaia"       "Oria Ibaia"        
#>  [4] "Oria Ibaia"         "Deba Ibaia"         "Rio Nervion"       
#>  [7] "Ibaizabal Ibaia"    "Rio Agüera O Mayor" "Rio Ason"          
#> [10] "Rio Miera"          "Rio Pas"            "Rio Saja"          
#> [13] "Rio Besaya"         "Rio Nansa"          "Rio Cares"         
#> [16] "Rio Piloña"         "Rio Nalon"          "Rio Narcea"        
#> [19] "Rio Pigüeña"        "Rio Nora"           "Rio Trubia"        
#> [22] "Rio Esva"           "Rio Barcena"        "Rio Navia"         
#> [25] "Rio Navia"          "Rio Navia"          "Rio Navia"         
#> [28] "Rio Agueira"        "Rio Masma"          "Rio Landro"        
#> [31] "Rio Mera"           "Rio Eume"           "Rio Eume"          
#> [34] "Rio Eume"           "Rio Mandeo"         "Rio Mero"          
#> [37] "Rio Anllons"        "Rio Xallas"         "Rio Xallas"        
#> [40] "Rio Xallas"         "Rio Tambre"         "Rio Ulla"          
#> [43] "Rio Ulla"           "Rio Ulla"           "Rio Arnego"        
#> [46] "Rio Arnego"         "Rio Umia"           "Rio Lerez"         
#> [49] "Rio Verdugo"        "Rio Miño"           "Rio Miño"          
#> [52] "Rio Miño"           "Rio Miño"           "Rio Miño"          
#> [55] "Rio Miño"           "Rio Miño"           "Rio Tea"           
#> [58] "Rio Arnoia"         "Rio Avia"           "Rio Avia"          
#> [61] "Rio Avia"           "Rio Sil"            "Rio Sil"           
#> [64] "Rio Sil"            "Rio Sil"            "Rio Sil"           
#> [67] "Rio Cabe"           "Rio Mao"            "Rio Bibei"         
#> [70] "Rio Bibei"          "Rio Bibei"          "Rio Xares"         
#> [73] "Rio Xares"          "Rio Xares"          "Rio Conso"         
#> [76] "Rio Conso"          "Rio Camba"          "Rio Camba"         
#> [79] "Rio Camba"          "Rio Camba"          "Rio Selmo"         
#> [82] "Rio Burbia"         "Rio Boeza"          "Rio Neira"         
#> [85] "Rio Ladra"          "Rio Parga"          "Rio Limia"         
#> [88] "Rio Limia"          "Rio Limia"          "Rio Salas"         
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

# Spatial predicate operations

Computes spatial relationships between two geometry datasets using
DuckDB's spatial extension. Returns a list where each element
corresponds to a row of `x`, containing the indices (or IDs) of rows in
`y` that satisfy the specified spatial predicate.

## Usage

``` r
ddbs_predicate(
  x,
  y,
  predicate = "intersects",
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  distance = NULL,
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
  `conn`. Data is returned from this object.

- predicate:

  A geometry predicate function. Defaults to `intersects`, a wrapper of
  `ST_Intersects`. See details for other options.

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

- distance:

  a numeric value specifying the distance for ST_DWithin. Units
  correspond to the coordinate system of the geometry (e.g. degrees or
  meters)

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

A **list** of length equal to the number of rows in `x`.

- Each element contains:

  - **integer vector** of row indices of `y` that satisfy the predicate
    with the corresponding geometry of `x`, or

  - **character vector** if `id_y` is supplied.

- The names of the list elements:

  - are integer row numbers of `x`, or

  - the values of `id_x` if provided.

If there's no match between `x` and `y` it returns `NULL`

## Details

This function provides a unified interface to all spatial predicate
operations in DuckDB's spatial extension. It performs pairwise
comparisons between all geometries in `x` and `y` using the specified
predicate.

### Available Predicates

- **intersects**: Geometries share at least one point

- **covers**: Geometry `x` completely covers geometry `y`

- **touches**: Geometries share a boundary but interiors do not
  intersect

- **disjoint**: Geometries have no points in common

- **within**: Geometry `x` is completely inside geometry `y`

- **dwithin**: Geometry `x` is completely within a distance of geometry
  `y`

- **contains**: Geometry `x` completely contains geometry `y`

- **overlaps**: Geometries share some but not all points

- **crosses**: Geometries have some interior points in common

- **equals**: Geometries are spatially equal

- **covered_by**: Geometry `x` is completely covered by geometry `y`

- **intersects_extent**: Bounding boxes of geometries intersect (faster
  but less precise)

- **contains_properly**: Geometry `x` contains geometry `y` without
  boundary contact

- **within_properly**: Geometry `x` is within geometry `y` without
  boundary contact

If `x` or `y` are not DuckDB tables, they are automatically copied into
a temporary in-memory DuckDB database (unless a connection is supplied
via `conn`).

`id_x` or `id_y` may be used to replace the default integer indices with
the values of an identifier column in `x` or `y`, respectively.

## Examples

``` r
## Load packages
library(duckspatial)
library(dplyr)
library(sf)

## create in-memory DuckDB database
conn <- ddbs_create_conn(dbdir = "memory")

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

## Store in DuckDB
ddbs_write_vector(conn, countries_sf, "countries")
#> ✔ Table countries successfully imported
ddbs_write_vector(conn, rivers_sf, "rivers")
#> ✔ Table rivers successfully imported

## Example 1: Check which rivers intersect each country
ddbs_predicate(countries_sf, rivers_sf, predicate = "intersects", conn)
#> ✔ Query successful
#> [[1]]
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> integer(0)
#> 
#> [[4]]
#> [1] 59 96
#> 

## Example 2: Find neighboring countries
ddbs_predicate(countries_sf, countries_sf, predicate = "touches",
               id_x = "NAME_ENGL", id_y = "NAME_ENGL")
#> ✔ Query successful
#> $Spain
#> [1] "France"   "Portugal"
#> 
#> $France
#> [1] "Spain" "Italy"
#> 
#> $Italy
#> [1] "France"
#> 
#> $Portugal
#> [1] "Spain"
#> 

## Example 3: Find rivers that don't intersect countries
ddbs_predicate(countries_sf, rivers_sf, predicate = "disjoint",
               id_x = "NAME_ENGL", id_y = "RIVER_NAME")
#> ✔ Query successful
#> $Spain
#> integer(0)
#> 
#> $France
#>  [1] "Rio Garona"         "Baztan Ibaia"       "Rio Urumea"        
#>  [4] "Oria Ibaia"         "Oria Ibaia"         "Urola Ibaia"       
#>  [7] "Deba Ibaia"         "Rio Nervion"        "Ibaizabal Ibaia"   
#> [10] "Rio Agüera O Mayor" "Rio Ason"           "Rio Miera"         
#> [13] "Rio Pas"            "Rio Saja"           "Rio Besaya"        
#> [16] "Rio Nansa"          "Rio Deva"           "Rio Cares"         
#> [19] "Rio Sella"          "Rio Piloña"         "Rio Nalon"         
#> [22] "Rio Narcea"         "Rio Pigüeña"        "Rio Nora"          
#> [25] "Rio Trubia"         "Rio Esva"           "Rio Barcena"       
#> [28] "Rio Navia"          "Rio Navia"          "Rio Navia"         
#> [31] "Rio Navia"          "Rio Navia"          "Rio Agueira"       
#> [34] "Rio Eo"             "Rio Masma"          "Rio Landro"        
#> [37] "Rio Sor"            "Rio Mera"           "Rio Eume"          
#> [40] "Rio Eume"           "Rio Eume"           "Rio Mandeo"        
#> [43] "Rio Mero"           "Rio Anllons"        "Rio Xallas"        
#> [46] "Rio Xallas"         "Rio Xallas"         "Rio Tambre"        
#> [49] "Rio Ulla"           "Rio Ulla"           "Rio Ulla"          
#> [52] "Rio Arnego"         "Rio Arnego"         "Rio Umia"          
#> [55] "Rio Lerez"          "Rio Verdugo"        "Rio Miño"          
#> [58] "Rio Miño"           "Rio Miño"           "Rio Miño"          
#> [61] "Rio Miño"           "Rio Miño"           "Rio Miño"          
#> [64] "Rio Miño"           "Rio Tea"            "Rio Arnoia"        
#> [67] "Rio Avia"           "Rio Avia"           "Rio Avia"          
#> [70] "Rio Sil"            "Rio Sil"            "Rio Sil"           
#> [73] "Rio Sil"            "Rio Sil"            "Rio Cabe"          
#> [76] "Rio Mao"            "Rio Bibei"          "Rio Bibei"         
#> [79] "Rio Bibei"          "Rio Xares"          "Rio Xares"         
#> [82] "Rio Xares"          "Rio Conso"          "Rio Conso"         
#> [85] "Rio Camba"          "Rio Camba"          "Rio Camba"         
#> [88] "Rio Camba"          "Rio Selmo"          "Rio Burbia"        
#> [91] "Rio Boeza"          "Rio Neira"          "Rio Ladra"         
#> [94] "Rio Parga"          "Rio Limia"          "Rio Limia"         
#> [97] "Rio Limia"          "Rio Limia"          "Rio Salas"         
#> 
#> $Italy
#>   [1] "Rio Garona"         "Bidasoa Ibaia"      "Baztan Ibaia"      
#>   [4] "Rio Urumea"         "Oria Ibaia"         "Oria Ibaia"        
#>   [7] "Urola Ibaia"        "Deba Ibaia"         "Rio Nervion"       
#>  [10] "Ibaizabal Ibaia"    "Rio Agüera O Mayor" "Rio Ason"          
#>  [13] "Rio Miera"          "Rio Pas"            "Rio Saja"          
#>  [16] "Rio Besaya"         "Rio Nansa"          "Rio Deva"          
#>  [19] "Rio Cares"          "Rio Sella"          "Rio Piloña"        
#>  [22] "Rio Nalon"          "Rio Narcea"         "Rio Pigüeña"       
#>  [25] "Rio Nora"           "Rio Trubia"         "Rio Esva"          
#>  [28] "Rio Barcena"        "Rio Navia"          "Rio Navia"         
#>  [31] "Rio Navia"          "Rio Navia"          "Rio Navia"         
#>  [34] "Rio Agueira"        "Rio Eo"             "Rio Masma"         
#>  [37] "Rio Landro"         "Rio Sor"            "Rio Mera"          
#>  [40] "Rio Eume"           "Rio Eume"           "Rio Eume"          
#>  [43] "Rio Mandeo"         "Rio Mero"           "Rio Anllons"       
#>  [46] "Rio Xallas"         "Rio Xallas"         "Rio Xallas"        
#>  [49] "Rio Tambre"         "Rio Ulla"           "Rio Ulla"          
#>  [52] "Rio Ulla"           "Rio Arnego"         "Rio Arnego"        
#>  [55] "Rio Umia"           "Rio Lerez"          "Rio Verdugo"       
#>  [58] "Rio Miño"           "Rio Miño"           "Rio Miño"          
#>  [61] "Rio Miño"           "Rio Miño"           "Rio Miño"          
#>  [64] "Rio Miño"           "Rio Miño"           "Rio Tea"           
#>  [67] "Rio Arnoia"         "Rio Avia"           "Rio Avia"          
#>  [70] "Rio Avia"           "Rio Sil"            "Rio Sil"           
#>  [73] "Rio Sil"            "Rio Sil"            "Rio Sil"           
#>  [76] "Rio Cabe"           "Rio Mao"            "Rio Bibei"         
#>  [79] "Rio Bibei"          "Rio Bibei"          "Rio Xares"         
#>  [82] "Rio Xares"          "Rio Xares"          "Rio Conso"         
#>  [85] "Rio Conso"          "Rio Camba"          "Rio Camba"         
#>  [88] "Rio Camba"          "Rio Camba"          "Rio Selmo"         
#>  [91] "Rio Burbia"         "Rio Boeza"          "Rio Neira"         
#>  [94] "Rio Ladra"          "Rio Parga"          "Rio Limia"         
#>  [97] "Rio Limia"          "Rio Limia"          "Rio Limia"         
#> [100] "Rio Salas"         
#> 
#> $Portugal
#>  [1] "Rio Garona"         "Bidasoa Ibaia"      "Baztan Ibaia"      
#>  [4] "Rio Urumea"         "Oria Ibaia"         "Oria Ibaia"        
#>  [7] "Urola Ibaia"        "Deba Ibaia"         "Rio Nervion"       
#> [10] "Ibaizabal Ibaia"    "Rio Agüera O Mayor" "Rio Ason"          
#> [13] "Rio Miera"          "Rio Pas"            "Rio Saja"          
#> [16] "Rio Besaya"         "Rio Nansa"          "Rio Deva"          
#> [19] "Rio Cares"          "Rio Sella"          "Rio Piloña"        
#> [22] "Rio Nalon"          "Rio Narcea"         "Rio Pigüeña"       
#> [25] "Rio Nora"           "Rio Trubia"         "Rio Esva"          
#> [28] "Rio Barcena"        "Rio Navia"          "Rio Navia"         
#> [31] "Rio Navia"          "Rio Navia"          "Rio Navia"         
#> [34] "Rio Agueira"        "Rio Eo"             "Rio Masma"         
#> [37] "Rio Landro"         "Rio Sor"            "Rio Mera"          
#> [40] "Rio Eume"           "Rio Eume"           "Rio Eume"          
#> [43] "Rio Mandeo"         "Rio Mero"           "Rio Anllons"       
#> [46] "Rio Xallas"         "Rio Xallas"         "Rio Xallas"        
#> [49] "Rio Tambre"         "Rio Ulla"           "Rio Ulla"          
#> [52] "Rio Ulla"           "Rio Arnego"         "Rio Arnego"        
#> [55] "Rio Umia"           "Rio Lerez"          "Rio Verdugo"       
#> [58] "Rio Miño"           "Rio Miño"           "Rio Miño"          
#> [61] "Rio Miño"           "Rio Miño"           "Rio Miño"          
#> [64] "Rio Miño"           "Rio Tea"            "Rio Arnoia"        
#> [67] "Rio Avia"           "Rio Avia"           "Rio Avia"          
#> [70] "Rio Sil"            "Rio Sil"            "Rio Sil"           
#> [73] "Rio Sil"            "Rio Sil"            "Rio Cabe"          
#> [76] "Rio Mao"            "Rio Bibei"          "Rio Bibei"         
#> [79] "Rio Bibei"          "Rio Xares"          "Rio Xares"         
#> [82] "Rio Xares"          "Rio Conso"          "Rio Conso"         
#> [85] "Rio Camba"          "Rio Camba"          "Rio Camba"         
#> [88] "Rio Camba"          "Rio Selmo"          "Rio Burbia"        
#> [91] "Rio Boeza"          "Rio Neira"          "Rio Ladra"         
#> [94] "Rio Parga"          "Rio Limia"          "Rio Limia"         
#> [97] "Rio Limia"          "Rio Salas"         
#> 

## Example 4: Use table names inside duckdb
ddbs_predicate("countries", "rivers", predicate = "within", conn, "NAME_ENGL")
#> ✔ Query successful
#> $Spain
#> integer(0)
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

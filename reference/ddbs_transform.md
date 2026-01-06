# Transform coordinate reference system of geometries

Transforms geometries from a DuckDB table to a different coordinate
reference system using the spatial extension. Works similarly to
[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html).
Returns the result as an `sf` object or creates a new table in the
database.

## Usage

``` r
ddbs_transform(
  x,
  y,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- y:

  Target CRS. Can be:

  - A character string with EPSG code (e.g., "EPSG:4326")

  - An `sf` object (uses its CRS)

  - Name of a DuckDB table (uses its CRS)

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- crs:

  The coordinates reference system of the data. Specify if the data
  doesn't have a `crs_column`, and you know the CRS.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

an `sf` object or `TRUE` (invisibly) for table creation

## Examples

``` r
## load packages
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
#> Reading layer `argentina' from data source 
#>   `/home/runner/work/_temp/Library/duckspatial/spatial/argentina.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 1 feature and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -73.52455 ymin: -52.39755 xmax: -53.62409 ymax: -21.81793
#> Geodetic CRS:  WGS 84

## store in duckdb
ddbs_write_vector(conn, argentina_sf, "argentina")
#> ✔ Table argentina successfully imported

## transform to different CRS using EPSG code
ddbs_transform("argentina", "EPSG:3857", conn)
#> ✔ Query successful
#> Simple feature collection with 1 feature and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5832869 ymin: -12326250 xmax: -2428761 ymax: -7099282
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   CNTR_ID NAME_ENGL ISO3_CODE CNTR_NAME FID       date
#> 1      AR Argentina       ARG Argentina  AR 2021-01-01
#>                         geometry
#> 1 POLYGON ((-2476911 -9013777...

## transform to match CRS of another sf object
argentina_3857_sf <- st_transform(argentina_sf, "EPSG:3857")
ddbs_write_vector(conn, argentina_3857_sf, "argentina_3857")
#> ✔ Table argentina_3857 successfully imported
ddbs_transform("argentina", argentina_3857_sf, conn)
#> ✔ Query successful
#> Simple feature collection with 1 feature and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5832869 ymin: -12326250 xmax: -2428761 ymax: -7099282
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   CNTR_ID NAME_ENGL ISO3_CODE CNTR_NAME FID       date
#> 1      AR Argentina       ARG Argentina  AR 2021-01-01
#>                         geometry
#> 1 POLYGON ((-2476911 -9013777...

## transform to match CRS of another DuckDB table
ddbs_transform("argentina", "argentina_3857", conn)
#> ✔ Query successful
#> Simple feature collection with 1 feature and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5832869 ymin: -12326250 xmax: -2428761 ymax: -7099282
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   CNTR_ID NAME_ENGL ISO3_CODE CNTR_NAME FID       date
#> 1      AR Argentina       ARG Argentina  AR 2021-01-01
#>                         geometry
#> 1 POLYGON ((-2476911 -9013777...

## transform without using a connection
ddbs_transform(argentina_sf, "EPSG:3857")
#> ✔ Query successful
#> Simple feature collection with 1 feature and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -5832869 ymin: -12326250 xmax: -2428761 ymax: -7099282
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   CNTR_ID NAME_ENGL ISO3_CODE CNTR_NAME FID       date
#> 1      AR Argentina       ARG Argentina  AR 2021-01-01
#>                         geometry
#> 1 POLYGON ((-2476911 -9013777...
```

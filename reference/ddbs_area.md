# Calculates the area of geometries

Calculates the area of geometries from a DuckDB table or a `sf` object
Returns the result as an `sf` object with an area column or creates a
new table in the database. Note: Area units depend on the CRS of the
input geometries (e.g., square meters for projected CRS, or degrees for
geographic CRS).

## Usage

``` r
ddbs_area(
  x,
  conn = NULL,
  name = NULL,
  new_column = NULL,
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

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- new_column:

  Name of the new column to create on the input data. If NULL, the
  function will return a vector with the result

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

a vector, an `sf` object or `TRUE` (invisibly) for table creation

## Examples

``` r
## load packages
library(duckspatial)
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial")) |>
    st_transform("EPSG:3857")
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

## calculate area (returns sf object with area column)
ddbs_area("argentina", conn)
#> [1] 4.253708e+12

## calculate area with custom column name
ddbs_area("argentina", conn, new_column = "area_sqm")
#> ✔ Query successful
#> Simple feature collection with 1 feature and 7 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -8184715 ymin: -6872329 xmax: -5969406 ymax: -2489680
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   CNTR_ID NAME_ENGL ISO3_CODE CNTR_NAME FID       date     area_sqm
#> 1      AR Argentina       ARG Argentina  AR 2021-01-01 4.253708e+12
#>                         geometry
#> 1 POLYGON ((-6973632 -2541624...

## create a new table with area calculations
ddbs_area("argentina", conn, name = "argentina_with_area")
#> [1] 4.253708e+12

## calculate area in a sf object
ddbs_area(argentina_sf)
#> [1] 4.253708e+12
```

# Generate random points within geometries

Generates random points within geometries from a DuckDB table using the
spatial extension. Works similarly to generating random points within
polygons in `sf`. Returns the result as an `sf` object or creates a new
table in the database.

## Usage

``` r
ddbs_generate_points(
  x,
  n,
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

- n:

  Number of random points to generate within each geometry

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

## generate 100 random points within each geometry
ddbs_generate_points("argentina", n = 100, conn)
#> ✔ Query successful
#> Simple feature collection with 100 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -73.50851 ymin: -52.03629 xmax: -53.72035 ymax: -22.21588
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                       geometry
#> 1  POINT (-68.08173 -47.21398)
#> 2  POINT (-56.02919 -35.94749)
#> 3  POINT (-62.15017 -26.63512)
#> 4  POINT (-65.35472 -23.31202)
#> 5  POINT (-61.71041 -47.10382)
#> 6   POINT (-55.11455 -29.3518)
#> 7  POINT (-60.23173 -33.47816)
#> 8  POINT (-68.24235 -39.00622)
#> 9  POINT (-68.65901 -26.42233)
#> 10 POINT (-63.19506 -47.75552)

## generate points without using a connection
ddbs_generate_points(argentina_sf, n = 100)
#> ✔ Query successful
#> Simple feature collection with 100 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -73.38563 ymin: -52.21347 xmax: -53.77032 ymax: -21.90505
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                       geometry
#> 1  POINT (-60.87414 -43.39879)
#> 2  POINT (-54.45109 -44.33853)
#> 3  POINT (-60.73822 -33.69727)
#> 4  POINT (-73.38563 -29.05542)
#> 5    POINT (-57.1724 -26.0192)
#> 6  POINT (-60.70053 -32.12977)
#> 7  POINT (-73.19363 -26.62783)
#> 8   POINT (-55.26723 -30.5626)
#> 9  POINT (-62.04115 -36.52936)
#> 10  POINT (-65.61947 -42.8571)
```

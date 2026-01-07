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
#> Bounding box:  xmin: -73.44218 ymin: -52.23984 xmax: -53.80854 ymax: -21.8676
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                       geometry
#> 1  POINT (-71.11505 -50.66566)
#> 2  POINT (-57.80266 -42.06965)
#> 3  POINT (-54.81055 -49.75124)
#> 4  POINT (-71.99969 -35.04964)
#> 5  POINT (-64.16614 -47.79881)
#> 6  POINT (-64.49384 -47.12553)
#> 7  POINT (-65.61573 -44.22061)
#> 8   POINT (-61.9153 -45.02629)
#> 9  POINT (-58.09611 -48.14884)
#> 10 POINT (-71.78374 -31.08563)

## generate points without using a connection
ddbs_generate_points(argentina_sf, n = 100)
#> ✔ Query successful
#> Simple feature collection with 100 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -73.30153 ymin: -52.37327 xmax: -54.26202 ymax: -22.48149
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                       geometry
#> 1  POINT (-67.96577 -38.81274)
#> 2  POINT (-63.40914 -31.16604)
#> 3  POINT (-56.77943 -41.48607)
#> 4   POINT (-65.22999 -23.2638)
#> 5  POINT (-57.47277 -40.36704)
#> 6  POINT (-56.83957 -47.63614)
#> 7   POINT (-60.6504 -43.35117)
#> 8  POINT (-54.47397 -26.97309)
#> 9   POINT (-71.12737 -44.1492)
#> 10 POINT (-60.64577 -29.61054)
```

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
#> Bounding box:  xmin: -73.13879 ymin: -52.30112 xmax: -53.75143 ymax: -22.13748
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                       geometry
#> 1  POINT (-70.45694 -28.46132)
#> 2   POINT (-71.2339 -42.78054)
#> 3   POINT (-66.54101 -23.8763)
#> 4  POINT (-67.12546 -39.60996)
#> 5  POINT (-63.47584 -22.13748)
#> 6  POINT (-67.46186 -45.82993)
#> 7   POINT (-58.0486 -45.41153)
#> 8  POINT (-70.53694 -43.39914)
#> 9  POINT (-63.74552 -33.27252)
#> 10   POINT (-54.20293 -43.708)

## generate points without using a connection
ddbs_generate_points(argentina_sf, n = 100)
#> ✔ Query successful
#> Simple feature collection with 100 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -73.51863 ymin: -51.16846 xmax: -53.72028 ymax: -22.18924
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                       geometry
#> 1  POINT (-71.28276 -32.71302)
#> 2  POINT (-64.77208 -25.99689)
#> 3  POINT (-63.34467 -30.63944)
#> 4  POINT (-54.80762 -38.41811)
#> 5  POINT (-63.62812 -42.57719)
#> 6   POINT (-59.90001 -27.3901)
#> 7  POINT (-69.04288 -30.54965)
#> 8  POINT (-72.69339 -39.50286)
#> 9  POINT (-60.79873 -22.18924)
#> 10 POINT (-70.82081 -48.29778)
```

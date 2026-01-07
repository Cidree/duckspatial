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
#> Bounding box:  xmin: -73.17525 ymin: -51.94103 xmax: -53.63621 ymax: -22.21769
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                       geometry
#> 1  POINT (-67.55918 -37.55614)
#> 2  POINT (-67.95443 -23.70558)
#> 3  POINT (-59.51957 -33.75779)
#> 4  POINT (-70.19719 -27.20299)
#> 5  POINT (-54.92967 -30.08589)
#> 6  POINT (-62.46567 -39.09313)
#> 7  POINT (-65.40091 -23.37483)
#> 8   POINT (-64.36579 -25.5467)
#> 9  POINT (-55.16624 -44.01768)
#> 10 POINT (-64.33896 -31.28683)

## generate points without using a connection
ddbs_generate_points(argentina_sf, n = 100)
#> ✔ Query successful
#> Simple feature collection with 100 features and 0 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -73.32007 ymin: -52.13122 xmax: -53.7359 ymax: -21.8637
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                       geometry
#> 1  POINT (-57.14945 -40.06358)
#> 2  POINT (-60.66123 -22.45267)
#> 3  POINT (-72.49163 -30.40944)
#> 4  POINT (-62.03211 -48.05127)
#> 5  POINT (-68.55939 -45.73208)
#> 6   POINT (-58.6759 -48.21793)
#> 7  POINT (-55.42315 -36.50397)
#> 8  POINT (-59.60544 -30.98027)
#> 9  POINT (-56.59802 -47.37473)
#> 10 POINT (-71.70211 -33.84806)
```

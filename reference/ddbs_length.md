# Calculates the length of geometries

Calculates the length of geometries from a DuckDB table or a `sf` object
Returns the result as an `sf` object with a length column or creates a
new table in the database. Note: Length units depend on the CRS of the
input geometries (e.g., meters for projected CRS, or degrees for
geographic CRS).

## Usage

``` r
ddbs_length(
  x,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  length_column = "length",
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

- crs:

  The coordinates reference system of the data. Specify if the data
  doesn't have a `crs_column`, and you know the CRS.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- length_column:

  Character string specifying the name of the output length column.
  Default is "length".

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignore when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

an `sf` object or `TRUE` (invisibly) for table creation

## Examples

``` r
## load packages
library(duckdb)
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))
#> Reading layer `rivers' from data source 
#>   `/home/runner/work/_temp/Library/duckspatial/spatial/rivers.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 100 features and 1 field
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 2766878 ymin: 2222357 xmax: 3578648 ymax: 2459939
#> Projected CRS: ETRS89-extended / LAEA Europe

## store in duckdb
ddbs_write_vector(conn, rivers_sf, "rivers")
#> ✔ Table rivers successfully imported

## calculate length (returns sf object with length column)
ddbs_length("rivers", conn)
#> ✔ Query successful
#> Simple feature collection with 100 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 2766878 ymin: 2222357 xmax: 3578648 ymax: 2459939
#> Projected CRS: ETRS89-extended / LAEA Europe
#> First 10 features:
#>         RIVER_NAME   length                       geometry
#> 1       Rio Garona 34232.52 LINESTRING (3563589 2240292...
#> 2    Bidasoa Ibaia 29223.36 LINESTRING (3373540 2299136...
#> 3     Baztan Ibaia 22409.65 LINESTRING (3373540 2299136...
#> 4       Rio Urumea 27511.07 LINESTRING (3359001 2301989...
#> 5       Oria Ibaia 14817.08 LINESTRING (3346144 2312252...
#> 6       Oria Ibaia 40245.75 LINESTRING (3346144 2312252...
#> 7      Urola Ibaia 37421.60 LINESTRING (3317563 2294789...
#> 8       Deba Ibaia 38787.44 LINESTRING (3300422 2296792...
#> 9      Rio Nervion 48753.19 LINESTRING (3263336 2300537...
#> 10 Ibaizabal Ibaia 28956.85 LINESTRING (3299820 2310528...

## calculate length with custom column name
ddbs_length("rivers", conn, length_column = "length_meters")
#> ✔ Query successful
#> Simple feature collection with 100 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 2766878 ymin: 2222357 xmax: 3578648 ymax: 2459939
#> Projected CRS: ETRS89-extended / LAEA Europe
#> First 10 features:
#>         RIVER_NAME length_meters                       geometry
#> 1       Rio Garona      34232.52 LINESTRING (3563589 2240292...
#> 2    Bidasoa Ibaia      29223.36 LINESTRING (3373540 2299136...
#> 3     Baztan Ibaia      22409.65 LINESTRING (3373540 2299136...
#> 4       Rio Urumea      27511.07 LINESTRING (3359001 2301989...
#> 5       Oria Ibaia      14817.08 LINESTRING (3346144 2312252...
#> 6       Oria Ibaia      40245.75 LINESTRING (3346144 2312252...
#> 7      Urola Ibaia      37421.60 LINESTRING (3317563 2294789...
#> 8       Deba Ibaia      38787.44 LINESTRING (3300422 2296792...
#> 9      Rio Nervion      48753.19 LINESTRING (3263336 2300537...
#> 10 Ibaizabal Ibaia      28956.85 LINESTRING (3299820 2310528...

## create a new table with length calculations
ddbs_length("rivers", conn, name = "rivers_with_length")
#> ✔ Query successful

## calculate length in a sf object (without a connection)
ddbs_length(rivers_sf)
#> ✔ Query successful
#> Simple feature collection with 100 features and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 2766878 ymin: 2222357 xmax: 3578648 ymax: 2459939
#> Projected CRS: ETRS89-extended / LAEA Europe
#> First 10 features:
#>         RIVER_NAME   length                       geometry
#> 1       Rio Garona 34232.52 LINESTRING (3563589 2240292...
#> 2    Bidasoa Ibaia 29223.36 LINESTRING (3373540 2299136...
#> 3     Baztan Ibaia 22409.65 LINESTRING (3373540 2299136...
#> 4       Rio Urumea 27511.07 LINESTRING (3359001 2301989...
#> 5       Oria Ibaia 14817.08 LINESTRING (3346144 2312252...
#> 6       Oria Ibaia 40245.75 LINESTRING (3346144 2312252...
#> 7      Urola Ibaia 37421.60 LINESTRING (3317563 2294789...
#> 8       Deba Ibaia 38787.44 LINESTRING (3300422 2296792...
#> 9      Rio Nervion 48753.19 LINESTRING (3263336 2300537...
#> 10 Ibaizabal Ibaia 28956.85 LINESTRING (3299820 2310528...
```

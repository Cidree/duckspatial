# Returns the minimal bounding box enclosing the input geometry

Returns the minimal bounding box enclosing the input geometry from a
`sf` object or a DuckDB table. Returns the result as an `sf` object or
creates a new table in the database.

## Usage

``` r
ddbs_bbox(
  x,
  by_feature = FALSE,
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

- by_feature:

  Boolean. The function defaults to `FALSE`, and returns a single
  bounding box for `x`. If `TRUE`, it return one bounding box for each
  feature.

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

# option 1: passing sf objects
ddbs_bbox(argentina_sf)
#> ✔ Query successful
#>       min_x     min_y     max_x     max_y
#> 1 -73.52455 -52.39755 -53.62409 -21.81793


## option 2: passing the names of tables in a duckdb db

# creates a duckdb write sf to it
conn <- duckspatial::ddbs_create_conn()
ddbs_write_vector(conn, argentina_sf, "argentina_tbl", overwrite = TRUE)
#> ℹ Table <argentina_tbl> dropped
#> ✔ Table argentina_tbl successfully imported

output2 <- ddbs_bbox(
    conn = conn,
    x = "argentina_tbl",
    name = "argentina_bbox"
)
#> ✔ Query successful

DBI::dbReadTable(conn, "argentina_bbox")
#>       min_x     min_y     max_x     max_y
#> 1 -73.52455 -52.39755 -53.62409 -21.81793
```

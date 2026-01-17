# Convert geometries to QuadKey tiles

Converts POINT geometries to QuadKey tile representations at a specified
zoom level. QuadKeys are a hierarchical spatial indexing system used by
mapping services like Bing Maps.

## Usage

``` r
ddbs_quadkey(
  x,
  level = 10,
  output = "polygon",
  field = NULL,
  fun = "mean",
  background = NA,
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

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

  Data is returned from this object.

- level:

  An integer specifying the zoom level for QuadKey generation (1-23).
  Higher values provide finer spatial resolution. Default is 10.

- output:

  Character string specifying output format. One of:

  - `"polygon"` - Returns QuadKey tile boundaries as polygons (default)

  - `"raster"` - Returns QuadKey values as a raster grid

  - `"tilexy"` - Returns tile XY coordinates

- field:

  Character string specifying the field name for raster output. Only
  used when `output = "raster"`

- fun:

  summarizing function for when there are multiple geometries in one
  cell (e.g. "mean", "min", "max", "sum"). Only used when
  `output = "raster"`

- background:

  numeric. Default value in raster cells without values. Only used when
  `output = "raster"`

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- crs:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) The coordinates
  reference system of the data. Specify if the data doesn't have a
  `crs_column`, and you know the CRS.

- crs_column:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) a character
  string of length one specifying the column storing the CRS (created
  automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

An sf object or TRUE (invisibly) for table creation

## Details

QuadKeys divide the world into a hierarchical grid of tiles, where each
tile is subdivided into four smaller tiles at the next zoom level. This
function wraps DuckDB's ST_QuadKey spatial function to generate these
tiles from input geometries.

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)
library(sf)
library(terra)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## create random points in Argentina
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
rand_sf <- st_sample(argentina_sf, 100) |> st_as_sf()
rand_sf["var"] <- runif(100)

## store in duckdb
ddbs_write_vector(conn, rand_sf, "rand_sf")

## generate QuadKey polygons at zoom level 8
qkey_sf <- ddbs_quadkey(conn = conn, "rand_sf", level = 8, output = "polygon")

## generate QuadKey raster with custom field name
qkey_rast <- ddbs_quadkey(conn = conn, "rand_sf", level = 6, output = "raster", field = "var")

## generate Quadkey XY tiles
qkey_tiles_tbl <- ddbs_quadkey(conn = conn, "rand_sf", level = 10, output = "tilexy")
} # }
```

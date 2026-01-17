# Performs spatial filter of two geometries

Filters data spatially based on a spatial predicate

## Usage

``` r
ddbs_filter(
  x,
  y,
  predicate = "intersects",
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  distance = NULL,
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

  Y table with geometry column within the DuckDB database

- predicate:

  A geometry predicate function. Defaults to `intersects`, a wrapper of
  `ST_Intersects`. See details for other options.

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

- distance:

  a numeric value specifying the distance for ST_DWithin. Units
  correspond to the coordinate system of the geometry (e.g. degrees or
  meters)

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

An sf object or TRUE (invisibly) for table creation

## Details

Spatial Join Predicates:

A spatial predicate is really just a function that evaluates some
spatial relation between two geometries and returns true or false, e.g.,
“does a contain b” or “is a within distance x of b”. Here is a quick
overview of the most commonly used ones, taking two geometries a and b:

- `"ST_Intersects"`: Whether a intersects b

- `"ST_Contains"`: Whether a contains b

- `"ST_ContainsProperly"`: Whether a contains b without b touching a's
  boundary

- `"ST_Within"`: Whether a is within b

- `"ST_Overlaps"`: Whether a overlaps b

- `"ST_Touches"`: Whether a touches b

- `"ST_Equals"`: Whether a is equal to b

- `"ST_Crosses"`: Whether a crosses b

- `"ST_Covers"`: Whether a covers b

- `"ST_CoveredBy"`: Whether a is covered by b

- `"ST_DWithin"`: x) Whether a is within distance x of b

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
countries_sf <- st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))

## store in duckdb
ddbs_write_vector(conn, countries_sf, "countries")
ddbs_write_vector(conn, argentina_sf, "argentina")

## filter countries touching argentina
ddbs_filter(conn = conn, "countries", "argentina", predicate = "touches")

## filter without using a connection
ddbs_filter(countries_sf, argentina_sf, predicate = "touches")
} # }
```

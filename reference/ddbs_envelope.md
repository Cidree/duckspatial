# Returns the envelope (bounding box) of geometries

Returns the minimum bounding rectangle (envelope) of geometries from a
`sf` object or a DuckDB table. Returns the result as an `sf` object or
creates a new table in the database.

## Usage

``` r
ddbs_envelope(
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

  Logical. If `TRUE`, returns one envelope per feature. If `FALSE`
  (default), returns a single envelope for all geometries combined.

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

## Details

ST_Envelope returns the minimum bounding rectangle (MBR) of a geometry
as a polygon. For points and lines, this creates a rectangular polygon
that encompasses the geometry. For polygons, it returns the smallest
rectangle that contains the entire polygon.

When `by_feature = FALSE`, all geometries are combined and a single
envelope is returned that encompasses the entire dataset.

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)
library(sf)

# read data
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))

# input as sf, and output as sf
env <- ddbs_envelope(x = argentina_sf, by_feature = TRUE)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

# store in duckdb
ddbs_write_vector(conn, argentina_sf, "argentina")

# envelope for each feature
env <- ddbs_envelope("argentina", conn, by_feature = TRUE)

# single envelope for entire dataset
env_all <- ddbs_envelope("argentina", conn, by_feature = FALSE)

# create a new table with envelopes
ddbs_envelope("argentina", conn, name = "argentina_bbox", by_feature = TRUE)
} # }
```

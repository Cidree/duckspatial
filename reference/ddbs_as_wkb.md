# Convert geometries to Well-Known Binary (WKB) format

Converts spatial geometries to their Well-Known Binary (WKB)
representation. This function wraps DuckDB's ST_AsWkb spatial function.

## Usage

``` r
ddbs_as_wkb(x, conn = NULL, quiet = FALSE)
```

## Arguments

- x:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

A list of raw vectors, where each element contains the WKB
representation of a geometry

## Details

Well-Known Binary (WKB) is a binary representation of vector geometry
objects. WKB is more compact than WKT and is commonly used for efficient
storage and transfer of spatial data between systems. Each geometry is
returned as a raw vector of bytes.

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
argentina_sf <- st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))

## store in duckdb
ddbs_write_vector(conn, argentina_sf, "argentina")

## convert geometries to WKB
wkb_list <- ddbs_as_wkb(conn = conn, "argentina")

## convert without using a connection
wkb_list <- ddbs_as_wkb(argentina_sf)
} # }
```

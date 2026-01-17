# Convert geometries to Well-Known Text (WKT) format

Converts spatial geometries to their Well-Known Text (WKT)
representation. This function wraps DuckDB's ST_AsText spatial function.

## Usage

``` r
ddbs_as_text(x, conn = NULL, quiet = FALSE)
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

A character vector containing WKT representations of the geometries

## Details

Well-Known Text (WKT) is a text markup language for representing vector
geometry objects. This function is useful for exporting geometries in a
portable text format that can be used with other spatial tools and
databases.

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

## convert geometries to WKT
wkt_text <- ddbs_as_text(conn = conn, "argentina")

## convert without using a connection
wkt_text <- ddbs_as_text(argentina_sf)
} # }
```

# Convert geometries to hexadecimal Well-Known Binary (HEXWKB) format

Converts spatial geometries to their hexadecimal Well-Known Binary
(HEXWKB) representation. This function wraps DuckDB's ST_AsHEXWKB
spatial function.

## Usage

``` r
ddbs_as_hexwkb(x, conn = NULL, quiet = FALSE)
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

A character vector containing hexadecimal-encoded WKB representations of
the geometries

## Details

HEXWKB is a hexadecimal string representation of Well-Known Binary (WKB)
format. This encoding is human-readable (unlike raw WKB) while
maintaining the compact binary structure. HEXWKB is commonly used in
databases and web services for transmitting spatial data as text
strings.

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

## convert geometries to HEXWKB
hexwkb_text <- ddbs_as_hexwkb(conn = conn, "argentina")

## convert without using a connection
hexwkb_text <- ddbs_as_hexwkb(argentina_sf)
} # }
```

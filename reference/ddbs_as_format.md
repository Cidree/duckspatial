# Convert geometries to standard interchange formats

Convert spatial geometries to common interchange formats using DuckDB
spatial serialization functions.

- `ddbs_as_text()` – Convert geometries to Well-Known Text (WKT)

- `ddbs_as_wkb()` – Convert geometries to Well-Known Binary (WKB)

- `ddbs_as_hexwkb()` – Convert geometries to hexadecimal Well-Known
  Binary (HEXWKB)

- `ddbs_as_geojson()` – Convert geometries to GeoJSON

## Usage

``` r
ddbs_as_text(x, conn = NULL, quiet = FALSE)

ddbs_as_wkb(x, conn = NULL, quiet = FALSE)

ddbs_as_hexwkb(x, conn = NULL, quiet = FALSE)

ddbs_as_geojson(x, conn = NULL, quiet = FALSE)
```

## Arguments

- x:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

  Data is returned from this object.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

Depending on the function:

- `ddbs_as_text()` returns a character vector of WKT geometries

- `ddbs_as_wkb()` returns a list of raw vectors (binary WKB)

- `ddbs_as_hexwkb()` returns a character vector of HEXWKB strings

- `ddbs_as_geojson()` returns a character vector of GeoJSON strings

## Details

These functions are thin wrappers around DuckDB spatial serialization
functions (`ST_AsText`, `ST_AsWKB`, `ST_AsHEXWKB`, and `ST_AsGeoJSON`).

They are useful for exporting geometries into widely supported formats
for interoperability with external spatial tools, databases, and web
services.

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)

argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", package = "duckspatial")
)

ddbs_as_text(argentina_ddbs)
ddbs_as_wkb(argentina_ddbs)
ddbs_as_hexwkb(argentina_ddbs)
ddbs_as_geojson(argentina_ddbs)
} # }
```

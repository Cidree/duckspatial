# Convert geometries to standard interchange formats

Convert spatial geometries to common interchange formats using DuckDB
spatial serialization functions.

- `ddbs_as_text()` – Convert geometries to Well-Known Text (WKT)

- `ddbs_as_wkb()` – Convert geometries to Well-Known Binary (WKB)

- `ddbs_as_hexwkb()` – Convert geometries to hexadecimal Well-Known
  Binary (HEXWKB)

- `ddbs_as_geojson()` – Convert to GeoJSON (a `FeatureCollection` or one
  `Feature` per row)

## Usage

``` r
ddbs_as_text(x, conn = NULL)

ddbs_as_wkb(x, conn = NULL)

ddbs_as_hexwkb(x, conn = NULL)

ddbs_as_geojson(x, conn = NULL, feature_collection = TRUE)
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

- feature_collection:

  Logical, only used by `ddbs_as_geojson()`. If `TRUE` (default), all
  rows are returned as a single GeoJSON `FeatureCollection` string. If
  `FALSE`, a character vector with one `Feature` string per row is
  returned.

## Value

Depending on the function:

- `ddbs_as_text()` returns a character vector of WKT geometries

- `ddbs_as_wkb()` returns a list of raw vectors (binary WKB)

- `ddbs_as_hexwkb()` returns a character vector of HEXWKB strings

- `ddbs_as_geojson()` returns a single GeoJSON `FeatureCollection`
  string (class `"geojson"`) when `feature_collection = TRUE`, or a
  character vector of `Feature` strings (one per row) when `FALSE`;
  non-geometry columns are included as feature properties

## Details

These functions are thin wrappers around DuckDB spatial serialization
functions (`ST_AsText`, `ST_AsWKB`, `ST_AsHEXWKB`, and `ST_AsGeoJSON`).

They are useful for exporting geometries into widely supported formats
for interoperability with external spatial tools, databases, and web
services.

Unlike the other serializers, which only encode the geometry,
`ddbs_as_geojson()` produces complete GeoJSON `Feature`s: the geometry
is placed in the `geometry` member and all remaining (non-geometry)
columns are included as feature `properties`. By default the features
are wrapped in a single `FeatureCollection`, the canonical GeoJSON
artifact for writing `.geojson` files and feeding web services and
mapping libraries.

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

## a single FeatureCollection (default)
ddbs_as_geojson(argentina_ddbs)

## one Feature string per row
ddbs_as_geojson(argentina_ddbs, feature_collection = FALSE)
} # }
```

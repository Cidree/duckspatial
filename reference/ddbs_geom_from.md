# Create geometries from standard interchange formats

Parse serialized geometries from common interchange formats into a
spatial object, using DuckDB spatial deserialization functions. These
are the inverses of the
[`ddbs_as_text()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md)
/
[`ddbs_as_wkb()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md)
/
[`ddbs_as_hexwkb()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md)
/
[`ddbs_as_geojson()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md)
serializers.

- `ddbs_geom_from_text()` – Parse Well-Known Text (WKT)

- `ddbs_geom_from_wkb()` – Parse Well-Known Binary (WKB)

- `ddbs_geom_from_hexwkb()` – Parse hexadecimal Well-Known Binary
  (HEXWKB)

- `ddbs_geom_from_hexewkb()` – Parse hexadecimal Extended Well-Known
  Binary (HEXEWKB)

- `ddbs_geom_from_geojson()` – Parse GeoJSON

## Usage

``` r
ddbs_geom_from_text(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_geom_from_wkb(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_geom_from_hexwkb(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_geom_from_hexewkb(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_geom_from_geojson(
  x,
  crs = NULL,
  ...,
  geom_col = "geometry",
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  For `ddbs_geom_from_wkb()`, a list of raw vectors (binary WKB). For
  all other functions, a character vector of serialized geometries.

- crs:

  Character or numeric CRS specification (e.g. `"EPSG:4326"` or `4326`)
  to assign to the resulting geometries. Defaults to `NULL` (no CRS
  assigned).

- ...:

  Named vectors of additional attribute columns to include in the
  output. Each must have the same length as `x`.

- geom_col:

  Name of the geometry column in the output. Defaults to `"geometry"`.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- mode:

  Character. Controls the return type. Options:

  - `"duckspatial"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB

  - `"sf"`: Eagerly collected sf object (uses memory)

  Can be set globally via
  [`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)`(mode = "...")`
  or per-function via this argument. Per-function overrides global
  setting.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

Depends on the `mode` argument (or global preference set by
[`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)):

- `duckspatial` (default): A `duckspatial_df` (lazy spatial data frame)
  backed by dbplyr/DuckDB.

- `sf`: An eagerly collected object in R memory, that will return the
  same data type as the `sf` equivalent (e.g. `sf` or `units` vector).

When `name` is provided, the result is also written as a table or view
in DuckDB and the function returns `TRUE` (invisibly).

## Details

These functions are thin wrappers around the DuckDB spatial
deserialization functions (`ST_GeomFromText`, `ST_GeomFromWKB`,
`ST_GeomFromHEXWKB`, `ST_GeomFromHEXEWKB`, and `ST_GeomFromGeoJSON`).

They are useful for importing geometries produced by external spatial
tools, databases, and web services back into a `duckspatial` workflow.

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)

## from WKT
ddbs_geom_from_text(
  c("POINT (-58.38 -34.60)", "POINT (-64.18 -31.42)"),
  crs = 4326
)

## from GeoJSON, with an extra attribute column
ddbs_geom_from_geojson(
  '{"type":"Point","coordinates":[-58.38,-34.60]}',
  city = "Buenos Aires",
  crs  = 4326
)
} # }
```

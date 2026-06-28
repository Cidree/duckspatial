# Write a Mapbox Vector Tile pyramid to an MBTiles file

Generates a pyramid of Mapbox Vector Tiles (MVT) from a spatial dataset
and stores them in an [MBTiles](https://github.com/mapbox/mbtiles-spec)
file (a SQLite database). For each tile in the requested zoom range the
geometries are clipped and re-projected into tile space (`ST_AsMVTGeom`)
and encoded into a binary vector tile (`ST_AsMVT`).

## Usage

``` r
ddbs_write_mbtiles(
  x,
  dsn,
  layer_name = "layer",
  zoom_levels = 0:6,
  extent = 4096L,
  buffer = 256L,
  conn = NULL,
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

- dsn:

  Path to the output `.mbtiles` file.

- layer_name:

  Name of the vector tile layer. This is the value you pass as
  `source_layer` when adding the layer in a web map. Defaults to
  `"layer"`.

- zoom_levels:

  Integer vector of zoom levels to generate (e.g. `0:8`). Defaults to
  `0:6`.

- extent:

  Integer tile extent (resolution). Defaults to `4096`.

- buffer:

  Integer tile buffer in pixels. Defaults to `256`.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- overwrite:

  Logical. Overwrite `dsn` if it already exists. Defaults to `FALSE`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

The path to `dsn`, invisibly.

## Details

The input is re-projected to web mercator (`EPSG:3857`), so `x` must
have a CRS. All non-geometry columns are carried into the tiles as
feature properties; columns whose type is not supported by the vector
tile format are cast to `DOUBLE` (numeric types) or `VARCHAR`
(everything else), and `BLOB` columns are dropped.

Writing the SQLite container relies on DuckDB's `sqlite` extension,
which is installed and loaded automatically (an internet connection may
be required the first time).

MBTiles can be served with a vector tile server, or converted to
[PMTiles](https://github.com/protomaps/PMTiles) (e.g. with the
`pmtiles convert` CLI) for static hosting. The resulting tiles can then
be displayed with the mapgl package, for example:


    library(mapgl)
    maplibre() |>
      add_pmtiles_source(id = "src", url = "https://example.com/argentina.pmtiles") |>
      add_fill_layer(id = "fill", source = "src", source_layer = "argentina")

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)

argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", package = "duckspatial")
)

ddbs_write_mbtiles(
  argentina_ddbs,
  dsn         = tempfile(fileext = ".mbtiles"),
  layer_name  = "argentina",
  zoom_levels = 0:6
)
} # }
```

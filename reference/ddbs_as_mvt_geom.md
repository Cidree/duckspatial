# Transform geometries into Mapbox Vector Tile coordinate space

Prepares geometries for encoding as Mapbox Vector Tiles (MVT) by
clipping them to a tile's bounding box and transforming the coordinates
into the tile's integer pixel space. This is a low-level building block;
to produce a ready-to-serve tile pyramid use
[`ddbs_write_mbtiles`](https://cidree.github.io/duckspatial/reference/ddbs_write_mbtiles.md).

## Usage

``` r
ddbs_as_mvt_geom(
  x,
  bounds,
  extent = 4096L,
  buffer = 256L,
  clip_geom = TRUE,
  conn = NULL,
  name = NULL,
  mode = NULL,
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

- bounds:

  The tile bounding box, in the same CRS as `x`. Either a numeric vector
  `c(xmin, ymin, xmax, ymax)` or an object understood by
  [`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
  (e.g. an `sf`, `sfc`, or `bbox` object).

- extent:

  Integer. The width and height of the tile coordinate space (the
  geometry is mapped into the square `[0, extent]`). Defaults to `4096`.

- buffer:

  Integer. The number of pixels by which the tile bounds are expanded
  before clipping, to avoid rendering artefacts at tile edges. Defaults
  to `256`.

- clip_geom:

  Logical. If `TRUE` (default), geometries are clipped to the (buffered)
  tile bounds; geometries falling entirely outside become `NULL`. If
  `FALSE`, geometries are transformed but not clipped.

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

This is a wrapper around DuckDB's `ST_AsMVTGeom`. The output geometries
use the MVT tile coordinate system: the origin is the top-left corner
and the y-axis points downwards, so the result is no longer in the input
CRS (the CRS is dropped). Geometries that fall completely outside the
tile bounds return `NULL` when `clip_geom = TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)

argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", package = "duckspatial")
)

## use the layer's own bounding box as the tile bounds
ddbs_as_mvt_geom(argentina_ddbs, bounds = sf::st_bbox(sf::st_as_sf(argentina_ddbs)))

## explicit numeric bounds and a smaller tile extent
ddbs_as_mvt_geom(
  argentina_ddbs,
  bounds = c(-74, -56, -53, -21),
  extent = 256
)
} # }
```

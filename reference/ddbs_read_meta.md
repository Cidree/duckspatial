# Read metadata from a spatial file

Retrieves file-level metadata from a spatial vector file (e.g.
GeoPackage, Shapefile, GeoJSON) using DuckDB's `ST_Read_Meta()`
function. Returns information about the file's driver and its layers as
a tibble.

## Usage

``` r
ddbs_read_meta(path, conn = NULL)
```

## Arguments

- path:

  character, path to the spatial file to inspect.

- conn:

  A `DBIConnection` object to a DuckDB database

## Value

A `tibble` with one row per file and the following columns:

- file_name:

  Path to the file.

- driver_short_name:

  Short name of the GDAL driver (e.g. `"GPKG"`).

- driver_long_name:

  Full name of the GDAL driver (e.g. `"GeoPackage"`).

- layers:

  A list-column of data frames, one per file, each describing the layers
  contained in the file. Unnest with
  [`unnest`](https://tidyr.tidyverse.org/reference/unnest.html) to
  access individual layer attributes such as name, geometry type, and
  feature count.

## Examples

``` r
if (FALSE) { # \dontrun{
## Read metadata from a GeoPackage
meta <- ddbs_read_meta(
  system.file("spatial/rivers.geojson",
  package = "duckspatial")
)

## View file-level metadata
meta

## Inspect layer details
tidyr::unnest(meta, layers)
} # }
```

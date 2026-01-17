# Write spatial dataset to disk

Writes spatial data to disk using DuckDB's `COPY` command. Supports
Parquet (native) and various GDAL spatial formats. Format is
auto-detected from file extension for common formats, or can be
specified explicitly via `gdal_driver`.

## Usage

``` r
ddbs_write_dataset(
  data,
  path,
  gdal_driver = NULL,
  conn = NULL,
  overwrite = FALSE,
  crs = NULL,
  options = list(),
  partitioning = if (inherits(data, c("tbl_lazy", "duckspatial_df")))
    dplyr::group_vars(data) else NULL,
  parquet_compression = NULL,
  parquet_row_group_size = NULL,
  layer_creation_options = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  A `duckspatial_df`, `tbl_lazy` (DuckDB), or `sf` object.

- path:

  Path to output file.

- gdal_driver:

  GDAL driver name for writing spatial formats. If `NULL` (default), the
  driver is auto-detected from the file extension for common formats:

  - `.geojson`, `.json` → "GeoJSON"

  - `.shp` → "ESRI Shapefile"

  - `.gpkg` → "GPKG"

  - `.fgb` → "FlatGeobuf"

  - `.kml` → "KML"

  - `.gpx` → "GPX"

  - `.gml` → "GML"

  - `.sqlite` → "SQLite"

  For **non-standard file extensions** (e.g., `.dat`, `.xyz`) or to
  **explicitly override** auto-detection, specify the exact driver name
  as it appears in `ddbs_drivers()$short_name`. Examples:
  `gdal_driver = "GeoJSON"`, `gdal_driver = "ESRI Shapefile"`.

  **Note**: If you specify a driver that doesn't match the file
  extension (e.g., `path = "output.shp"` with
  `gdal_driver = "GeoJSON"`), a warning will be issued but your explicit
  driver choice will be honored (creating a GeoJSON file with `.shp`
  extension).

  The function validates that the specified driver is available and
  writable on your system. Note: `.parquet` and `.csv` files use native
  DuckDB writers and do not require a GDAL driver.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- overwrite:

  Logical. If `TRUE`, overwrites existing file.

- crs:

  Output CRS (e.g., "EPSG:4326"). Passed to GDAL as `SRS` option.
  Ignored for Parquet.

- options:

  Named list of additional options passed to `COPY`.

- partitioning:

  Character vector of columns to partition by (Parquet/CSV only).

- parquet_compression:

  Compression codec for Parquet.

- parquet_row_group_size:

  Row group size for Parquet.

- layer_creation_options:

  GDAL layer creation options.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

The `path` invisibly.

## References

This function is inspired by and builds upon the logic found in the
`duckdbfs` package (<https://github.com/cboettig/duckdbfs>),
particularly its `write_dataset` and `write_geo` functions. For advanced
features like cloud storage (S3) support, the `duckdbfs` package is
highly recommended.

## See also

[`ddbs_drivers()`](https://cidree.github.io/duckspatial/reference/ddbs_drivers.md)
to list all available GDAL drivers and formats.

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)

# Read example data
path <- system.file("spatial/countries.geojson", package = "duckspatial")
ds <- ddbs_open_dataset(path)

# Auto-detect format from extension
ddbs_write_dataset(ds, "output.geojson")
ddbs_write_dataset(ds, "output.gpkg")
ddbs_write_dataset(ds, "output.parquet")

# Explicit GDAL driver for non-standard extension
ddbs_write_dataset(ds, "mydata.dat", gdal_driver = "GeoJSON")

# See available drivers on your system
drivers <- ddbs_drivers()
writable <- drivers[drivers$can_create == TRUE, ]
head(writable)

# CRS override
ddbs_write_dataset(ds, "output_3857.geojson", crs = "EPSG:3857")

# Overwrite existing file
ddbs_write_dataset(ds, "output.gpkg", overwrite = TRUE)
} # }
```

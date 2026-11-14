# Write spatial dataset to disk

Writes spatial data to disk using DuckDB's `COPY` command for Parquet
and GDAL spatial formats, or as a native DuckDB database for `.duckdb`,
`.db`, and `.ddb` paths. Format is auto-detected from file extension for
common formats, or can be specified explicitly via `gdal_driver`.

## Usage

``` r
ddbs_write_dataset(
  data,
  path,
  gdal_driver = NULL,
  conn = NULL,
  overwrite = FALSE,
  crs = NULL,
  layer = "spatial",
  options = list(),
  partitioning = if (inherits(data, c("tbl_lazy", "duckspatial_df")))
    dplyr::group_vars(data) else NULL,
  parquet_compression = NULL,
  parquet_row_group_size = NULL,
  layer_creation_options = NULL,
  quiet = FALSE,
  duckdb_storage_version = duckspatial_storage_default()
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

- layer:

  Table name for native DuckDB database output.

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

- duckdb_storage_version:

  Storage compatibility for newly created persistent native DuckDB files
  (`.duckdb`, `.db`, `.ddb`). See
  <https://duckdb.org/docs/2.0/internals/storage> for more information
  on DuckDB storage versions and compatibility.

  - `"v2.0.0"` (**Default, Modern Storage**): Introduces lazy column
    metadata loading, enables `DICT_FSST` string compression by default,
    stores row deletions more compactly, and performs stronger
    corruption validation. Requires DuckDB \>= 2.0.0 to open the file.

  - `"v1.5.0"` (**Native Spatial Storage**): Preserves CRS metadata in
    native DuckDB `GEOMETRY` columns. Requires DuckDB \>= 1.5.0 to open
    the file.

  - `"v1.0.0"` (**Legacy Compatibility**): Creates files readable by
    older DuckDB versions (\>= 1.0.0). Persists CRS metadata in
    duckspatial-managed column comments (a convention not recognized by
    other spatial software).

  - `"latest"`: Use the highest storage version supported by your
    installed DuckDB engine.

  Other major version strings like `"v1.4.0"`, `"v1.3.0"`, etc., are
  also supported.

## Value

The `path` invisibly.

## Details

Persistent DuckDB database files created by duckspatial use **Modern
Storage** (`duckdb_storage_version = "v2.0.0"`) by default, which keeps
CRS metadata in native `GEOMETRY` columns. These files require DuckDB
\>= 2.0.0 to open. Use `duckdb_storage_version = "v1.5.0"` for files
readable by DuckDB \>= 1.5.0, or **Legacy Compatibility** (`"v1.0.0"`)
when the output must be readable by older DuckDB versions.

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

# Write to a temporary directory
out_dir <- tempdir()

# Auto-detect format from extension
ddbs_write_dataset(ds, file.path(out_dir, "output.geojson"))
ddbs_write_dataset(ds, file.path(out_dir, "output.gpkg"))
ddbs_write_dataset(ds, file.path(out_dir, "output.parquet"))

# Explicit GDAL driver for non-standard extension
ddbs_write_dataset(ds, file.path(out_dir, "mydata.dat"), gdal_driver = "GeoJSON")

# See available drivers on your system
drivers <- ddbs_drivers()
writable <- drivers[drivers$can_create == TRUE, ]
head(writable)

# CRS override
ddbs_write_dataset(ds, file.path(out_dir, "output_3857.geojson"), crs = "EPSG:3857")

# Overwrite existing file
ddbs_write_dataset(ds, file.path(out_dir, "output.gpkg"), overwrite = TRUE)

# Clean up
unlink(file.path(out_dir, c(
  "output.geojson", "output.gpkg", "output.parquet",
  "mydata.dat", "output_3857.geojson"
)))
} # }
```

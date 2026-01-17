# Open spatial dataset lazily via DuckDB

Reads spatial data directly from disk using DuckDB's spatial extension
or native Parquet reader, returning a `duckspatial_df` object for lazy
processing.

## Usage

``` r
ddbs_open_dataset(
  path,
  crs = NULL,
  layer = NULL,
  geom_col = NULL,
  conn = NULL,
  parquet_binary_as_string = NULL,
  parquet_file_row_number = NULL,
  parquet_filename = NULL,
  parquet_hive_partitioning = NULL,
  parquet_union_by_name = NULL,
  parquet_encryption_config = NULL,
  read_shp_mode = c("ST_ReadSHP", "GDAL"),
  read_osm_mode = c("GDAL", "ST_ReadOSM"),
  shp_encoding = NULL,
  gdal_spatial_filter = NULL,
  gdal_spatial_filter_box = NULL,
  gdal_keep_wkb = NULL,
  gdal_max_batch_size = NULL,
  gdal_sequential_layer_scan = NULL,
  gdal_sibling_files = NULL,
  gdal_allowed_drivers = NULL,
  gdal_open_options = NULL
)
```

## Arguments

- path:

  Path to spatial file. Supports Parquet (`.parquet`, with optional
  GeoParquet metadata), GeoJSON, GeoPackage, Shapefile, FlatGeoBuf, OSM
  PBF, and other GDAL-supported formats.

- crs:

  Coordinate reference system. Can be an EPSG code (e.g., 4326), a CRS
  string, or an `sf` crs object. If `NULL` (default), attempts to
  auto-detect from the file.

- layer:

  Layer name or index to read (ST_Read only). Default is NULL (first
  layer).

- geom_col:

  Name of the geometry column. Default is `NULL`, which attempts
  auto-detection.

- conn:

  DuckDB connection to use. If NULL, uses the default connection.

- parquet_binary_as_string:

  Logical. (Parquet) If TRUE, load binary columns as strings.

- parquet_file_row_number:

  Logical. (Parquet) If TRUE, include a `file_row_number` column.

- parquet_filename:

  Logical. (Parquet) If TRUE, include a `filename` column.

- parquet_hive_partitioning:

  Logical. (Parquet) If TRUE, interpret path as Hive partitioned.

- parquet_union_by_name:

  Logical. (Parquet) If TRUE, unify columns by name.

- parquet_encryption_config:

  List/Struct. (Parquet) Encryption configuration (advanced).

- read_shp_mode:

  Mode for reading Shapefiles. "ST_ReadSHP" (default, fast native
  reader) or "GDAL" (ST_Read).

- read_osm_mode:

  Mode for reading OSM PBF files. "GDAL" (default, ST_Read) or
  "ST_ReadOSM" (fast native reader, no geometry).

- shp_encoding:

  Encoding for Shapefiles when using "ST_ReadSHP" (e.g., "UTF-8",
  "ISO-8859-1").

- gdal_spatial_filter:

  Optional WKB geometry (as raw vector or hex string) to filter
  spatially (ST_Read only).

- gdal_spatial_filter_box:

  Optional bounding box (as numeric vector `c(minx, miny, maxx, maxy)`)
  (ST_Read only).

- gdal_keep_wkb:

  Logical. If TRUE, return WKB blobs instead of GEOMETRY type (ST_Read
  only).

- gdal_max_batch_size:

  Integer. Maximum batch size for reading (ST_Read only).

- gdal_sequential_layer_scan:

  Logical. If TRUE, scan layers sequentially (ST_Read only).

- gdal_sibling_files:

  Character vector. List of sibling files (ST_Read only).

- gdal_allowed_drivers:

  Character vector. List of allowed GDAL drivers (ST_Read only).

- gdal_open_options:

  Character vector. Driver-specific open options (ST_Read only).

## Value

A `duckspatial_df` object.

## References

This function is inspired by the dataset opening logic in the `duckdbfs`
package (<https://github.com/cboettig/duckdbfs>).

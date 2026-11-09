# Get or set global duckspatial options

Get or set global duckspatial options

## Usage

``` r
ddbs_options(output_type = NULL, mode = NULL, duckdb_storage_version = NULL)
```

## Arguments

- output_type:

  Character string. Controls the default return type for
  [ddbs_collect](https://cidree.github.io/duckspatial/reference/ddbs_collect.md).
  Must be one of:

  - `"sf"` (default): Eagerly collected `sf` object (in-memory).

  - `"tibble"`: Eagerly collected `tibble` without geometry.

  - `"raw"`: Eagerly collected `tibble` with geometry as raw WKB bytes.

  - `"geoarrow"`: Eagerly collected `tibble` with geometry as
    `geoarrow_vctr`.

  If `NULL` (the default), the existing option is not changed.

- mode:

  Character. Controls the return type. Options:

  - `"duckspatial"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB

  - `"sf"`: Eagerly collected sf object (uses memory)

  Can be set globally via `ddbs_options(mode = "...")` or per-function
  via this argument. Per-function overrides global setting.

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

Invisibly returns a list containing the currently set options.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set default mode to geoarrow
ddbs_options(mode = "geoarrow")

# Set default output to tibble
ddbs_options(output_type = "tibble")

# Check current settings
ddbs_options()
} # }
```

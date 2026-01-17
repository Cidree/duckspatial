# Get or set global duckspatial options

Get or set global duckspatial options

## Usage

``` r
ddbs_options(output_type = NULL)
```

## Arguments

- output_type:

  Character string. Controls the default return type for spatial
  operations. Must be one of:

  - `"duckspatial_df"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB.

  - `"sf"`: Eagerly collected `sf` object (in-memory).

  - `"tibble"`: Eagerly collected `tibble` without geometry.

  - `"raw"`: Eagerly collected `tibble` with geometry as raw WKB bytes.

  - `"geoarrow"`: Eagerly collected `tibble` with geometry as
    `geoarrow_vctr`.

  If `NULL` (the default), the existing option is not changed.

## Value

Invisibly returns a list containing the currently set options.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set default output to sf
ddbs_options(output_type = "sf")

# Set default output to tibble
ddbs_options(output_type = "tibble")

# Set default output to duckspatial_df
ddbs_options(output_type = "duckspatial_df")

# Check current settings
ddbs_options()
} # }
```

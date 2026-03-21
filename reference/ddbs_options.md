# Get or set global duckspatial options

Get or set global duckspatial options

## Usage

``` r
ddbs_options(output_type = NULL, mode = NULL)
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

  If `NULL` (the default), the existing option is not changed.

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

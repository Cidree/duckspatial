# Collect a duckspatial_df with flexible output formats

Materializes a lazy `duckspatial_df` object by executing the underlying
DuckDB query. Supports multiple output formats.

## Usage

``` r
# S3 method for class 'duckspatial_df'
collect(x, ..., as = NULL)

ddbs_collect(x, ..., as = c("sf", "tibble", "raw", "geoarrow"))
```

## Arguments

- x:

  A `duckspatial_df` object

- ...:

  Additional arguments passed to `collect`

- as:

  Output format. One of:

  `"sf"`

  :   (Default) Returns an `sf` object with `sfc` geometry

  `"tibble"`

  :   Returns a tibble with geometry column dropped (fastest)

  `"raw"`

  :   Returns a tibble with geometry as raw WKB bytes

  `"geoarrow"`

  :   Returns a tibble with geometry as `geoarrow_vctr`

## Value

Collected data in the specified format

Data in the specified format

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)

# Load lazy spatial data
nc <- ddbs_open_dataset(system.file("shape/nc.shp", package = "sf"))

# Perform lazy operations
result <- nc |> dplyr::filter(AREA > 0.1)

# Collect to sf (default)
result_sf <- ddbs_collect(result)
plot(result_sf["AREA"])

# Collect as tibble without geometry (fast)
result_tbl <- ddbs_collect(result, as = "tibble")

# Collect with raw WKB bytes
result_raw <- ddbs_collect(result, as = "raw")

# Collect as geoarrow for Arrow workflows
result_ga <- ddbs_collect(result, as = "geoarrow")
} # }
```

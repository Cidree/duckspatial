# dplyr methods for duckspatial_df

These methods use dplyr's extension mechanism (dplyr_reconstruct) to
properly preserve spatial metadata through operations.

Executes the accumulated query and stores the result in a DuckDB
temporary table. The result remains lazy (a `duckspatial_df`) but points
to the materialized data, avoiding repeated computation of complex query
plans.

## Usage

``` r
# S3 method for class 'duckspatial_df'
dplyr_reconstruct(data, template)

# S3 method for class 'duckspatial_df'
left_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  relationship = NULL
)

# S3 method for class 'duckspatial_df'
inner_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL,
  na_matches = c("na", "never"),
  relationship = NULL
)

# S3 method for class 'duckspatial_df'
head(x, n = 6L, ...)

# S3 method for class 'duckspatial_df'
compute(x, name = NULL, temporary = TRUE, ...)
```

## Arguments

- x:

  A `duckspatial_df` object

- ...:

  Additional arguments passed to
  [`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html)

- name:

  Optional name for the result table. If NULL, a unique temporary name
  is generated.

- temporary:

  If TRUE (default), creates a temporary table that is automatically
  cleaned up when the connection closes.

## Value

A new `duckspatial_df` pointing to the materialized table, with spatial
metadata (CRS, geometry column) preserved.

## Details

This is useful when you want to:

- Cache intermediate results for reuse across multiple subsequent
  operations

- Simplify complex query plans before heavy operations like spatial
  joins

- Force execution at a specific point without pulling data into R memory

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Complex pipeline - compute() caches intermediate result
result <- countries |>
  filter(POP_EST > 50000000) |>
  ddbs_filter(argentina, predicate = "touches") |>
  compute() |>  # Execute and cache here
  select(NAME_ENGL, POP_EST) |>
  ddbs_join(rivers, join = "intersects")

# Check query plan - should reference the cached table
show_query(result)
} # }
```

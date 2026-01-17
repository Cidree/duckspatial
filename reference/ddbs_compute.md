# Force computation of a lazy duckspatial_df

Executes the accumulated query and stores the result in a DuckDB
temporary table. The result remains lazy (a `duckspatial_df`) but points
to the materialized data, avoiding repeated computation of complex query
plans.

## Usage

``` r
ddbs_compute(x, ..., name = NULL, temporary = TRUE)
```

## Arguments

- x:

  A `duckspatial_df` object

- ...:

  Additional arguments passed to
  [`dplyr::compute`](https://dplyr.tidyverse.org/reference/compute.html)

- name:

  Optional name for the result table. If NULL, a unique temporary name
  is generated.

- temporary:

  If TRUE (default), creates a temporary table that is automatically
  cleaned up when the connection closes.

## Value

A new `duckspatial_df` pointing to the materialized table

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
library(duckspatial)
library(dplyr)

# Load lazy spatial data
countries <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", package = "duckspatial")
)

# Complex pipeline - ddbs_compute() caches intermediate result
cached <- countries |>
  filter(CNTR_ID %in% c("DE", "FR", "IT")) |>
  ddbs_compute()  # Execute and store in temp table

# Check query plan - should reference temp table
show_query(cached)

# Further operations continue from cached result
result <- cached |>
  ddbs_filter(other_layer, predicate = "intersects") |>
  st_as_sf()
} # }
```

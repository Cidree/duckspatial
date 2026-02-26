# Get the geometry type of features

Returns the type of each geometry (e.g., POINT, LINESTRING, POLYGON) in
the input features.

## Usage

``` r
ddbs_geometry_type(x, conn = NULL, by_feature = TRUE, quiet = FALSE)
```

## Arguments

- x:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

  Data is returned from this object.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- by_feature:

  Logical. If `TRUE`, the geometric operation is applied separately to
  each geometry. If `FALSE`, the geometric operation is applied to the
  data as a whole.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

A factor with geometry type(s)

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)

## read data
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
)

# option 1: passing sf objects
# Get geometry type for each feature
ddbs_geometry_type(countries_ddbs)

# Get overall geometry type
ddbs_geometry_type(countries_ddbs, by_feature = FALSE)
} # }
```

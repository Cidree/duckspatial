# Get the geometry type of features

Returns the type of each geometry (e.g., POINT, LINESTRING, POLYGON) in
the input features.

## Usage

``` r
ddbs_geometry_type(x, by_feature = TRUE, conn = NULL)
```

## Arguments

- x:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

  Data is returned from this object.

- by_feature:

  Logical. If `TRUE`, the geometric operation is applied separately to
  each geometry. If `FALSE`, the geometric operation is applied to the
  data as a whole.

- conn:

  A connection object to a DuckDB database. Required when an input is a
  table name. If `NULL` (the default), the connection of a
  `duckspatial_df` input is used when there is one; otherwise the
  default connection from
  [`ddbs_default_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_default_conn.md)
  is used.

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

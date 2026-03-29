# Drop geometry column from a duckspatial_df object

Removes the geometry column from a `duckspatial_df` object, returning a
lazy tibble without spatial information.

## Usage

``` r
ddbs_drop_geometry(x)
```

## Arguments

- x:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

  Data is returned from this object.

## Value

A lazy tibble backed by dbplyr

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

## drop geometry column
countries_tbl <- ddbs_drop_geometry(countries_ddbs)
} # }
```

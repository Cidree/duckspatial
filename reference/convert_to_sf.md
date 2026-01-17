# Converts from data frame to sf

Converts a table that has been read from DuckDB into an sf object

## Usage

``` r
convert_to_sf(data, crs, crs_column, x_geom)
```

## Arguments

- data:

  a tibble or data frame

- crs:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) The coordinates
  reference system of the data. Specify if the data doesn't have a
  `crs_column`, and you know the CRS.

- crs_column:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) a character
  string of length one specifying the column storing the CRS (created
  automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- x_geom:

  name of geometry

## Value

sf

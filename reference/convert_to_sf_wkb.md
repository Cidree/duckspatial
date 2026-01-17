# Converts from data frame to sf using WKB conversion

Converts a table that has been read from DuckDB into an sf object.

## Usage

``` r
convert_to_sf_wkb(data, crs, crs_column, x_geom)
```

## Arguments

- data:

  a tibble or data frame

- crs:

  The coordinates reference system of the data. Specify if the data
  doesn't have a `crs_column`, and you know the CRS.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- x_geom:

  name of geometry column

## Value

sf

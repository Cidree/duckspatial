# Converts from data frame to sf using WKB conversion

Converts a table that has been read from DuckDB into an sf object.

## Usage

``` r
convert_to_sf_wkb(data, crs, x_geom)
```

## Arguments

- data:

  a tibble or data frame

- crs:

  The coordinates reference system

- x_geom:

  name of geometry column

## Value

sf

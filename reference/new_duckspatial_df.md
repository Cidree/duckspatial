# Create a duckspatial lazy spatial data frame

Extends tbl_duckdb_connection with spatial metadata (CRS, geometry
column).

## Usage

``` r
new_duckspatial_df(x, crs = NULL, geom_col = "geom", source_table = NULL)
```

## Arguments

- x:

  Input: tbl_duckdb_connection, tbl_lazy, or similar dbplyr object

- crs:

  CRS object or string

- geom_col:

  Name of geometry column (default: "geom")

- source_table:

  Name of the source table if applicable

## Value

A duckspatial_df object

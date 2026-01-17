# Convert objects to duckspatial_df

Convert objects to duckspatial_df

## Usage

``` r
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = "geom", ...)

# S3 method for class 'duckspatial_df'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)

# S3 method for class 'sf'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)

# S3 method for class 'tbl_duckdb_connection'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = "geom", ...)

# S3 method for class 'tbl_lazy'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = "geom", ...)

# S3 method for class 'character'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = "geom", ...)

# S3 method for class 'data.frame'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = "geom", ...)
```

## Arguments

- x:

  Object to convert (sf, tbl_lazy, data.frame, or table name)

- conn:

  DuckDB connection (required for character table names)

- crs:

  CRS object or string (auto-detected from sf objects)

- geom_col:

  Geometry column name (default: "geom")

- ...:

  Additional arguments passed to methods

## Value

A duckspatial_df object

# Convert objects to duckspatial_df

`as_duckspatial_df()` creates a lazy spatial data frame
(`duckspatial_df`) from various inputs. When `x` is a table name
(character) or an existing DuckDB table (`tbl_duckdb_connection`), the
function creates a zero-copy representation of the data directly from
the database without loading it into memory. This is the canonical way
to "register" or wrap existing persistent spatial tables.

**CRS Persistence:** `duckspatial` reads native DuckDB 1.5.0+ CRS
metadata and, for compatibility with files written by older versions of
`duckspatial`, CRS metadata stored in column comments. DuckDB files
saved in pre-1.5.0 format without `duckspatial`-managed comments will
not have CRS information and will default to `NA` with a warning.

## Usage

``` r
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)

# S3 method for class 'duckspatial_df'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)

# S3 method for class 'sf'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)

# S3 method for class 'tbl_duckdb_connection'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)

# S3 method for class 'tbl_lazy'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)

# S3 method for class 'character'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)

# S3 method for class 'data.frame'
as_duckspatial_df(x, conn = NULL, crs = NULL, geom_col = NULL, ...)
```

## Arguments

- x:

  Object to convert (sf, tbl_lazy, data.frame, or table name)

- conn:

  DuckDB connection (required for character table names)

- crs:

  CRS object or string. Auto-detected from `sf` objects and persistent
  DuckDB tables.

- geom_col:

  Geometry column name (default: "geom")

- ...:

  Additional arguments passed to methods:

  `coords`

  :   Character vector of length 2 for point ingestion

  `wkt`

  :   Character name of WKT column for ingestion

  `remove`

  :   Logical. If TRUE (default), coordinate/WKT columns are removed

  `na.fail`

  :   Logical. If TRUE (default), errors on missing values

## Value

A duckspatial_df object

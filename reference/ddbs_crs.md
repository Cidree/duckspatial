# Check CRS of spatial objects or database tables

This is an S3 generic that extracts CRS information from various spatial
objects.

## Usage

``` r
ddbs_crs(x, ...)

# S3 method for class 'duckspatial_df'
ddbs_crs(x, ...)

# S3 method for class 'sf'
ddbs_crs(x, ...)

# S3 method for class 'tbl_duckdb_connection'
ddbs_crs(x, ...)

# S3 method for class 'character'
ddbs_crs(x, conn, crs_column = "crs_duckspatial", ...)

# S3 method for class 'duckdb_connection'
ddbs_crs(x, name, ...)

# Default S3 method
ddbs_crs(x, ...)
```

## Arguments

- x:

  An object containing spatial data. Can be:

  - `duckspatial_df`: Lazy spatial data frame (CRS from attributes)

  - `sf`: sf object (CRS from sf metadata)

  - `character`: Name of table in database (requires `conn`)

- ...:

  Additional arguments passed to methods

- conn:

  A DuckDB connection (required for character method)

- crs_column:

  Column name storing CRS info (default: "crs_duckspatial")

- name:

  Table name (for backward compatibility when first arg is connection)

## Value

CRS object from `sf` package

## Examples

``` r
if (FALSE) { # interactive()
## load packages
library(duckdb)
library(duckspatial)
library(sf)

# Method 1: duckspatial_df objects
nc <- ddbs_open_dataset(system.file("shape/nc.shp", package = "sf"))
ddbs_crs(nc)

# Method 2: sf objects
nc_sf <- st_read(system.file("shape/nc.shp", package = "sf"))
ddbs_crs(nc_sf)

# Method 3: table name in database
conn <- ddbs_create_conn(dbdir = "memory")
ddbs_write_vector(conn, nc_sf, "nc_table")
ddbs_crs(conn, "nc_table")
ddbs_stop_conn(conn)
}
```

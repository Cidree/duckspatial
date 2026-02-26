# Check first rows of the data

Prints a transposed table of the first rows of a DuckDB table, similarly
as the S3
[dplyr::glimpse](https://pillar.r-lib.org/reference/glimpse.html)
method.

## Usage

``` r
ddbs_glimpse(
  conn,
  name,
  crs = NULL,
  crs_column = "crs_duckspatial",
  quiet = FALSE
)
```

## Arguments

- conn:

  A `DBIConnection` object to a DuckDB database

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

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

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

Invisibly `duckspatial_df` object

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
argentina_sf <- ddbs_open_dataset(system.file("spatial/argentina.geojson", package = "duckspatial"))

## store in duckdb
ddbs_write_vector(conn, argentina_sf, "argentina")

## glimpse the inserted table
ddbs_glimpse(conn, "argentina")
} # }
```

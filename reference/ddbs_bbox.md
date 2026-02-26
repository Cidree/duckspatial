# Get the bounding box of geometries

Returns the minimal rectangle that encloses the geometry, typically used
to summarize its spatial extent.

## Usage

``` r
ddbs_bbox(
  x,
  by_feature = FALSE,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  overwrite = FALSE,
  quiet = FALSE
)
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

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

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

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

A data frame or `TRUE` (invisibly) for table creation when name is not
NULL.

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)

## read data
argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", 
  package = "duckspatial")
)

# option 1: passing sf objects
ddbs_bbox(argentina_ddbs)


## option 2: passing the names of tables in a duckdb db

# creates a duckdb write sf to it
conn <- duckspatial::ddbs_create_conn()
ddbs_write_vector(conn, argentina_ddbs, "argentina_tbl", overwrite = TRUE)

output2 <- ddbs_bbox(
    conn = conn,
    x = "argentina_tbl",
    name = "argentina_bbox"
)

DBI::dbReadTable(conn, "argentina_bbox")
} # }
```

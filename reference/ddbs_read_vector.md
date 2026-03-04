# Load spatial vector data from DuckDB into R

**\[deprecated\]**

`ddbs_read_vector()` was renamed to
[`ddbs_read_table`](https://cidree.github.io/duckspatial/reference/ddbs_read_table.md).

## Usage

``` r
ddbs_read_vector(
  conn,
  name,
  crs = NULL,
  crs_column = "crs_duckspatial",
  clauses = NULL,
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

- clauses:

  character, additional SQL code to modify the query from the table
  (e.g. "WHERE ...", "ORDER BY...")

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

an sf object

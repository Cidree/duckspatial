# Load spatial vector data from DuckDB into R

**\[deprecated\]**

`ddbs_read_vector()` was renamed to
[`ddbs_read_table`](https://cidree.github.io/duckspatial/reference/ddbs_read_table.md).

## Usage

``` r
ddbs_read_vector(conn, name, clauses = NULL, quiet = FALSE)
```

## Arguments

- conn:

  A `DBIConnection` object to a DuckDB database

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- clauses:

  character, additional SQL code to modify the query from the table
  (e.g. "WHERE ...", "ORDER BY...")

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

an sf object

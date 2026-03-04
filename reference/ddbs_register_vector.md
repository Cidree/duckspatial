# Register an SF Object as an Arrow Table in DuckDB

**\[deprecated\]**

`ddbs_register_vector()` was renamed to
[`ddbs_register_table`](https://cidree.github.io/duckspatial/reference/ddbs_register_table.md).

## Usage

``` r
ddbs_register_vector(conn, data, name, overwrite = FALSE, quiet = FALSE)
```

## Arguments

- conn:

  A `DBIConnection` object to a DuckDB database

- data:

  A `sf` object to write to the DuckDB database, or the path to a local
  file that can be read with `ST_READ`

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

TRUE (invisibly) on successful registration.

# Write an SF Object to a DuckDB Database

**\[deprecated\]**

`ddbs_write_vector()` was renamed to
[`ddbs_write_table`](https://cidree.github.io/duckspatial/reference/ddbs_write_table.md).

## Usage

``` r
ddbs_write_vector(
  conn,
  data,
  name,
  overwrite = FALSE,
  temp_view = FALSE,
  quiet = FALSE
)
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

- temp_view:

  If `TRUE`, registers the `sf` object as a temporary Arrow-backed
  database 'view' using
  [ddbs_register_table](https://cidree.github.io/duckspatial/reference/ddbs_register_table.md)
  instead of creating a persistent table. This is much faster but the
  view will not persist. Defaults to `FALSE`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

TRUE (invisibly) for successful import

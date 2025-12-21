# Check first rows of the data

Check first rows of the data

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

  A connection object to a DuckDB database

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names.

- crs:

  The coordinates reference system of the data. Specify if the data
  doesn't have a `crs_column`, and you know the CRS.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

`sf` object

## Examples

``` r
if (FALSE) { # interactive()
## TODO
2+2
}
```

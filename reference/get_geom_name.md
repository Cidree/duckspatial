# Get column names in a DuckDB database

Get column names in a DuckDB database

## Usage

``` r
get_geom_name(conn, x, rest = FALSE, collapse = FALSE)
```

## Arguments

- conn:

  A connection object to a DuckDB database

- x:

  name of the table

- rest:

  whether to return geometry column name, of the rest of the columns

## Value

name of the geometry column of a table

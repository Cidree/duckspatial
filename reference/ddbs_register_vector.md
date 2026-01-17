# Register an SF Object as an Arrow Table in DuckDB

This function registers a Simple Features (SF) object as a temporary
Arrow-backed view in a DuckDB database. This is a zero-copy operation
and is significantly faster than `ddbs_write_vector` for workflows that
do not require data to be permanently materialized in the database.

## Usage

``` r
ddbs_register_vector(conn, data, name, overwrite = FALSE, quiet = FALSE)
```

## Arguments

- conn:

  A connection object to a DuckDB database

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

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)
library(sf)

conn <- ddbs_create_conn("memory")

nc <- st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)

ddbs_register_vector(conn, nc, "nc_arrow_view")

dbGetQuery(conn, "SELECT COUNT(*) FROM nc_arrow_view;")

ddbs_stop_conn(conn, shutdown = TRUE)
} # }
```

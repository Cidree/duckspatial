# Write an SF Object to a DuckDB Database

This function writes a Simple Features (SF) object into a DuckDB
database as a new table. The table is created in the specified schema of
the DuckDB database.

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

- temp_view:

  If `TRUE`, registers the `sf` object as a temporary Arrow-backed
  database 'view' using `ddbs_register_vector` instead of creating a
  persistent table. This is much faster but the view will not persist.
  Defaults to `FALSE`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

TRUE (invisibly) for successful import

## Examples

``` r
if (FALSE) { # interactive()
## load packages
library(duckspatial)
library(sf)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## create random points
random_points <- data.frame(
  id = 1:5,
  x = runif(5, min = -180, max = 180),  # Random longitude values
  y = runif(5, min = -90, max = 90)     # Random latitude values
)

## convert to sf
sf_points <- st_as_sf(random_points, coords = c("x", "y"), crs = 4326)

## insert data into the database
ddbs_write_vector(conn, sf_points, "points")

## read data back into R
ddbs_read_vector(conn, "points", crs = 4326)

## disconnect from db
dbDisconnect(conn)
}
```

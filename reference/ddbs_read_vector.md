# Load spatial vector data from DuckDB into R

Retrieves the data from a DuckDB table, view, or Arrow view with a
geometry column, and converts it to an R `sf` object. This function
works with both persistent tables created by `ddbs_write_vector` and
temporary Arrow views created by `ddbs_register_vector`.

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

  A connection object to a DuckDB database

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- crs:

  The coordinates reference system of the data. Specify if the data
  doesn't have a `crs_column`, and you know the CRS.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
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
  x = runif(5, min = -180, max = 180),
  y = runif(5, min = -90, max = 90)
)

## convert to sf
sf_points <- st_as_sf(random_points, coords = c("x", "y"), crs = 4326)

## Example 1: Write and read persistent table
ddbs_write_vector(conn, sf_points, "points")
ddbs_read_vector(conn, "points", crs = 4326)

## Example 2: Register and read Arrow view (faster, temporary)
ddbs_register_vector(conn, sf_points, "points_view")
ddbs_read_vector(conn, "points_view", crs = 4326)

## disconnect from db
ddbs_stop_conn(conn)
}
```

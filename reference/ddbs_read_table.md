# Reads a vectorial table from DuckDB into R

Retrieves the data from a DuckDB table, view, or Arrow view with a
geometry column, and converts it to an R `sf` object. This function
works with both persistent tables created by `ddbs_write_table` and
temporary Arrow views created by `ddbs_register_table`.

## Usage

``` r
ddbs_read_table(conn, name, clauses = NULL, quiet = FALSE)
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

## Examples

``` r
if (FALSE) { # \dontrun{
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
ddbs_read_table(conn, "points")

## Example 2: Register and read Arrow view (faster, temporary)
ddbs_register_vector(conn, sf_points, "points_view")
ddbs_read_table(conn, "points_view")

## disconnect from db
ddbs_stop_conn(conn)
} # }
```

# Returns the concave hull enclosing the geometry

Returns the concave hull enclosing the geometry from an `sf` object or
from a DuckDB table using the spatial extension. Returns the result as
an `sf` object or creates a new table in the database.

## Usage

``` r
ddbs_concave_hull(
  x,
  ratio = 0.5,
  allow_holes = TRUE,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- ratio:

  Numeric. The ratio parameter dictates the level of concavity; `1`
  returns the convex hull, while `0` indicates to return the most
  concave hull possible. Defaults to `0.5`.

- allow_holes:

  Boolean. If `TRUE` (the default), it allows the output to contain
  holes.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

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

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

an `sf` object or `TRUE` (invisibly) for table creation

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)
library(sf)

# create points data
n <- 5
points_sf <- data.frame(
    id = 1,
    x = runif(n, min = -180, max = 180),
    y = runif(n, min = -90, max = 90)
    ) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
    st_geometry() |>
    st_combine() |>
    st_cast("MULTIPOINT") |>
    st_as_sf()

# option 1: passing sf objects
output1 <- duckspatial::ddbs_concave_hull(x = points_sf)

plot(output1)


# option 2: passing the name of a table in a duckdb db

# creates a duckdb
conn <- duckspatial::ddbs_create_conn()

# write sf to duckdb
ddbs_write_vector(conn, points_sf, "points_tbl")

# spatial join
output2 <- duckspatial::ddbs_concave_hull(
    conn = conn,
    x = "points_tbl"
    )

plot(output2)

} # }
```

# Geometry binary operations

Perform geometric set operations between two sets of geometries.

## Usage

``` r
ddbs_intersection(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_difference(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_sym_difference(
  x,
  y,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

  Data is returned from this object.

- y:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- conn_x:

  A `DBIConnection` object to a DuckDB database for the input `x`. If
  `NULL` (default), it is resolved from `conn` or extracted from `x`.

- conn_y:

  A `DBIConnection` object to a DuckDB database for the input `y`. If
  `NULL` (default), it is resolved from `conn` or extracted from `y`.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- mode:

  Character. Controls the return type. Options:

  - `"duckspatial"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB

  - `"sf"`: Eagerly collected sf object (uses memory)

  Can be set globally via
  [`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)`(mode = "...")`
  or per-function via this argument. Per-function overrides global
  setting.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

Depends on the `mode` argument (or global preference set by
[`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)):

- `duckspatial` (default): A `duckspatial_df` (lazy spatial data frame)
  backed by dbplyr/DuckDB.

- `sf`: An eagerly collected object in R memory, that will return the
  same data type as the `sf` equivalent (e.g. `sf` or `units` vector).

When `name` is provided, the result is also written as a table or view
in DuckDB and the function returns `TRUE` (invisibly).

## Details

These functions perform different geometric set operations:

- `ddbs_intersection`:

  Returns the geometric intersection of two sets of geometries,
  producing the area, line, or point shared by both.

- `ddbs_difference`:

  Returns the portion of the first geometry that does not overlap with
  the second geometry.

- `ddbs_sym_difference`:

  Returns the portions of both geometries that do not overlap with each
  other. Equivalent to `(A - B) UNION (B - A)`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)
library(sf)

# Create two overlapping polygons for testing
poly1 <- st_polygon(list(matrix(c(
  0, 0,
  4, 0,
  4, 4,
  0, 4,
  0, 0
), ncol = 2, byrow = TRUE)))

poly2 <- st_polygon(list(matrix(c(
  2, 2,
  6, 2,
  6, 6,
  2, 6,
  2, 2
), ncol = 2, byrow = TRUE)))

x <- st_sf(id = 1, geometry = st_sfc(poly1), crs = 4326)
y <- st_sf(id = 2, geometry = st_sfc(poly2), crs = 4326)

# Visualize the input polygons
plot(st_geometry(x), col = "lightblue", main = "Input Polygons")
plot(st_geometry(y), col = "lightcoral", add = TRUE, alpha = 0.5)

# Intersection: only the overlapping area (2,2 to 4,4)
result_intersect <- ddbs_intersection(x, y)
plot(st_geometry(result_intersect), col = "purple", 
     main = "Intersection")

# Difference: part of x not in y (L-shaped area)
result_diff <- ddbs_difference(x, y)
plot(st_geometry(result_diff), col = "lightblue", 
     main = "Difference (x - y)")

# Symmetric Difference: parts of both that don't overlap
result_symdiff <- ddbs_sym_difference(x, y)
plot(st_geometry(result_symdiff), col = "orange", 
     main = "Symmetric Difference")

# Using with database connection
conn <- ddbs_create_conn(dbdir = "memory")

ddbs_write_vector(conn, x, "poly_x")
ddbs_write_vector(conn, y, "poly_y")

# Perform operations with connection
ddbs_intersection("poly_x", "poly_y", conn)
ddbs_difference("poly_x", "poly_y", conn)
ddbs_sym_difference("poly_x", "poly_y", conn)

# Save results to database table
ddbs_difference("poly_x", "poly_y", conn, name = "diff_result")
} # }
```

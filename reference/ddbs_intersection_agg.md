# Aggregate the intersection of geometries

Computes the geometric intersection of a set of geometries — the area
common to all of them — using DuckDB's `ST_Intersection_Agg()`
aggregate. This is the intersection counterpart to
[`ddbs_union_agg()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md).

## Usage

``` r
ddbs_intersection_agg(
  x,
  by = NULL,
  conn = NULL,
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

- by:

  Character vector of one or more column names to group by. The
  intersection is computed within each group. When `NULL` (default), all
  geometries are intersected into a single geometry.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

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

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)

## create a connection and three overlapping polygons
conn <- ddbs_create_conn()
polys <- sf::st_as_sf(
  data.frame(grp = c("a", "a", "a")),
  geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0, 3,0, 3,3, 0,3, 0,0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(1,1, 4,1, 4,4, 1,4, 1,1), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(2,2, 5,2, 5,5, 2,5, 2,2), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(3,3, 6,3, 6,6, 3,6, 3,3), ncol = 2, byrow = TRUE)))
  ),
  crs = 4326
)

## intersect all geometries into their common area
polys_inters <- ddbs_intersection_agg(polys, mode = "sf")

## intersect within groups
polys_inters_grp <- ddbs_intersection_agg(polys, by = "grp", mode = "sf")

## plot them
plot(polys["grp"], key.pos = NULL, reset = FALSE)
plot(polys_inters_grp["grp"], pal = c("red", "blue"), add = TRUE)
plot(sf::st_geometry(polys_inters), col = "black", pch = 19, cex = 2, add = TRUE)
} # }
```

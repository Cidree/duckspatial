# Operations on polygonal coverages

Functions for working with polygonal coverages — sets of polygons that
share edges with no overlaps or gaps:

- `ddbs_coverage_union()` - Union all polygons in a coverage into a
  single geometry

- `ddbs_coverage_invalid_edges()` - Detect edges not shared by exactly
  two polygons

## Usage

``` r
ddbs_coverage_union(
  x,
  conn = NULL,
  name = NULL,
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_coverage_invalid_edges(
  x,
  tolerance = 0,
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

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), no table is written and the result is
  returned as a `duckspatial_df` or `sf` object, depending on `mode`

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

- tolerance:

  Numeric. Tolerance used to detect shared edges in
  `ddbs_coverage_invalid_edges()`. Default is `0`.

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

### ddbs_coverage_union(x)

Unions all polygons into one geometry. Faster than
[`ddbs_union()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md)
on valid coverages, at the cost of more memory.

### ddbs_coverage_invalid_edges(x, tolerance)

Returns `NULL` if `x` is not a polygonal coverage, or if it is already
valid. Use this to validate a coverage before relying on
`ddbs_coverage_union()`.

## See also

[ddbs_union_funs](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md)
for general-purpose union/combine operations.

## Examples

``` r
if (FALSE) { # \dontrun{
library(duckspatial)
library(sf)

# ddbs_coverage_invalid_edges

## Two squares meant to share the edge x=1, but one has a mismatched vertex
p1 <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
p2 <- st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,0.9), c(1,0), c(1,0)))) 
sfc <- st_sfc(p1, p2, crs = 4326)
sf_obj <- st_sf(id = 1:2, geom = sfc)

ddbs_coverage_invalid_edges(sf_obj)

# ddbs_coverage_union

## Load countries dataset
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
)

## Union into a single geometry
ddbs_coverage_union(countries_ddbs)
} # }
```

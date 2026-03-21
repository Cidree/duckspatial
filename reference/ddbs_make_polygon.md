# Create a polygon from a single closed linestring

Converts a single closed linestring geometry into a polygon. The
linestring must be closed (first and last points identical). Does not
work with MULTILINESTRING inputs - use
[`ddbs_polygonize()`](https://cidree.github.io/duckspatial/reference/ddbs_polygonize.md)
or
[`ddbs_build_area()`](https://cidree.github.io/duckspatial/reference/ddbs_build_area.md)
instead.

## Usage

``` r
ddbs_make_polygon(
  x,
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

## See also

[`ddbs_polygonize()`](https://cidree.github.io/duckspatial/reference/ddbs_polygonize.md),
[`ddbs_build_area()`](https://cidree.github.io/duckspatial/reference/ddbs_build_area.md)

Other polygon construction:
[`ddbs_build_area()`](https://cidree.github.io/duckspatial/reference/ddbs_build_area.md),
[`ddbs_polygonize()`](https://cidree.github.io/duckspatial/reference/ddbs_polygonize.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## load package
library(duckspatial)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", 
  package = "duckspatial")
)

## store in duckdb
ddbs_write_vector(conn, argentina_ddbs, "argentina")

## extract exterior ring as linestring, then convert back to polygon
ring_ddbs <- ddbs_exterior_ring(conn = conn, "argentina")
ddbs_make_polygon(conn = conn, ring_ddbs, name = "argentina_poly")

## create polygon without using a connection
ddbs_make_polygon(ring_ddbs)
} # }
```

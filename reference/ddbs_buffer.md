# Creates a buffer around geometries

Computes a polygon that represents all locations within a specified
distance from the original geometry

## Usage

``` r
ddbs_buffer(
  x,
  distance,
  num_triangles = 8L,
  cap_style = "CAP_ROUND",
  join_style = "JOIN_ROUND",
  mitre_limit = 1,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
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

- distance:

  a numeric value specifying the buffer distance. Units correspond to
  the coordinate system of the geometry (e.g. degrees or meters)

- num_triangles:

  an integer representing how many triangles will be produced to
  approximate a quarter circle. The larger the number, the smoother the
  resulting geometry. Default is 8.

- cap_style:

  a character string specifying the cap style. Must be one of
  "CAP_ROUND" (default), "CAP_FLAT", or "CAP_SQUARE". Case-insensitive.

- join_style:

  a character string specifying the join style. Must be one of
  "JOIN_ROUND" (default), "JOIN_MITRE", or "JOIN_BEVEL".
  Case-insensitive.

- mitre_limit:

  a numeric value specifying the mitre limit ratio. Only applies when
  `join_style` is "JOIN_MITRE". It is the ratio of the distance from the
  corner to the mitre point to the corner radius. Default is 1.0.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- crs:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) The coordinates
  reference system of the data. Specify if the data doesn't have a
  `crs_column`, and you know the CRS.

- crs_column:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) a character
  string of length one specifying the column storing the CRS (created
  automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

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

## create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", 
  package = "duckspatial")
)

## store in duckdb
ddbs_write_vector(conn, argentina_ddbs, "argentina")

## basic buffer
ddbs_buffer(conn = conn, "argentina", distance = 1)

## buffer with custom parameters
ddbs_buffer(conn = conn, "argentina", distance = 1, 
            num_triangles = 16, cap_style = "CAP_SQUARE")

## buffer without using a connection
ddbs_buffer(argentina_ddbs, distance = 1)
} # }
```

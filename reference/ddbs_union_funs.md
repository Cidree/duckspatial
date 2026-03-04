# Union and combine geometries

Perform union and combine operations on spatial geometries in DuckDB.

- `ddbs_union()` - Union all geometries into one, or perform pairwise
  union between two datasets

- `ddbs_union_agg()` - Union geometries grouped by one or more columns

- `ddbs_combine()` - Combine geometries into a MULTI-geometry without
  dissolving boundaries

## Usage

``` r
ddbs_union(
  x,
  y = NULL,
  by_feature = FALSE,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_combine(
  x,
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  mode = NULL,
  overwrite = FALSE,
  quiet = FALSE
)

ddbs_union_agg(
  x,
  by,
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

- y:

  Input spatial data. Can be:

  - `NULL` (default): performs only the union of `x`

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

- by_feature:

  Logical. When `y` is provided:

  - `FALSE` (default) - Union all geometries from both `x` and `y` into
    a single geometry

  - `TRUE` - Perform row-by-row union between matching features from `x`
    and `y` (requires same number of rows)

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

- by:

  Character vector specifying one or more column names to group by when
  computing unions. Geometries will be unioned within each group.
  Default is `NULL`

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

### ddbs_union(x, y, by_feature)

Performs geometric union operations that dissolve internal boundaries:

- When `y = NULL`: Unions all geometries in `x` into a single geometry

- When `y != NULL` and `by_feature = FALSE`: Unions all geometries from
  both `x` and `y` into a single geometry

- When `y != NULL` and `by_feature = TRUE`: Performs row-wise union,
  pairing the first geometry from `x` with the first from `y`, second
  with second, etc.

### ddbs_union_agg(x, by)

Groups geometries by one or more columns, then unions geometries within
each group. Useful for dissolving boundaries between features that share
common attributes.

### ddbs_combine(x)

Combines all geometries into a single MULTI-geometry (e.g.,
MULTIPOLYGON, MULTILINESTRING) without dissolving shared boundaries.
This is faster than union but preserves all original geometry
boundaries.

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(dplyr)
library(duckspatial)

## create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
) |> 
  filter(ISO3_CODE != "ATA")

rivers_ddbs <- ddbs_open_dataset(
  system.file("spatial/rivers.geojson", 
  package = "duckspatial")
) |> 
 ddbs_transform("EPSG:4326")

## combine countries into a single MULTI-geometry
## (without solving boundaries)
combined_countries_ddbs <- ddbs_combine(countries_ddbs)

## combine countries into a single MULTI-geometry
## (solving boundaries)
union_countries_ddbs <- ddbs_union(countries_ddbs)

## union of geometries of two objects, into 1 geometry
union_countries_rivers_ddbs <- ddbs_union(countries_ddbs, rivers_ddbs)
} # }
```

# Evaluate spatial predicates between geometries

Determines which geometries in one dataset satisfy a specified spatial
relationship with geometries in another dataset, such as intersection,
containment, or touching.

## Usage

``` r
ddbs_predicate(
  x,
  y,
  predicate = "intersects",
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  distance = NULL,
  quiet = FALSE
)

ddbs_intersects(x, y, ...)

ddbs_covers(x, y, ...)

ddbs_touches(x, y, ...)

ddbs_is_within_distance(x, y, distance = NULL, ...)

ddbs_disjoint(x, y, ...)

ddbs_within(x, y, ...)

ddbs_contains(x, y, ...)

ddbs_overlaps(x, y, ...)

ddbs_crosses(x, y, ...)

ddbs_equals(x, y, ...)

ddbs_covered_by(x, y, ...)

ddbs_intersects_extent(x, y, ...)

ddbs_contains_properly(x, y, ...)

ddbs_within_properly(x, y, ...)
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

- predicate:

  A geometry predicate function. Defaults to `intersects`, a wrapper of
  `ST_Intersects`. See details for other options.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- conn_x:

  A `DBIConnection` object to a DuckDB database for the input `x`. If
  `NULL` (default), it is resolved from `conn` or extracted from `x`.

- conn_y:

  A `DBIConnection` object to a DuckDB database for the input `y`. If
  `NULL` (default), it is resolved from `conn` or extracted from `y`.

- id_x:

  Character; optional name of the column in `x` whose values will be
  used to name the list elements. If `NULL`, integer row numbers of `x`
  are used.

- id_y:

  Character; optional name of the column in `y` whose values will
  replace the integer indices returned in each element of the list.

- sparse:

  A logical value. If `TRUE`, it returns a sparse index list. If
  `FALSE`, it returns a dense logical matrix.

- distance:

  a numeric value specifying the distance for ST_DWithin. Units
  correspond to the coordinate system of the geometry (e.g. degrees or
  meters)

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

- ...:

  Passed to ddbs_predicate

## Value

A **list** of length equal to the number of rows in `x`.

- Each element contains:

  - **integer vector** of row indices of `y` that satisfy the predicate
    with the corresponding geometry of `x`, or

  - **character vector** if `id_y` is supplied.

- The names of the list elements:

  - are integer row numbers of `x`, or

  - the values of `id_x` if provided.

If there's no match between `x` and `y` it returns `NULL`

## Details

This function provides a unified interface to all spatial predicate
operations in DuckDB's spatial extension. It performs pairwise
comparisons between all geometries in `x` and `y` using the specified
predicate.

### Available Predicates

- **intersects**: Geometries share at least one point

- **covers**: Geometry `x` completely covers geometry `y`

- **touches**: Geometries share a boundary but interiors do not
  intersect

- **disjoint**: Geometries have no points in common

- **within**: Geometry `x` is completely inside geometry `y`

- **dwithin**: Geometry `x` is completely within a distance of geometry
  `y`

- **contains**: Geometry `x` completely contains geometry `y`

- **overlaps**: Geometries share some but not all points

- **crosses**: Geometries have some interior points in common

- **equals**: Geometries are spatially equal

- **covered_by**: Geometry `x` is completely covered by geometry `y`

- **intersects_extent**: Bounding boxes of geometries intersect (faster
  but less precise)

- **contains_properly**: Geometry `x` contains geometry `y` without
  boundary contact

- **within_properly**: Geometry `x` is within geometry `y` without
  boundary contact

If `x` or `y` are not DuckDB tables, they are automatically copied into
a temporary in-memory DuckDB database (unless a connection is supplied
via `conn`).

`id_x` or `id_y` may be used to replace the default integer indices with
the values of an identifier column in `x` or `y`, respectively.

## Examples

``` r
if (FALSE) { # \dontrun{
## Load packages
library(duckspatial)
library(dplyr)

## create in-memory DuckDB database
conn <- ddbs_create_conn(dbdir = "memory")

## read countries data, and rivers
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
) |>
  filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))

rivers_ddbs <- ddbs_open_dataset(
  system.file("spatial/rivers.geojson", 
  package = "duckspatial")
) |>
  ddbs_transform(ddbs_crs(countries_ddbs))

## Store in DuckDB
ddbs_write_vector(conn, countries_ddbs, "countries")
ddbs_write_vector(conn, rivers_ddbs, "rivers")

## Example 1: Check which rivers intersect each country
ddbs_predicate(countries_ddbs, rivers_ddbs, predicate = "intersects")
ddbs_intersects(countries_ddbs, rivers_ddbs)

## Example 2: Find neighboring countries
ddbs_predicate(
  countries_ddbs, 
  countries_ddbs, 
  predicate = "touches",
  id_x = "NAME_ENGL", 
  id_y = "NAME_ENGL"
)

ddbs_touches(
  countries_ddbs, 
  countries_ddbs, 
  id_x = "NAME_ENGL", 
  id_y = "NAME_ENGL"
)

## Example 3: Find rivers that don't intersect countries
ddbs_predicate(
  countries_ddbs, 
  rivers_ddbs, 
  predicate = "disjoint",
  id_x = "NAME_ENGL", 
  id_y = "RIVER_NAME"
)

## Example 4: Use table names inside duckdb
ddbs_predicate("countries", "rivers", predicate = "within", conn, id_x = "NAME_ENGL")
ddbs_within("countries", "rivers", conn,  id_x = "NAME_ENGL")
} # }
```

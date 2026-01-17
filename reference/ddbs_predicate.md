# Spatial predicate operations

Computes spatial relationships between two geometry datasets using
DuckDB's spatial extension. Returns a list where each element
corresponds to a row of `x`, containing the indices (or IDs) of rows in
`y` that satisfy the specified spatial predicate.

## Usage

``` r
ddbs_predicate(
  x,
  y,
  predicate = "intersects",
  conn = NULL,
  id_x = NULL,
  id_y = NULL,
  sparse = TRUE,
  distance = NULL,
  quiet = FALSE
)
```

## Arguments

- x:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- y:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- predicate:

  A geometry predicate function. Defaults to `intersects`, a wrapper of
  `ST_Intersects`. See details for other options.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

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
library(sf)

## create in-memory DuckDB database
conn <- ddbs_create_conn(dbdir = "memory")

## read countries data, and rivers
countries_sf <- read_sf(system.file("spatial/countries.geojson", package = "duckspatial")) |>
  filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))
rivers_sf <- st_read(system.file("spatial/rivers.geojson", package = "duckspatial")) |>
  st_transform(st_crs(countries_sf))

## Store in DuckDB
ddbs_write_vector(conn, countries_sf, "countries")
ddbs_write_vector(conn, rivers_sf, "rivers")

## Example 1: Check which rivers intersect each country
ddbs_predicate(countries_sf, rivers_sf, predicate = "intersects", conn)

## Example 2: Find neighboring countries
ddbs_predicate(countries_sf, countries_sf, predicate = "touches",
               id_x = "NAME_ENGL", id_y = "NAME_ENGL")

## Example 3: Find rivers that don't intersect countries
ddbs_predicate(countries_sf, rivers_sf, predicate = "disjoint",
               id_x = "NAME_ENGL", id_y = "RIVER_NAME")

## Example 4: Use table names inside duckdb
ddbs_predicate("countries", "rivers", predicate = "within", conn, "NAME_ENGL")
} # }
```

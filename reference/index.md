# Package index

## Spatial extension

- [`ddbs_install()`](https://cidree.github.io/duckspatial/reference/ddbs_install.md)
  : Checks and installs the Spatial extension
- [`ddbs_load()`](https://cidree.github.io/duckspatial/reference/ddbs_load.md)
  : Loads the Spatial extension

## Read/Write

- [`ddbs_read_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_read_vector.md)
  : Load spatial vector data from DuckDB into R
- [`ddbs_write_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)
  : Write an SF Object to a DuckDB Database
- [`ddbs_register_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_register_vector.md)
  : Register an SF Object as an Arrow Table in DuckDB

## Spatial Predicates

- [`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  : Spatial predicate operations
- [`ddbs_contains()`](https://cidree.github.io/duckspatial/reference/ddbs_contains.md)
  : Spatial contains predicate
- [`ddbs_contains_properly()`](https://cidree.github.io/duckspatial/reference/ddbs_contains_properly.md)
  : Spatial contains properly predicate
- [`ddbs_covered_by()`](https://cidree.github.io/duckspatial/reference/ddbs_covered_by.md)
  : Spatial covered by predicate
- [`ddbs_covers()`](https://cidree.github.io/duckspatial/reference/ddbs_covers.md)
  : Spatial covers predicate
- [`ddbs_crosses()`](https://cidree.github.io/duckspatial/reference/ddbs_crosses.md)
  : Spatial crosses predicate
- [`ddbs_disjoint()`](https://cidree.github.io/duckspatial/reference/ddbs_disjoint.md)
  : Spatial disjoint predicate
- [`ddbs_equals()`](https://cidree.github.io/duckspatial/reference/ddbs_equals.md)
  : Spatial equals predicate
- [`ddbs_intersects()`](https://cidree.github.io/duckspatial/reference/ddbs_intersects.md)
  : Spatial intersects predicate
- [`ddbs_intersects_extent()`](https://cidree.github.io/duckspatial/reference/ddbs_intersects_extent.md)
  : Spatial intersects extent predicate
- [`ddbs_overlaps()`](https://cidree.github.io/duckspatial/reference/ddbs_overlaps.md)
  : Spatial overlaps predicate
- [`ddbs_touches()`](https://cidree.github.io/duckspatial/reference/ddbs_touches.md)
  : Spatial touches predicate
- [`ddbs_within()`](https://cidree.github.io/duckspatial/reference/ddbs_within.md)
  : Spatial within predicate
- [`ddbs_within_properly()`](https://cidree.github.io/duckspatial/reference/ddbs_within_properly.md)
  : Spatial within properly predicate

## Spatial operations (binary)

- [`ddbs_difference()`](https://cidree.github.io/duckspatial/reference/ddbs_difference.md)
  : Calculates the difference of two geometries
- [`ddbs_filter()`](https://cidree.github.io/duckspatial/reference/ddbs_filter.md)
  : Performs spatial filter of two geometries
- [`ddbs_intersection()`](https://cidree.github.io/duckspatial/reference/ddbs_intersection.md)
  : Calculates the intersection of two geometries
- [`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md)
  : Performs spatial joins of two geometries

## Spatial operations (unary)

- [`ddbs_bbox()`](https://cidree.github.io/duckspatial/reference/ddbs_bbox.md)
  : Returns the minimal bounding box enclosing the input geometry
- [`ddbs_boundary()`](https://cidree.github.io/duckspatial/reference/ddbs_boundary.md)
  : Returns the boundary of geometries
- [`ddbs_buffer()`](https://cidree.github.io/duckspatial/reference/ddbs_buffer.md)
  : Creates a buffer around geometries
- [`ddbs_centroid()`](https://cidree.github.io/duckspatial/reference/ddbs_centroid.md)
  : Calculates the centroid of geometries
- [`ddbs_concave_hull()`](https://cidree.github.io/duckspatial/reference/ddbs_concave_hull.md)
  : Returns the concave hull enclosing the geometry
- [`ddbs_convex_hull()`](https://cidree.github.io/duckspatial/reference/ddbs_convex_hull.md)
  : Returns the convex hull enclosing the geometry
- [`ddbs_is_simple()`](https://cidree.github.io/duckspatial/reference/ddbs_is_simple.md)
  : Check if geometries are simple
- [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_is_valid.md)
  : Check if geometries are valid
- [`ddbs_make_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_make_valid.md)
  : Make invalid geometries valid
- [`ddbs_simplify()`](https://cidree.github.io/duckspatial/reference/ddbs_simplify.md)
  : Simplify geometries

## Spatial operations (measures)

- [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_area.md)
  : Calculates the area of geometries
- [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_length.md)
  : Calculates the length of geometries

## SQL wrappers

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md)
  : Create a duckdb connection
- [`ddbs_stop_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_stop_conn.md)
  : Close a duckdb connection
- [`ddbs_create_schema()`](https://cidree.github.io/duckspatial/reference/ddbs_create_schema.md)
  : Check and create schema
- [`ddbs_crs()`](https://cidree.github.io/duckspatial/reference/ddbs_crs.md)
  : Check CRS of a table
- [`ddbs_drivers()`](https://cidree.github.io/duckspatial/reference/ddbs_drivers.md)
  : Get list of GDAL drivers and file formats
- [`ddbs_glimpse()`](https://cidree.github.io/duckspatial/reference/ddbs_glimpse.md)
  : Check first rows of the data
- [`ddbs_list_tables()`](https://cidree.github.io/duckspatial/reference/ddbs_list_tables.md)
  : Check tables and schemas inside a database

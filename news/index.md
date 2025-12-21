# Changelog

## duckspatial 0.2.0999 dev

### MAJOR CHANGES

- `conn` argument defaults now to NULL. This parameter is not mandatory
  anymore in spatial operations, and it will be handled internally. The
  argument has been moved after `x` and `y` arguments.

- [`ddbs_filter()`](https://cidree.github.io/duckspatial/reference/ddbs_filter.md):
  uses `intersects` for `ST_Intersects` instead of `intersection`.

- Allow the use of either `sf` object or a DuckDB table as input/output
  in every operation.

- Functions that use `x` and `y` arguments, can indistinctively use
  `sf`, DuckDB table name, or mixed.

### NEW FEATURES

- Affine functions:
  [`ddbs_rotate()`](https://cidree.github.io/duckspatial/reference/ddbs_rotate.md),
  [`ddbs_rotate_3d()`](https://cidree.github.io/duckspatial/reference/ddbs_rotate_3d.md),
  [`ddbs_shift()`](https://cidree.github.io/duckspatial/reference/ddbs_shift.md),
  [`ddbs_flip()`](https://cidree.github.io/duckspatial/reference/ddbs_flip.md),
  [`ddbs_scale()`](https://cidree.github.io/duckspatial/reference/ddbs_scale.md),
  and
  [`ddbs_shear()`](https://cidree.github.io/duckspatial/reference/ddbs_shear.md).

- [`ddbs_boundary()`](https://cidree.github.io/duckspatial/reference/ddbs_boundary.md):
  returns the boundary of geometries.

- [`ddbs_combine()`](https://cidree.github.io/duckspatial/reference/ddbs_combine.md):
  combines geometries into a multi-geometry

- [`ddbs_concave_hull()`](https://cidree.github.io/duckspatial/reference/ddbs_concave_hull.md):
  new function to create the concave hull enclosing a geometry.

- [`ddbs_convex_hull()`](https://cidree.github.io/duckspatial/reference/ddbs_convex_hull.md):
  new function to create the convex hull enclosing a geometry.

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md):
  new convenient function to create a DuckDB connection.

- [`ddbs_drivers()`](https://cidree.github.io/duckspatial/reference/ddbs_drivers.md):
  get list of GDAL drivers and file formats supported by DuckDB spatial
  extension.

- [`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md):
  new function to perform spatial join operations.

- [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_length.md):
  adds a new column with the length of the geometries

- [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_area.md):
  adds a new column with the area of the geometries

- [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_is_valid.md):
  adds a new logical column asserting the simplicity of the geometries

- [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_is_valid.md):
  adds a new logical column asserting the validity of the geometries

- [`ddbs_make_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_make_valid.md):
  makes the geometries valid

- [`ddbs_simplify()`](https://cidree.github.io/duckspatial/reference/ddbs_simplify.md):
  makes the geometries simple

- [`ddbs_bbox()`](https://cidree.github.io/duckspatial/reference/ddbs_bbox.md):
  calculates the bounding box

- [`ddbs_union()`](https://cidree.github.io/duckspatial/reference/ddbs_union.md):
  union of geometries

- **Spatial predicates**: spatial predicates are all included in a
  function called
  [`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md),
  where the user can specify the spatial predicate. Another option, it’s
  to use the spatial predicate function, such as
  [`ddbs_intersects()`](https://cidree.github.io/duckspatial/reference/ddbs_intersects.md),
  [`ddbs_crosses()`](https://cidree.github.io/duckspatial/reference/ddbs_crosses.md),
  [`ddbs_touches()`](https://cidree.github.io/duckspatial/reference/ddbs_touches.md),
  etc.

### MINOR CHANGES

- All functions now have a parameter `quiet` that allows users to
  suppress informational messages. Closed
  [\#3](https://github.com/Cidree/duckspatial/issues/3)

## duckspatial 0.2.0

CRAN release: 2025-04-29

### NEW FEATURES

- [`ddbs_read_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_read_vector.md):
  gains a new argument `clauses` to modify the query from the table
  (e.g. “WHERE …”, “ORDER BY…”)

### NEW FUNCTIONS

- [`ddbs_list_tables()`](https://cidree.github.io/duckspatial/reference/ddbs_list_tables.md):
  lists table schemas and tables inside the database

- [`ddbs_glimpse()`](https://cidree.github.io/duckspatial/reference/ddbs_glimpse.md):
  check first rows of a table

- [`ddbs_buffer()`](https://cidree.github.io/duckspatial/reference/ddbs_buffer.md):
  calculates the buffer around the input geometry

- [`ddbs_centroid()`](https://cidree.github.io/duckspatial/reference/ddbs_centroid.md):
  calculates the centroid of the input geometry

- [`ddbs_difference()`](https://cidree.github.io/duckspatial/reference/ddbs_difference.md):
  calculates the geometric difference between two objects

### IMPROVEMENTS

- [`ddbs_intersection()`](https://cidree.github.io/duckspatial/reference/ddbs_intersection.md):
  overwrite argument defaults to `FALSE` instead of `NULL`

- Better schemas management. Added support for all functions.

## duckspatial 0.1.0

CRAN release: 2025-04-19

- Initial CRAN submission.

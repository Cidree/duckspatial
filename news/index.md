# Changelog

## duckspatial 0.9.0

### MAJOR CHANGES

- `conn` argument defaults now to `NULL`. This parameter is not
  mandatory anymore in spatial operations, and it will be handled
  internally. The argument has been moved after `x`, `y`, and
  function-mandatory arguments
  ([\#9](https://github.com/Cidree/duckspatial/issues/9)).

- [`ddbs_write_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)
  allows to create a temporary view with the argument `temp = TRUE`,
  which is much faster than creating a table
  ([\#14](https://github.com/Cidree/duckspatial/issues/14)).

- [`ddbs_read_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_read_vector.md)
  uses internal optimizations with `geoarrow` making it much faster
  ([\#15](https://github.com/Cidree/duckspatial/issues/15)).

- The spatial functions allow now to have either an `sf` or a DuckDB
  table as input (`x`) and/or output (`name = NULL` or `name != NULL`)
  ([\#19](https://github.com/Cidree/duckspatial/issues/19)).

- The `crs` and `crs_column` arguments are deprecated and will be
  removed in `duckspatial` v1.0.0. This change aligns with planned
  native CRS support in DuckDB, scheduled for v1.5.0 (expected
  February 2025)
  ([\#7](https://github.com/Cidree/duckspatial/issues/7)).

### NEW FEATURES

- Affine functions:
  [`ddbs_rotate()`](https://cidree.github.io/duckspatial/reference/ddbs_rotate.md),
  [`ddbs_rotate_3d()`](https://cidree.github.io/duckspatial/reference/ddbs_rotate_3d.md),
  [`ddbs_shift()`](https://cidree.github.io/duckspatial/reference/ddbs_shift.md),
  [`ddbs_flip()`](https://cidree.github.io/duckspatial/reference/ddbs_flip.md),
  [`ddbs_scale()`](https://cidree.github.io/duckspatial/reference/ddbs_scale.md),
  and
  [`ddbs_shear()`](https://cidree.github.io/duckspatial/reference/ddbs_shear.md)
  ([\#37](https://github.com/Cidree/duckspatial/issues/37)).

- [`ddbs_boundary()`](https://cidree.github.io/duckspatial/reference/ddbs_boundary.md):
  returns the boundary of geometries
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_concave_hull()`](https://cidree.github.io/duckspatial/reference/ddbs_concave_hull.md):
  new function to create the concave hull enclosing a geometry
  ([\#23](https://github.com/Cidree/duckspatial/issues/23)).

- [`ddbs_convex_hull()`](https://cidree.github.io/duckspatial/reference/ddbs_convex_hull.md):
  new function to create the convex hull enclosing a geometry
  ([\#23](https://github.com/Cidree/duckspatial/issues/23)).

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md):
  new convenient function to create a DuckDB connection with spatial
  extension installed and loaded.

- [`ddbs_drivers()`](https://cidree.github.io/duckspatial/reference/ddbs_drivers.md):
  get list of GDAL drivers and file formats supported by DuckDB spatial
  extension.

- [`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md):
  new function to perform spatial join operations
  ([\#6](https://github.com/Cidree/duckspatial/issues/6)).

- [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_length.md):
  adds a new column with the length of the geometries
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_area.md):
  adds a new column with the area of the geometries
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_distance()`](https://cidree.github.io/duckspatial/reference/ddbs_distance.md):
  calculates the distance between two geometries
  ([\#34](https://github.com/Cidree/duckspatial/issues/34)).

- [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_is_valid.md):
  adds a new logical column asserting the simplicity of the geometries
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_is_valid.md):
  adds a new logical column asserting the validity of the geometries
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_make_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_make_valid.md):
  makes the geometries valid
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_simplify()`](https://cidree.github.io/duckspatial/reference/ddbs_simplify.md):
  makes the geometries simple
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_bbox()`](https://cidree.github.io/duckspatial/reference/ddbs_bbox.md):
  calculates the bounding box
  ([\#25](https://github.com/Cidree/duckspatial/issues/25)).

- [`ddbs_envelope()`](https://cidree.github.io/duckspatial/reference/ddbs_envelope.md):
  returns the envelope of the geometries
  ([\#36](https://github.com/Cidree/duckspatial/issues/36)).

- [`ddbs_union()`](https://cidree.github.io/duckspatial/reference/ddbs_union.md):
  union of geometries
  ([\#36](https://github.com/Cidree/duckspatial/issues/36)).

- [`ddbs_combine()`](https://cidree.github.io/duckspatial/reference/ddbs_combine.md):
  combines geometries into a multi-geometry
  ([\#36](https://github.com/Cidree/duckspatial/issues/36)).

- [`ddbs_quadkey()`](https://cidree.github.io/duckspatial/reference/ddbs_quadkey.md):
  calculates quadkey tiles from point geometries
  ([\#52](https://github.com/Cidree/duckspatial/issues/52)).

- [`ddbs_exterior_ring()`](https://cidree.github.io/duckspatial/reference/ddbs_exterior_ring.md):
  returns the exterior ring (shell) of a polygon geometry
  ([\#45](https://github.com/Cidree/duckspatial/issues/45)).

- [`ddbs_make_polygon()`](https://cidree.github.io/duckspatial/reference/ddbs_make_polygon.md):
  create a POLYGON from a LINESTRING shell
  ([\#46](https://github.com/Cidree/duckspatial/issues/46)).

- [`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md):
  spatial predicates between two geometries
  ([\#28](https://github.com/Cidree/duckspatial/issues/28)).

- [`ddbs_intersects()`](https://cidree.github.io/duckspatial/reference/ddbs_intersects.md),
  [`ddbs_crosses()`](https://cidree.github.io/duckspatial/reference/ddbs_crosses.md),
  [`ddbs_touches()`](https://cidree.github.io/duckspatial/reference/ddbs_touches.md),
  …: shortcuts for e.g.: `ddbs_predicate(predicate = "intersects")`
  ([\#28](https://github.com/Cidree/duckspatial/issues/28)).

- [`ddbs_transform()`](https://cidree.github.io/duckspatial/reference/ddbs_transform.md):
  transforms from one coordinates reference system to another
  ([\#43](https://github.com/Cidree/duckspatial/issues/43)).

- [`ddbs_as_text()`](https://cidree.github.io/duckspatial/reference/ddbs_as_text.md):
  converts geometries to well-known text (WKT) format
  ([\#47](https://github.com/Cidree/duckspatial/issues/47)).

- [`ddbs_as_wkb()`](https://cidree.github.io/duckspatial/reference/ddbs_as_wkb.md):
  converts geometries to well-known binary (WKB) format
  ([\#48](https://github.com/Cidree/duckspatial/issues/48)).

- [`ddbs_generate_points()`](https://cidree.github.io/duckspatial/reference/ddbs_generate_points.md):
  generates random points within the bounding box of `x`
  ([\#54](https://github.com/Cidree/duckspatial/issues/54)).

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
  suppress messages
  ([\#3](https://github.com/Cidree/duckspatial/issues/3)).

- Spatial operations now don’t fail when a column has a dot
  ([\#33](https://github.com/Cidree/duckspatial/issues/33)).

- Added some vignettes
  ([\#42](https://github.com/Cidree/duckspatial/issues/42)).

- [`ddbs_filter()`](https://cidree.github.io/duckspatial/reference/ddbs_filter.md):
  uses `intersects` for `ST_Intersects` instead of `intersection`.

- [`ddbs_filter()`](https://cidree.github.io/duckspatial/reference/ddbs_filter.md):
  doesn’t return duplicated observations when the same geometry fulfills
  the spatial predicate in more than one geometries of `y`
  ([\#50](https://github.com/Cidree/duckspatial/issues/50)).

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

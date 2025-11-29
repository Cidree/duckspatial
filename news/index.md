# Changelog

## duckspatial 0.2.0999 dev

### MAJOR CHANGES

- `conn` argument defaults now to NULL. This parameter is not mandatory
  anymore in spatial operations, and it will be handled internally. The
  argument has been moved after `x` and `y` arguments.

- [`ddbs_filter()`](https://cidree.github.io/duckspatial/reference/ddbs_filter.md):
  uses `intersects` for `ST_Intersects` instead of `intersection`.

### NEW FEATURES

- [`ddbs_boundary()`](https://cidree.github.io/duckspatial/reference/ddbs_boundary.md):
  returns the boundary of geometries.

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md):
  new convenient function to create a DuckDB connection.

- [`ddbs_drivers()`](https://cidree.github.io/duckspatial/reference/ddbs_drivers.md):
  get list of GDAL drivers and file formats supported by DuckDB spatial
  extension.

- [`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md):
  new function to perform spatial join operations.

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

# duckspatial 0.2.0999 dev

## MAJOR CHANGES

* `conn` argument defaults now to NULL. This parameter is not mandatory anymore in spatial operations, and it will be handled internally. The argument has been moved after `x` and `y` arguments.

* `ddbs_filter()`: uses `intersects` for `ST_Intersects` instead of `intersection`.

## NEW FEATURES

* `ddbs_boundary()`: returns the boundary of geometries.

* `ddbs_concave_hull()`: new function to create the concave hull enclosing a geometry.

* `ddbs_convex_hull()`: new function to create the convex hull enclosing a geometry.

* `ddbs_create_conn()`: new convenient function to create a DuckDB connection.

* `ddbs_drivers()`: get list of GDAL drivers and file formats supported by DuckDB spatial extension.

* `ddbs_join()`: new function to perform spatial join operations.




## MINOR CHANGES

* All functions now have a parameter `quiet` that allows users to suppress informational messages. Closed [#3](https://github.com/Cidree/duckspatial/issues/3)


# duckspatial 0.2.0

## NEW FEATURES

* `ddbs_read_vector()`: gains a new argument `clauses` to modify the query from the table (e.g. "WHERE ...", "ORDER BY...")

## NEW FUNCTIONS

* `ddbs_list_tables()`: lists table schemas and tables inside the database

* `ddbs_glimpse()`: check first rows of a table

* `ddbs_buffer()`: calculates the buffer around the input geometry

* `ddbs_centroid()`: calculates the centroid of the input geometry

* `ddbs_difference()`: calculates the geometric difference between two objects


## IMPROVEMENTS

* `ddbs_intersection()`: overwrite argument defaults to `FALSE` instead of `NULL`

* Better schemas management. Added support for all functions.

# duckspatial 0.1.0

* Initial CRAN submission.

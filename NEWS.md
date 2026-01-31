# duckspatial (development version)

## MAJOR CHANGES

* `duckspatial_df` becomes the main class of `duckspatial`. It represents a lazy, table-like object whose data is not loaded into memory until explicitly materialized (with `ddbs_collect()` or `st_as_sf()`). Every function now accepts this class as input, and it's the returned class by default. If the user is interested in returning a different class, there's a convenient `output` argument (#55, #63).

* `ddbs_union()`: is spplited into two new functions depending on the desired behavior: `ddbs_union()` and `ddbs_union_agg()` (#77).

* `ddbs_length`, `ddbs_area` and `ddbs_distance`: now use by default the best DuckDB function (e.g. `ST_Area` or `ST_Area_Spheroid`) depending on the input's CRS. Additionally, they return an `units` object (#80, #82).

## NEW FEATURES

* `ddbs_as_spatial()`: converts a table with coordinates into a spatial object (#75).

* `ddbs_geometry_type()`: returns the geometry type of an object (#76).

* `ddbs_as_geojson()`: converts the geometry to geojson format (#84).

## MINOR CHANGES

* `ddbs_buffer()`: now has four new arguments: `num_triangles`, `cap_style`, `join_style`, and `mitre_limit` (#72).

* `ddbs_quadkey()`: can aggregate by `field` when output is `polygon` and `tilexy` (#78).

## BUG FIXES

* `ddbs_length`, `ddbs_area` and `ddbs_distance` were calculating the wrong measure when the CRS was geographic (#82).


# duckspatial 0.9.0

Learn more about this version [here](https://adrian-cidre.com/posts/014_duckspatial/).

## MAJOR CHANGES

* `conn` argument defaults now to `NULL`. This parameter is not mandatory anymore in spatial operations, and it will be handled internally. The argument has been moved after `x`, `y`, and function-mandatory arguments (#9).

* `ddbs_write_vector()` allows to create a temporary view with the argument `temp = TRUE`, which is much faster than creating a table (#14).

* `ddbs_read_vector()` uses internal optimizations with `geoarrow` making it much faster (#15).

* The spatial functions allow now to have either an `sf` or a DuckDB table as input (`x`) and/or output (`name = NULL` or `name != NULL`) (#19).

* The `crs` and `crs_column` arguments are deprecated and will be removed in `duckspatial` v1.0.0. This change aligns with planned native CRS support in DuckDB, scheduled for v1.5.0 (expected February 2025) (#7).


## NEW FEATURES

* Affine functions: `ddbs_rotate()`, `ddbs_rotate_3d()`, `ddbs_shift()`, `ddbs_flip()`, `ddbs_scale()`, and `ddbs_shear()` (#37).

* `ddbs_boundary()`: returns the boundary of geometries (#17).

* `ddbs_concave_hull()`: new function to create the concave hull enclosing a geometry (#23).

* `ddbs_convex_hull()`: new function to create the convex hull enclosing a geometry (#23).

* `ddbs_create_conn()`: new convenient function to create a DuckDB connection with spatial extension installed and loaded.

* `ddbs_drivers()`: get list of GDAL drivers and file formats supported by DuckDB spatial extension.

* `ddbs_join()`: new function to perform spatial join operations (#6).

* `ddbs_length()`: adds a new column with the length of the geometries (#17).

* `ddbs_area()`: adds a new column with the area of the geometries (#17).

* `ddbs_distance()`: calculates the distance between two geometries (#34).

* `ddbs_is_valid()`: adds a new logical column asserting the simplicity of the geometries (#17).

* `ddbs_is_valid()`: adds a new logical column asserting the validity of the geometries (#17).

* `ddbs_make_valid()`: makes the geometries valid (#17).

* `ddbs_simplify()`: makes the geometries simple (#17).

* `ddbs_bbox()`: calculates the bounding box (#25).

* `ddbs_envelope()`: returns the envelope of the geometries (#36).

* `ddbs_union()`: union of geometries (#36).

* `ddbs_combine()`: combines geometries into a multi-geometry (#36).

* `ddbs_quadkey()`: calculates quadkey tiles from point geometries (#52).

* `ddbs_exterior_ring()`: returns the exterior ring (shell) of a polygon geometry (#45).

* `ddbs_make_polygon()`: create a POLYGON from a LINESTRING shell (#46).

* `ddbs_predicate()`: spatial predicates between two geometries (#28).

* `ddbs_intersects()`, `ddbs_crosses()`, `ddbs_touches()`, ...: shortcuts for e.g.: `ddbs_predicate(predicate = "intersects")` (#28).

* `ddbs_transform()`: transforms from one coordinates reference system to another (#43).

* `ddbs_as_text()`: converts geometries to well-known text (WKT) format (#47).

* `ddbs_as_wkb()`: converts geometries to well-known binary (WKB) format (#48).

* `ddbs_generate_points()`: generates random points within the bounding box of `x` (#54).

* **Spatial predicates**: spatial predicates are all included in a function called `ddbs_predicate()`, where the user can specify the spatial predicate. Another option, it's to use the spatial predicate function, such as `ddbs_intersects()`, `ddbs_crosses()`, `ddbs_touches()`, etc.

## MINOR CHANGES

* All functions now have a parameter `quiet` that allows users to suppress messages (#3).

* Spatial operations now don't fail when a column has a dot (#33).

* Added some vignettes (#42).

* `ddbs_filter()`: uses `intersects` for `ST_Intersects` instead of `intersection`.

* `ddbs_filter()`: doesn't return duplicated observations when the same geometry fulfills the spatial predicate in more than one geometries of `y` (#50).




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

# duckspatial 1.1.2

## NEW FEATURES

* `ddbs_shortest_line()`: returns the LINESTRING connecting the closest points between each pair of geometries from `x` and `y`.

* `ddbs_azimuth()`: computes the clockwise azimuth (bearing from north) between two sets of POINT geometries. Returns a numeric matrix (`mode = "sf"`) or a lazy tbl with all pairs (default). Supports radians (default) and degrees via the `unit` argument.

* `ddbs_vertices()`: collects all vertices of a geometry into a MULTIPOINT.

* `ddbs_point()`: creates POINT geometries from numeric coordinate vectors. Supports 2D, 3D (Z), and 4D (Z + M) coordinates, extra attribute columns via `...`, and CRS assignment.

* `ddbs_xmax()`, `ddbs_xmin()`, `ddbs_ymax()`, `ddbs_ymin()`, `ddbs_zmax()`, `ddbs_zmin()`, `ddbs_mmax()`, `ddbs_mmin()`: return the maximum or minimum coordinate value for each geometry (`by_feature = TRUE`) or the global extreme across the dataset (`by_feature = FALSE`).

* `ddbs_dimension()`: returns the topological dimension of each geometry (0 = point, 1 = line, 2 = polygon, -1 = empty).

* `ddbs_line_locate_point()`: returns the fractional position (0–1) of the closest point on a linestring to a reference point. The `y` argument accepts an `sf` object, a `duckspatial_df`, or a character DuckDB table name (each must contain exactly 1 point feature).

## ENHANCEMENTS

* `ddbs_union_agg()`: gains a `mem` argument. Set `mem = TRUE` to use `ST_MemUnion_Agg()` instead of `ST_Union_Agg()` — slower but more memory efficient.

# duckspatial 1.1.1

## NEW FEATURES

* `ddbs_get_npoints()`: returns the number of points (vertices) in a geometry.

* `ddbs_get_ngeometries()`: returns the number of sub-geometries in a GEOMETRYCOLLECTION or MULTI* geometry.

* `ddbs_affine()`: applies an affine transformation to geometries using a 2x3 or 3x4 matrix (#133).

## ENHANCEMENTS

* `ddbs_create_conn()` and `ddbs_write_dataset()` gain a `duckdb_storage_version` argument to control DuckDB storage compatibility. They now default to DuckDB `v1.5.0` storage (**Native Spatial Storage**) so that CRS metadata can persist in native `GEOMETRY` columns. Users can specify older versions (e.g., `v1.0.0` for **Legacy Compatibility**) when the output must be readable by older DuckDB clients. For more details on DuckDB storage versions, see <https://duckdb.org/docs/internals/storage> (#130, #132).

* `ddbs_create_conn()`: stricter validation of `dbdir` parameter. Now only accepts `"memory"`, `"tempdir"`, or file paths with `.duckdb`, `.db`, or `.ddb` extensions (#132).

* `ddbs_stop_conn()`: now explicitly shuts down the DuckDB driver and forces a checkpoint (necessary to release the file lock on Windows) (#132).

* `dplyr` methods on `duckspatial_df` now return a lazy temporary view, rather than creating a new temporary table (#130, #134).


# duckspatial 1.1.0

## NEW FEATURES

* Implementation of `duckspatial` macros: this allows to use some `duckspatial` functions within `dplyr` verbs (e.g. `data |> mutate(area = ddbs_area(geometry))`) (#92).

* `ddbs_dump()`: decompose multi-geometry types into individual single geometry components (#44, 117).

* `ddbs_maximum_inscribed_circle()`: returns the maximum inscribed circle of the input geometry (#117).

* `ddbs_minimum_rotated_rectangle()`: returns the minimum rotated rectangle that bounds the input geometry (#117).

* `ddbs_set_crs()`: assigns the CRS to a spatial object. No transformation is applied to the geometries (#118).

* `ddbs_crop()`: similar to `ddbs_intersection()`, but it crops to the bounding box (#118).

* `ddbs_line_interpolate()`: interpolates a point or points along a line geometry (#118).

* `ddbs_line_substring()`: gets a fraction of a linestring (#118).

* `ddbs_line_merge()`:merges connected multistrings (#118).

* `ddbs_z()` and `ddbs_m()`: to extract Z and M coordinates as a new column (#118).

* `ddbs_make_envelope()`: creates a rectangular polygon from 4 coordinates (#118).

* `ddbs_locate_between()`: locates points that fall with the specified M range (#118).

* `ddbs_locate_along()`: locates points that match the specified M value (#118).

* `ddbs_remove_repeated_points()`: removes repeated points, optionally with some tolerance (#118).

* `ddbs_read_meta()`: reads the metadata of a vectorial data file (#118).

* `ddbs_make_line()`: creates LINESTRINGS from POINT geometries (#126).

## ENHANCEMENTS

* `group_by` and `summarise` methods now drop the spatial attributes when the output is not a `duckspatial_df` anymore (#119).

* `ddbs_create_conn()`: gains the `upgrade` argument that is passed to `ddbs_install()`.

* `ddbs_install()`: now returns a better error message if the extension is already loaded, and there's an attempt to upgrade it.

* `ddbs_centroid()`: gains the argument `method` to implement ST_PointOnSurface (#118).

* `ddbs_as_points()` allows to create a `duckspatial_df` from raw coordinate or WKT columns. It also gains two new arguments: `remove` and `na.fail` (#125).

* `ddbs_open_dataset()`: can open geoparquet files when the geometry is encoded as WKB geoparquet. It also fails with a better error message when the geometry is encoded as a native arrow/geoarrow encoding (#129).

## BUG FIXES

* Large datasets couldn't be processed because an `arrow` code limitation in `ddbs_register_table()` (#124).


# duckspatial 1.0.0

Learn more about this version [here](https://adrian-cidre.com/posts/015_duckspatial_v100/).

## MAJOR CHANGES

-   `duckspatial_df` becomes the main class of `duckspatial`. It represents a lazy, table-like object whose data is not loaded into memory until explicitly materialized (with `ddbs_collect()` or `st_as_sf()`). Every function now accepts this class as input, and it's the returned class by default. If the user wants to materialize the result in the same way `sf` would do, that can be done with `mode = "sf"` (#55, #63).

-   `ddbs_buffer()`: now has four new arguments: `num_triangles`, `cap_style`, `join_style`, and `mitre_limit` (#72).

-   `ddbs_union()`: is split into two new functions depending on the desired behavior: `ddbs_union()` and `ddbs_union_agg()` (#77).

-   `ddbs_length()`, `ddbs_area()` and `ddbs_distance()`: now use by default the best DuckDB function (e.g. `ST_Area()` or `ST_Area_Spheroid()`) depending on the input's CRS. They also return a `duckspatial_df` object by default rather than a materialized vector. In the case of `ddbs_distance()`, it returns a `tbl_duckdb_connection` (#80, #82, #103).

-   `ddbs_simplify()`: tolerance defaults to 0; gains a new argument `preserve_topology` specified before `conn` (#86).

-   `ddbs_is_simple()`, `ddbs_is_valid()`, `ddbs_area()`, `ddbs_length()`, `ddbs_distance()`: the `new_column` argument now defaults to a column name, as we now encourage the users to keep most of the work inside DuckDB, rather than materialize the result. For materializing a vector in R, use `mode = "sf"`. This argument is also moved before `conn` argument (#83).

-    `ddbs_predicate()` and colleagues: they gain new arguments: name, mode, overwrite, and quiet. When `mode = "duckspatial"`, they return a lazy tbl backed by DuckDB. When `mode = "sf"`, they return a list/matrix (#105).

## NEW FEATURES

-   `ddbs_as_points()`: converts a table with coordinates into a spatial object (#75).

-   `ddbs_geometry_type()`: returns the geometry type of an object (#76).

-   `ddbs_as_geojson()`: converts the geometry to geojson format (#84).

-   `ddbs_perimeter()`: calculates the perimeter of polygons (#89).

-   New geometry validation/check functions: `ddbs_is_empty()`, `ddbs_is_ring()` and `ddbs_is_closed()` (#91).

-   `ddbs_sym_difference()`: performs symmetric difference between pairs of geometries (#91).

-   `ddbs_force_2d()`, `ddbs_force_3d()`, `ddbs_force_4d()`: force the geometries to have specfic dimensions (#91).

-   `ddbs_has_z()` and `ddbs_has_m()`: check if the geometry has the dimension (#91).

-   `ddbs_polygonize()`, `ddbs_build_area()`: generates polygons from lines (#91).

-   `ddbs_voronoi()`: generates Voronoi diagrams from point geometries (#91).

-   `ddbs_endpoint()` and `ddbs_start_point()`: extracts the start/end point of a linestring geometry (#91).

-   `ddbs_flip_coordinates()`: swaps X and Y coordinates (#91).

-   `ddbs_register_vector()`, `ddbs_write_vector()` and `ddbs_read_vector()` deprecated in favour of `ddbs_register_table()`, `ddbs_write_table()` and `ddbs_read_table()` (#100).

-   `ddbs_x()` and `ddbs_y()`: extract the `x` and `y` coordinates of points (#108).

-   `ddbs_drop_geometry()`: drops the geometry column of a `duckspatial_df` object.

-   `ddbs_options()`: to set some `duckspatial` default options.

-   `ddbs_join()`: dwithin is now implemented for spatial join.

## MINOR CHANGES

-   Improve the documentation of the functions (#85).

-   `ddbs_buffer()`: warns if the input CRS is not a projected CRS, as the distance uses its units.

-   `ddbs_quadkey()`: can aggregate by `field` when output is `polygon` and `tilexy` (#78).

-   `ddbs_crs()`: accepts CRS codes and `crs` objects as inputs. It returns `NULL` when the input doesn't have a geometry (e.g. a `data.frame`) (#87).

-   `ddbs_create_conn()`: now has ... that are paseed to `dbConnect()` for extra configuration.

## BUG FIXES

-   `ddbs_length()`, `ddbs_area()` and `ddbs_distance()` were calculating the wrong measure when the CRS was geographic (#82).

-   `ddbs_filter(predicate = "dwithin")` and `ddbs_is_within_distance` were calculating wrong distances for geographic CRS (#88).



# duckspatial 0.9.0

Learn more about this version [here](https://adrian-cidre.com/posts/014_duckspatial/).

## MAJOR CHANGES

-   `conn` argument defaults now to `NULL`. This parameter is not mandatory anymore in spatial operations, and it will be handled internally. The argument has been moved after `x`, `y`, and function-mandatory arguments (#9).

-   `ddbs_write_vector()` allows to create a temporary view with the argument `temp = TRUE`, which is much faster than creating a table (#14).

-   `ddbs_read_vector()` uses internal optimizations with `geoarrow` making it much faster (#15).

-   The spatial functions allow now to have either an `sf` or a DuckDB table as input (`x`) and/or output (`name = NULL` or `name != NULL`) (#19).

-   The `crs` and `crs_column` arguments are deprecated and will be removed in `duckspatial` v1.0.0. This change aligns with planned native CRS support in DuckDB, scheduled for v1.5.0 (expected February 2025) (#7).

## NEW FEATURES

-   Affine functions: `ddbs_rotate()`, `ddbs_rotate_3d()`, `ddbs_shift()`, `ddbs_flip()`, `ddbs_scale()`, and `ddbs_shear()` (#37).

-   `ddbs_boundary()`: returns the boundary of geometries (#17).

-   `ddbs_concave_hull()`: new function to create the concave hull enclosing a geometry (#23).

-   `ddbs_convex_hull()`: new function to create the convex hull enclosing a geometry (#23).

-   `ddbs_create_conn()`: new convenient function to create a DuckDB connection with spatial extension installed and loaded.

-   `ddbs_drivers()`: get list of GDAL drivers and file formats supported by DuckDB spatial extension.

-   `ddbs_join()`: new function to perform spatial join operations (#6).

-   `ddbs_length()`: adds a new column with the length of the geometries (#17).

-   `ddbs_area()`: adds a new column with the area of the geometries (#17).

-   `ddbs_distance()`: calculates the distance between two geometries (#34).

-   `ddbs_is_valid()`: adds a new logical column asserting the simplicity of the geometries (#17).

-   `ddbs_is_valid()`: adds a new logical column asserting the validity of the geometries (#17).

-   `ddbs_make_valid()`: makes the geometries valid (#17).

-   `ddbs_simplify()`: makes the geometries simple (#17).

-   `ddbs_bbox()`: calculates the bounding box (#25).

-   `ddbs_envelope()`: returns the envelope of the geometries (#36).

-   `ddbs_union()`: union of geometries (#36).

-   `ddbs_combine()`: combines geometries into a multi-geometry (#36).

-   `ddbs_quadkey()`: calculates quadkey tiles from point geometries (#52).

-   `ddbs_exterior_ring()`: returns the exterior ring (shell) of a polygon geometry (#45).

-   `ddbs_make_polygon()`: create a POLYGON from a LINESTRING shell (#46).

-   `ddbs_predicate()`: spatial predicates between two geometries (#28).

-   `ddbs_intersects()`, `ddbs_crosses()`, `ddbs_touches()`, ...: shortcuts for e.g.: `ddbs_predicate(predicate = "intersects")` (#28).

-   `ddbs_transform()`: transforms from one coordinates reference system to another (#43).

-   `ddbs_as_text()`: converts geometries to well-known text (WKT) format (#47).

-   `ddbs_as_wkb()`: converts geometries to well-known binary (WKB) format (#48).

-   `ddbs_generate_points()`: generates random points within the bounding box of `x` (#54).

-   **Spatial predicates**: spatial predicates are all included in a function called `ddbs_predicate()`, where the user can specify the spatial predicate. Another option, it's to use the spatial predicate function, such as `ddbs_intersects()`, `ddbs_crosses()`, `ddbs_touches()`, etc.

## MINOR CHANGES

-   All functions now have a parameter `quiet` that allows users to suppress messages (#3).

-   Spatial operations now don't fail when a column has a dot (#33).

-   Added some vignettes (#42).

-   `ddbs_filter()`: uses `intersects` for `ST_Intersects` instead of `intersection`.

-   `ddbs_filter()`: doesn't return duplicated observations when the same geometry fulfills the spatial predicate in more than one geometries of `y` (#50).

# duckspatial 0.2.0

## NEW FEATURES

-   `ddbs_read_vector()`: gains a new argument `clauses` to modify the query from the table (e.g. "WHERE ...", "ORDER BY...")

## NEW FUNCTIONS

-   `ddbs_list_tables()`: lists table schemas and tables inside the database

-   `ddbs_glimpse()`: check first rows of a table

-   `ddbs_buffer()`: calculates the buffer around the input geometry

-   `ddbs_centroid()`: calculates the centroid of the input geometry

-   `ddbs_difference()`: calculates the geometric difference between two objects

## IMPROVEMENTS

-   `ddbs_intersection()`: overwrite argument defaults to `FALSE` instead of `NULL`

-   Better schemas management. Added support for all functions.

# duckspatial 0.1.0

-   Initial CRAN submission.

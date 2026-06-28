# Package index

## Setup and Connection

Install the spatial extension and manage DuckDB connections

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md)
  : Create a DuckDB connection with spatial extension
- [`ddbs_stop_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_stop_conn.md)
  : Close a DuckDB connection
- [`ddbs_install()`](https://cidree.github.io/duckspatial/reference/ddbs_install.md)
  : Checks and installs the Spatial extension
- [`ddbs_load()`](https://cidree.github.io/duckspatial/reference/ddbs_load.md)
  : Loads the Spatial extension
- [`ddbs_extension_info()`](https://cidree.github.io/duckspatial/reference/ddbs_extension_info.md)
  : Glimpse the status of a DuckDB extension
- [`ddbs_options()`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)
  : Get or set global duckspatial options
- [`ddbs_set_resources()`](https://cidree.github.io/duckspatial/reference/ddbs_set_resources.md)
  [`ddbs_get_resources()`](https://cidree.github.io/duckspatial/reference/ddbs_set_resources.md)
  : Get or set connection resources
- [`ddbs_sitrep()`](https://cidree.github.io/duckspatial/reference/ddbs_sitrep.md)
  : Report duckspatial configuration status

## Data Import / Export

Read and write spatial data to and from DuckDB

- [`ddbs_open_dataset()`](https://cidree.github.io/duckspatial/reference/ddbs_open_dataset.md)
  : Open spatial dataset lazily via DuckDB
- [`ddbs_read_table()`](https://cidree.github.io/duckspatial/reference/ddbs_read_table.md)
  : Reads a vectorial table from DuckDB into R
- [`ddbs_write_dataset()`](https://cidree.github.io/duckspatial/reference/ddbs_write_dataset.md)
  : Write spatial dataset to disk
- [`ddbs_write_table()`](https://cidree.github.io/duckspatial/reference/ddbs_write_table.md)
  : Write an SF Object to a DuckDB Database
- [`ddbs_register_table()`](https://cidree.github.io/duckspatial/reference/ddbs_register_table.md)
  : Register an SF Object as an Arrow Table in DuckDB
- [`ddbs_read_meta()`](https://cidree.github.io/duckspatial/reference/ddbs_read_meta.md)
  : Read metadata from a spatial file

## Spatial Predicates

Test spatial relationships between geometries

- [`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_intersects()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_covers()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_touches()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_is_within_distance()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_disjoint()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_within()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_contains()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_overlaps()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_crosses()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_equals()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_covered_by()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_intersects_extent()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_contains_properly()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  [`ddbs_within_properly()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  : Evaluate spatial predicates between geometries

## Spatial Joins and Filters

Combine or subset geometries based on spatial relationships

- [`ddbs_intersection()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md)
  [`ddbs_difference()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md)
  [`ddbs_sym_difference()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md)
  [`ddbs_crop()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md)
  [`ddbs_shortest_line()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md)
  : Geometry binary operations
- [`ddbs_filter()`](https://cidree.github.io/duckspatial/reference/ddbs_filter.md)
  : Perform a spatial filter
- [`ddbs_interpolate_aw()`](https://cidree.github.io/duckspatial/reference/ddbs_interpolate_aw.md)
  : Areal-Weighted Interpolation using DuckDB
- [`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md)
  : Perform a spatial join of two geometries

## Geometry Construction

Create new geometries from scratch or from existing data

- [`ddbs_as_points()`](https://cidree.github.io/duckspatial/reference/ddbs_as_points.md)
  : Generate point geometries from coordinates
- [`ddbs_point()`](https://cidree.github.io/duckspatial/reference/ddbs_point.md)
  : Create point geometries from coordinate vectors
- [`ddbs_generate_points()`](https://cidree.github.io/duckspatial/reference/ddbs_generate_points.md)
  : Generate random points within bounding boxes of geometries
- [`ddbs_quadkey()`](https://cidree.github.io/duckspatial/reference/ddbs_quadkey.md)
  : Convert point geometries to QuadKey tiles

## Geometry Processing - General

Modify, simplify, and transform individual geometries

- [`ddbs_buffer()`](https://cidree.github.io/duckspatial/reference/ddbs_buffer.md)
  : Creates a buffer around geometries
- [`ddbs_centroid()`](https://cidree.github.io/duckspatial/reference/ddbs_centroid.md)
  : Calculates the centroid of geometries
- [`ddbs_concave_hull()`](https://cidree.github.io/duckspatial/reference/ddbs_concave_hull.md)
  : Compute the concave hull of geometries
- [`ddbs_convex_hull()`](https://cidree.github.io/duckspatial/reference/ddbs_convex_hull.md)
  : Compute the convex hull of geometries
- [`ddbs_exterior_ring()`](https://cidree.github.io/duckspatial/reference/ddbs_exterior_ring.md)
  : Extract the exterior ring of polygons
- [`ddbs_maximum_inscribed_circle()`](https://cidree.github.io/duckspatial/reference/ddbs_maximum_inscribed_circle.md)
  : Computes the maximum inscribed circle of a geometry
- [`ddbs_make_line()`](https://cidree.github.io/duckspatial/reference/ddbs_make_line.md)
  : Create lines from point geometries
- [`ddbs_normalize()`](https://cidree.github.io/duckspatial/reference/ddbs_normalize.md)
  : Normalize geometries to a canonical form
- [`ddbs_reverse()`](https://cidree.github.io/duckspatial/reference/ddbs_reverse.md)
  : Reverse the vertex order of geometries
- [`ddbs_vertices()`](https://cidree.github.io/duckspatial/reference/ddbs_vertices.md)
  : Collect all vertices of a geometry into a MULTIPOINT
- [`ddbs_voronoi()`](https://cidree.github.io/duckspatial/reference/ddbs_voronoi.md)
  : Computes a Voronoi diagram from point geometries

## Geometry Processing - Linestrings

Specialized functions for working with linestring geometries

- [`ddbs_line_startpoint()`](https://cidree.github.io/duckspatial/reference/ddbs_endpoint_startpoint.md)
  [`ddbs_line_endpoint()`](https://cidree.github.io/duckspatial/reference/ddbs_endpoint_startpoint.md)
  : Extract the start or end point of a linestring geometry
- [`ddbs_line_interpolate()`](https://cidree.github.io/duckspatial/reference/ddbs_line_interpolate.md)
  : Interpolates a point or points along a line geometry
- [`ddbs_line_locate_point()`](https://cidree.github.io/duckspatial/reference/ddbs_line_locate_point.md)
  : Locate a point along a linestring
- [`ddbs_line_merge()`](https://cidree.github.io/duckspatial/reference/ddbs_line_merge.md)
  : Merge line geometries into a single line
- [`ddbs_line_node()`](https://cidree.github.io/duckspatial/reference/ddbs_line_node.md)
  : Node a set of linestrings
- [`ddbs_line_substring()`](https://cidree.github.io/duckspatial/reference/ddbs_line_substring.md)
  : Extract a substring of a line geometry
- [`ddbs_build_area()`](https://cidree.github.io/duckspatial/reference/ddbs_build_area.md)
  : Build polygon areas from multiple linestrings
- [`ddbs_make_polygon()`](https://cidree.github.io/duckspatial/reference/ddbs_make_polygon.md)
  : Create a polygon from a single closed linestring
- [`ddbs_polygonize()`](https://cidree.github.io/duckspatial/reference/ddbs_polygonize.md)
  : Assemble polygons from multiple linestrings

## Geometry Type Conversion

Change geometry types or decompose multi-part geometries

- [`ddbs_dump()`](https://cidree.github.io/duckspatial/reference/ddbs_dump.md)
  : Dumps geometries into their component parts
- [`ddbs_multi()`](https://cidree.github.io/duckspatial/reference/ddbs_multi.md)
  : Convert geometries to multi-type
- [`ddbs_union()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md)
  [`ddbs_combine()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md)
  [`ddbs_union_agg()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md)
  : Union and combine geometries
- [`ddbs_intersection_agg()`](https://cidree.github.io/duckspatial/reference/ddbs_intersection_agg.md)
  : Aggregate the intersection of geometries

## Coordinate Operations

Transform and manipulate coordinate systems

- [`ddbs_transform()`](https://cidree.github.io/duckspatial/reference/ddbs_transform.md)
  : Transform the coordinate reference system of geometries
- [`ddbs_set_crs()`](https://cidree.github.io/duckspatial/reference/ddbs_set_crs.md)
  : Set the coordinate reference system of geometries
- [`ddbs_flip_coordinates()`](https://cidree.github.io/duckspatial/reference/ddbs_flip_coordinates.md)
  : Flips the X and Y coordinates of geometries
- [`ddbs_x()`](https://cidree.github.io/duckspatial/reference/ddbs_xy.md)
  [`ddbs_y()`](https://cidree.github.io/duckspatial/reference/ddbs_xy.md)
  [`ddbs_m()`](https://cidree.github.io/duckspatial/reference/ddbs_xy.md)
  [`ddbs_z()`](https://cidree.github.io/duckspatial/reference/ddbs_xy.md)
  : Extract coordinates from geometries
- [`ddbs_xmax()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md)
  [`ddbs_xmin()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md)
  [`ddbs_ymax()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md)
  [`ddbs_ymin()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md)
  [`ddbs_zmax()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md)
  [`ddbs_zmin()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md)
  [`ddbs_mmax()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md)
  [`ddbs_mmin()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md)
  : Coordinate bounds of geometries
- [`ddbs_force_2d()`](https://cidree.github.io/duckspatial/reference/ddbs_force_dim.md)
  [`ddbs_force_3d()`](https://cidree.github.io/duckspatial/reference/ddbs_force_dim.md)
  [`ddbs_force_4d()`](https://cidree.github.io/duckspatial/reference/ddbs_force_dim.md)
  : Force geometry dimensions

## Dimension operations

Utility functions for M and Z dimensions

- [`ddbs_locate_along()`](https://cidree.github.io/duckspatial/reference/ddbs_locate.md)
  [`ddbs_locate_between()`](https://cidree.github.io/duckspatial/reference/ddbs_locate.md)
  : Locate geometries at specific M values
- [`ddbs_has_z()`](https://cidree.github.io/duckspatial/reference/ddbs_has_dim.md)
  [`ddbs_has_m()`](https://cidree.github.io/duckspatial/reference/ddbs_has_dim.md)
  : Check geometry dimensions

## Geometry Validation

Check and repair geometry validity and dimensionality

- [`ddbs_geometry_type()`](https://cidree.github.io/duckspatial/reference/ddbs_geometry_type.md)
  : Get the geometry type of features
- [`ddbs_make_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_make_valid.md)
  : Make invalid geometries valid
- [`ddbs_reduce_precision()`](https://cidree.github.io/duckspatial/reference/ddbs_reduce_precision.md)
  : Reduce the coordinate precision of geometries
- [`ddbs_remove_repeated_points()`](https://cidree.github.io/duckspatial/reference/ddbs_remove_repeated_points.md)
  : Remove repeated points from a geometry
- [`ddbs_simplify()`](https://cidree.github.io/duckspatial/reference/ddbs_simplify.md)
  : Simplify geometries
- [`ddbs_is_simple()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md)
  [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md)
  [`ddbs_is_closed()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md)
  [`ddbs_is_empty()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md)
  [`ddbs_is_ring()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md)
  : Geometry validation functions

## Format Conversion

Convert geometries to and from standard spatial formats

- [`ddbs_as_text()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md)
  [`ddbs_as_wkb()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md)
  [`ddbs_as_hexwkb()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md)
  [`ddbs_as_geojson()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md)
  : Convert geometries to standard interchange formats
- [`ddbs_geom_from_text()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md)
  [`ddbs_geom_from_wkb()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md)
  [`ddbs_geom_from_hexwkb()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md)
  [`ddbs_geom_from_hexewkb()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md)
  [`ddbs_geom_from_geojson()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md)
  : Create geometries from standard interchange formats

## Vector Tiles (MVT)

Encode geometries as Mapbox Vector Tiles for web mapping

- [`ddbs_as_mvt_geom()`](https://cidree.github.io/duckspatial/reference/ddbs_as_mvt_geom.md)
  : Transform geometries into Mapbox Vector Tile coordinate space
- [`ddbs_write_mbtiles()`](https://cidree.github.io/duckspatial/reference/ddbs_write_mbtiles.md)
  : Write a Mapbox Vector Tile pyramid to an MBTiles file

## Spatial Extent and Boundaries

Extract bounding boxes and envelopes

- [`ddbs_bbox()`](https://cidree.github.io/duckspatial/reference/ddbs_bbox.md)
  : Get the bounding box of geometries
- [`ddbs_boundary()`](https://cidree.github.io/duckspatial/reference/ddbs_boundary.md)
  : Get the boundary of geometries
- [`ddbs_envelope()`](https://cidree.github.io/duckspatial/reference/ddbs_envelope.md)
  : Get the envelope (bounding box) of geometries
- [`ddbs_make_envelope()`](https://cidree.github.io/duckspatial/reference/ddbs_make_envelope.md)
  : Create a rectangular polygon from bounding coordinates
- [`ddbs_minimum_rotated_rectangle()`](https://cidree.github.io/duckspatial/reference/ddbs_minimum_rotated_rectangle.md)
  : Computes the minimum rotated rectangle enclosing a geometry

## Affine Transformations

Rotate, scale, shear, and shift geometries

- [`ddbs_affine()`](https://cidree.github.io/duckspatial/reference/ddbs_affine.md)
  : Apply an affine transformation to geometries
- [`ddbs_flip()`](https://cidree.github.io/duckspatial/reference/ddbs_flip.md)
  : Flip geometries horizontally or vertically
- [`ddbs_rotate()`](https://cidree.github.io/duckspatial/reference/ddbs_rotate.md)
  : Rotate geometries around their centroid
- [`ddbs_rotate_3d()`](https://cidree.github.io/duckspatial/reference/ddbs_rotate_3d.md)
  : Rotate 3D geometries around an axis
- [`ddbs_scale()`](https://cidree.github.io/duckspatial/reference/ddbs_scale.md)
  : Scale geometries by X and Y factors
- [`ddbs_shear()`](https://cidree.github.io/duckspatial/reference/ddbs_shear.md)
  : Shear geometries
- [`ddbs_shift()`](https://cidree.github.io/duckspatial/reference/ddbs_shift.md)
  : Shift geometries by X and Y offsets

## Geometry Queries

Query structural properties of geometries without modifying them

- [`ddbs_get_npoints()`](https://cidree.github.io/duckspatial/reference/ddbs_get_npoints.md)
  [`ddbs_get_ngeometries()`](https://cidree.github.io/duckspatial/reference/ddbs_get_npoints.md)
  [`ddbs_get_ninterior_rings()`](https://cidree.github.io/duckspatial/reference/ddbs_get_npoints.md)
  : Count geometry components
- [`ddbs_dimension()`](https://cidree.github.io/duckspatial/reference/ddbs_dimension.md)
  : Get the topological dimension of a geometry

## Measurements

Calculate areas, distances, lengths, perimeters, and bearings

- [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md)
  [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md)
  [`ddbs_perimeter()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md)
  [`ddbs_distance()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md)
  [`ddbs_azimuth()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md)
  : Calculate geometric measurements

## Database Utilities

Helper functions for managing DuckDB schemas and metadata

- [`ddbs_create_schema()`](https://cidree.github.io/duckspatial/reference/ddbs_create_schema.md)
  : Check and create schema
- [`ddbs_crs()`](https://cidree.github.io/duckspatial/reference/ddbs_crs.md)
  : Check CRS of spatial objects or database tables
- [`ddbs_drivers()`](https://cidree.github.io/duckspatial/reference/ddbs_drivers.md)
  : Get list of GDAL drivers and file formats
- [`ddbs_glimpse()`](https://cidree.github.io/duckspatial/reference/ddbs_glimpse.md)
  : Check first rows of the data
- [`ddbs_list_tables()`](https://cidree.github.io/duckspatial/reference/ddbs_list_tables.md)
  : Check tables and schemas inside a database

## Lazy Spatial Data Frames

Create and work with lazy `duckspatial_df` objects

- [`as_duckspatial_df()`](https://cidree.github.io/duckspatial/reference/as_duckspatial_df.md)
  : Convert objects to duckspatial_df
- [`is_duckspatial_df()`](https://cidree.github.io/duckspatial/reference/is_duckspatial_df.md)
  : Check if object is a duckspatial_df
- [`as_nanoarrow_array_stream.duckspatial_df()`](https://cidree.github.io/duckspatial/reference/as_nanoarrow_array_stream.duckspatial_df.md)
  : Convert a duckspatial_df to a nanoarrow_array_stream
- [`collect(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/ddbs_collect.md)
  [`ddbs_collect()`](https://cidree.github.io/duckspatial/reference/ddbs_collect.md)
  : Collect a duckspatial_df with flexible output formats
- [`ddbs_compute()`](https://cidree.github.io/duckspatial/reference/ddbs_compute.md)
  : Force computation of a lazy duckspatial_df
- [`ddbs_drop_geometry()`](https://cidree.github.io/duckspatial/reference/ddbs_drop_geometry.md)
  : Drop geometry column from a duckspatial_df object
- [`ddbs_geom_col()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_col.md)
  : Get the geometry column name
- [`st_crs(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_sf.md)
  [`st_geometry(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_sf.md)
  [`st_bbox(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_sf.md)
  [`st_as_sf(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_sf.md)
  [`print(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_sf.md)
  : sf methods for duckspatial_df
- [`dplyr_reconstruct(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`compute(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`select(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`filter(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`arrange(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`rename(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`slice(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`head(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`glimpse(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`mutate(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`count(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`distinct(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`left_join(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`inner_join(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`right_join(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`full_join(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`group_by(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`ungroup(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  [`summarise(`*`<duckspatial_df>`*`)`](https://cidree.github.io/duckspatial/reference/duckspatial_df_dplyr.md)
  : dplyr methods for duckspatial_df

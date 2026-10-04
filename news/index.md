# Changelog

## duckspatial 1.2.1

CRAN release: 2026-07-04

### ENHANCEMENTS

- Capture output message of
  [`ddbs_install()`](https://cidree.github.io/duckspatial/reference/ddbs_install.md)
  and
  [`ddbs_load()`](https://cidree.github.io/duckspatial/reference/ddbs_load.md)([\#147](https://github.com/Cidree/duckspatial/issues/147)).

## duckspatial 1.2.0

CRAN release: 2026-07-02

### NEW FEATURES

- [`ddbs_extension_info()`](https://cidree.github.io/duckspatial/reference/ddbs_extension_info.md):
  prints a
  [`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) of a
  DuckDB extension’s row from `duckdb_extensions()` (the spatial
  extension by default), showing its installed/loaded status, version,
  and install path.

- [`ddbs_reduce_precision()`](https://cidree.github.io/duckspatial/reference/ddbs_reduce_precision.md):
  snaps geometry coordinates to a regular grid, reducing their
  precision.

- [`ddbs_line_node()`](https://cidree.github.io/duckspatial/reference/ddbs_line_node.md):
  nodes a set of line geometries, splitting them at every crossing and
  returning a fully noded `MULTILINESTRING`.

- [`ddbs_intersection_agg()`](https://cidree.github.io/duckspatial/reference/ddbs_intersection_agg.md):
  computes the geometric intersection (common area) of a set of
  geometries, optionally grouped by one or more columns. The
  intersection counterpart to
  [`ddbs_union_agg()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md).

- [`ddbs_reverse()`](https://cidree.github.io/duckspatial/reference/ddbs_reverse.md):
  returns each geometry with the order of its vertices reversed.

- [`ddbs_normalize()`](https://cidree.github.io/duckspatial/reference/ddbs_normalize.md):
  returns each geometry in its normalized (canonical) form.

- [`ddbs_write_mbtiles()`](https://cidree.github.io/duckspatial/reference/ddbs_write_mbtiles.md):
  generates a Mapbox Vector Tile pyramid from a spatial dataset and
  writes it to an MBTiles file, ready to serve or convert to PMTiles.

- [`ddbs_as_mvt_geom()`](https://cidree.github.io/duckspatial/reference/ddbs_as_mvt_geom.md):
  transforms geometries into Mapbox Vector Tile (MVT) coordinate space,
  clipping them to a tile’s bounding box and mapping the coordinates
  into the tile’s integer pixel space.

- [`ddbs_geom_from_text()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md),
  [`ddbs_geom_from_wkb()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md),
  [`ddbs_geom_from_hexwkb()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md),
  [`ddbs_geom_from_hexewkb()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md),
  [`ddbs_geom_from_geojson()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_from.md):
  parse serialized geometries (WKT, WKB, HEXWKB, HEXEWKB, GeoJSON) into
  a spatial object. These are the inverses of the `ddbs_as_*()`
  serializers.

- [`ddbs_get_ninterior_rings()`](https://cidree.github.io/duckspatial/reference/ddbs_get_npoints.md):
  returns the number of interior rings (holes) in a POLYGON geometry.

### ENHANCEMENTS

- [`ddbs_install()`](https://cidree.github.io/duckspatial/reference/ddbs_install.md):
  gains a `repos` argument to install an extension from a specific
  DuckDB repository (e.g. `"core"`, `"core_nightly"`, `"community"`).
  When `NULL` (default), the previous behaviour is kept (core, then
  community)
  ([\#144](https://github.com/Cidree/duckspatial/issues/144)).

- [`ddbs_as_geojson()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md):
  now includes all non-geometry columns as feature `properties` instead
  of serializing only the geometry. By default it returns a single
  GeoJSON `FeatureCollection` (matching
  [`geojsonsf::sf_geojson()`](https://rdrr.io/pkg/geojsonsf/man/sf_geojson.html));
  pass `feature_collection = FALSE` for a vector with one `Feature` per
  row ([\#141](https://github.com/Cidree/duckspatial/issues/141)).

### BUG FIXES

- Fix a mistake in the startup message
  ([\#146](https://github.com/Cidree/duckspatial/issues/146)).

- [`ddbs_install()`](https://cidree.github.io/duckspatial/reference/ddbs_install.md):
  removed a broken “already on the latest version” check that referenced
  a `requires_version_upgrade` column which `duckdb_extensions()` does
  not provide (and compared `install_mode` with the wrong case), so it
  never took effect
  ([\#144](https://github.com/Cidree/duckspatial/issues/144)).

## duckspatial 1.1.2

CRAN release: 2026-06-22

### NEW FEATURES

- [`ddbs_shortest_line()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md):
  returns the LINESTRING connecting the closest points between each pair
  of geometries from `x` and `y`.

- [`ddbs_azimuth()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md):
  computes the clockwise azimuth (bearing from north) between two sets
  of POINT geometries. Returns a numeric matrix (`mode = "sf"`) or a
  lazy tbl with all pairs (default). Supports radians (default) and
  degrees via the `unit` argument.

- [`ddbs_vertices()`](https://cidree.github.io/duckspatial/reference/ddbs_vertices.md):
  collects all vertices of a geometry into a MULTIPOINT.

- [`ddbs_point()`](https://cidree.github.io/duckspatial/reference/ddbs_point.md):
  creates POINT geometries from numeric coordinate vectors. Supports 2D,
  3D (Z), and 4D (Z + M) coordinates, extra attribute columns via `...`,
  and CRS assignment.

- [`ddbs_xmax()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md),
  [`ddbs_xmin()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md),
  [`ddbs_ymax()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md),
  [`ddbs_ymin()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md),
  [`ddbs_zmax()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md),
  [`ddbs_zmin()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md),
  [`ddbs_mmax()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md),
  [`ddbs_mmin()`](https://cidree.github.io/duckspatial/reference/ddbs_coord_bounds.md):
  return the maximum or minimum coordinate value for each geometry
  (`by_feature = TRUE`) or the global extreme across the dataset
  (`by_feature = FALSE`).

- [`ddbs_dimension()`](https://cidree.github.io/duckspatial/reference/ddbs_dimension.md):
  returns the topological dimension of each geometry (0 = point, 1 =
  line, 2 = polygon, -1 = empty).

- [`ddbs_line_locate_point()`](https://cidree.github.io/duckspatial/reference/ddbs_line_locate_point.md):
  returns the fractional position (0–1) of the closest point on a
  linestring to a reference point. The `y` argument accepts an `sf`
  object, a `duckspatial_df`, or a character DuckDB table name (each
  must contain exactly 1 point feature).

### ENHANCEMENTS

- [`ddbs_union_agg()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md):
  gains a `mem` argument. Set `mem = TRUE` to use `ST_MemUnion_Agg()`
  instead of `ST_Union_Agg()` — slower but more memory efficient.

## duckspatial 1.1.1

CRAN release: 2026-06-06

### NEW FEATURES

- [`ddbs_get_npoints()`](https://cidree.github.io/duckspatial/reference/ddbs_get_npoints.md):
  returns the number of points (vertices) in a geometry.

- [`ddbs_get_ngeometries()`](https://cidree.github.io/duckspatial/reference/ddbs_get_npoints.md):
  returns the number of sub-geometries in a GEOMETRYCOLLECTION or
  MULTI\* geometry.

- [`ddbs_affine()`](https://cidree.github.io/duckspatial/reference/ddbs_affine.md):
  applies an affine transformation to geometries using a 2x3 or 3x4
  matrix ([\#133](https://github.com/Cidree/duckspatial/issues/133)).

### ENHANCEMENTS

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md)
  and
  [`ddbs_write_dataset()`](https://cidree.github.io/duckspatial/reference/ddbs_write_dataset.md)
  gain a `duckdb_storage_version` argument to control DuckDB storage
  compatibility. They now default to DuckDB `v1.5.0` storage (**Native
  Spatial Storage**) so that CRS metadata can persist in native
  `GEOMETRY` columns. Users can specify older versions (e.g., `v1.0.0`
  for **Legacy Compatibility**) when the output must be readable by
  older DuckDB clients. For more details on DuckDB storage versions, see
  <https://duckdb.org/docs/internals/storage>
  ([\#130](https://github.com/Cidree/duckspatial/issues/130),
  [\#132](https://github.com/Cidree/duckspatial/issues/132)).

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md):
  stricter validation of `dbdir` parameter. Now only accepts `"memory"`,
  `"tempdir"`, or file paths with `.duckdb`, `.db`, or `.ddb` extensions
  ([\#132](https://github.com/Cidree/duckspatial/issues/132)).

- [`ddbs_stop_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_stop_conn.md):
  now explicitly shuts down the DuckDB driver and forces a checkpoint
  (necessary to release the file lock on Windows)
  ([\#132](https://github.com/Cidree/duckspatial/issues/132)).

- `dplyr` methods on `duckspatial_df` now return a lazy temporary view,
  rather than creating a new temporary table
  ([\#130](https://github.com/Cidree/duckspatial/issues/130),
  [\#134](https://github.com/Cidree/duckspatial/issues/134)).

## duckspatial 1.1.0

CRAN release: 2026-05-17

### NEW FEATURES

- Implementation of `duckspatial` macros: this allows to use some
  `duckspatial` functions within `dplyr` verbs
  (e.g. `data |> mutate(area = ddbs_area(geometry))`)
  ([\#92](https://github.com/Cidree/duckspatial/issues/92)).

- [`ddbs_dump()`](https://cidree.github.io/duckspatial/reference/ddbs_dump.md):
  decompose multi-geometry types into individual single geometry
  components ([\#44](https://github.com/Cidree/duckspatial/issues/44),
  117).

- [`ddbs_maximum_inscribed_circle()`](https://cidree.github.io/duckspatial/reference/ddbs_maximum_inscribed_circle.md):
  returns the maximum inscribed circle of the input geometry
  ([\#117](https://github.com/Cidree/duckspatial/issues/117)).

- [`ddbs_minimum_rotated_rectangle()`](https://cidree.github.io/duckspatial/reference/ddbs_minimum_rotated_rectangle.md):
  returns the minimum rotated rectangle that bounds the input geometry
  ([\#117](https://github.com/Cidree/duckspatial/issues/117)).

- [`ddbs_set_crs()`](https://cidree.github.io/duckspatial/reference/ddbs_set_crs.md):
  assigns the CRS to a spatial object. No transformation is applied to
  the geometries
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_crop()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md):
  similar to
  [`ddbs_intersection()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md),
  but it crops to the bounding box
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_line_interpolate()`](https://cidree.github.io/duckspatial/reference/ddbs_line_interpolate.md):
  interpolates a point or points along a line geometry
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_line_substring()`](https://cidree.github.io/duckspatial/reference/ddbs_line_substring.md):
  gets a fraction of a linestring
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_line_merge()`](https://cidree.github.io/duckspatial/reference/ddbs_line_merge.md):merges
  connected multistrings
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_z()`](https://cidree.github.io/duckspatial/reference/ddbs_xy.md)
  and
  [`ddbs_m()`](https://cidree.github.io/duckspatial/reference/ddbs_xy.md):
  to extract Z and M coordinates as a new column
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_make_envelope()`](https://cidree.github.io/duckspatial/reference/ddbs_make_envelope.md):
  creates a rectangular polygon from 4 coordinates
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_locate_between()`](https://cidree.github.io/duckspatial/reference/ddbs_locate.md):
  locates points that fall with the specified M range
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_locate_along()`](https://cidree.github.io/duckspatial/reference/ddbs_locate.md):
  locates points that match the specified M value
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_remove_repeated_points()`](https://cidree.github.io/duckspatial/reference/ddbs_remove_repeated_points.md):
  removes repeated points, optionally with some tolerance
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_read_meta()`](https://cidree.github.io/duckspatial/reference/ddbs_read_meta.md):
  reads the metadata of a vectorial data file
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_make_line()`](https://cidree.github.io/duckspatial/reference/ddbs_make_line.md):
  creates LINESTRINGS from POINT geometries
  ([\#126](https://github.com/Cidree/duckspatial/issues/126)).

### ENHANCEMENTS

- `group_by` and `summarise` methods now drop the spatial attributes
  when the output is not a `duckspatial_df` anymore
  ([\#119](https://github.com/Cidree/duckspatial/issues/119)).

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md):
  gains the `upgrade` argument that is passed to
  [`ddbs_install()`](https://cidree.github.io/duckspatial/reference/ddbs_install.md).

- [`ddbs_install()`](https://cidree.github.io/duckspatial/reference/ddbs_install.md):
  now returns a better error message if the extension is already loaded,
  and there’s an attempt to upgrade it.

- [`ddbs_centroid()`](https://cidree.github.io/duckspatial/reference/ddbs_centroid.md):
  gains the argument `method` to implement ST_PointOnSurface
  ([\#118](https://github.com/Cidree/duckspatial/issues/118)).

- [`ddbs_as_points()`](https://cidree.github.io/duckspatial/reference/ddbs_as_points.md)
  allows to create a `duckspatial_df` from raw coordinate or WKT
  columns. It also gains two new arguments: `remove` and `na.fail`
  ([\#125](https://github.com/Cidree/duckspatial/issues/125)).

- [`ddbs_open_dataset()`](https://cidree.github.io/duckspatial/reference/ddbs_open_dataset.md):
  can open geoparquet files when the geometry is encoded as WKB
  geoparquet. It also fails with a better error message when the
  geometry is encoded as a native arrow/geoarrow encoding
  ([\#129](https://github.com/Cidree/duckspatial/issues/129)).

### BUG FIXES

- Large datasets couldn’t be processed because an `arrow` code
  limitation in
  [`ddbs_register_table()`](https://cidree.github.io/duckspatial/reference/ddbs_register_table.md)
  ([\#124](https://github.com/Cidree/duckspatial/issues/124)).

## duckspatial 1.0.0

CRAN release: 2026-03-30

Learn more about this version
[here](https://adrian-cidre.com/posts/015_duckspatial_v100/).

### MAJOR CHANGES

- `duckspatial_df` becomes the main class of `duckspatial`. It
  represents a lazy, table-like object whose data is not loaded into
  memory until explicitly materialized (with
  [`ddbs_collect()`](https://cidree.github.io/duckspatial/reference/ddbs_collect.md)
  or
  [`st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)).
  Every function now accepts this class as input, and it’s the returned
  class by default. If the user wants to materialize the result in the
  same way `sf` would do, that can be done with `mode = "sf"`
  ([\#55](https://github.com/Cidree/duckspatial/issues/55),
  [\#63](https://github.com/Cidree/duckspatial/issues/63)).

- [`ddbs_buffer()`](https://cidree.github.io/duckspatial/reference/ddbs_buffer.md):
  now has four new arguments: `num_triangles`, `cap_style`,
  `join_style`, and `mitre_limit`
  ([\#72](https://github.com/Cidree/duckspatial/issues/72)).

- [`ddbs_union()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md):
  is split into two new functions depending on the desired behavior:
  [`ddbs_union()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md)
  and
  [`ddbs_union_agg()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md)
  ([\#77](https://github.com/Cidree/duckspatial/issues/77)).

- [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md),
  [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md)
  and
  [`ddbs_distance()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md):
  now use by default the best DuckDB function (e.g. `ST_Area()` or
  `ST_Area_Spheroid()`) depending on the input’s CRS. They also return a
  `duckspatial_df` object by default rather than a materialized vector.
  In the case of
  [`ddbs_distance()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md),
  it returns a `tbl_duckdb_connection`
  ([\#80](https://github.com/Cidree/duckspatial/issues/80),
  [\#82](https://github.com/Cidree/duckspatial/issues/82),
  [\#103](https://github.com/Cidree/duckspatial/issues/103)).

- [`ddbs_simplify()`](https://cidree.github.io/duckspatial/reference/ddbs_simplify.md):
  tolerance defaults to 0; gains a new argument `preserve_topology`
  specified before `conn`
  ([\#86](https://github.com/Cidree/duckspatial/issues/86)).

- [`ddbs_is_simple()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md),
  [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md),
  [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md),
  [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md),
  [`ddbs_distance()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md):
  the `new_column` argument now defaults to a column name, as we now
  encourage the users to keep most of the work inside DuckDB, rather
  than materialize the result. For materializing a vector in R, use
  `mode = "sf"`. This argument is also moved before `conn` argument
  ([\#83](https://github.com/Cidree/duckspatial/issues/83)).

- [`ddbs_predicate()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md)
  and colleagues: they gain new arguments: name, mode, overwrite, and
  quiet. When `mode = "duckspatial"`, they return a lazy tbl backed by
  DuckDB. When `mode = "sf"`, they return a list/matrix
  ([\#105](https://github.com/Cidree/duckspatial/issues/105)).

### NEW FEATURES

- [`ddbs_as_points()`](https://cidree.github.io/duckspatial/reference/ddbs_as_points.md):
  converts a table with coordinates into a spatial object
  ([\#75](https://github.com/Cidree/duckspatial/issues/75)).

- [`ddbs_geometry_type()`](https://cidree.github.io/duckspatial/reference/ddbs_geometry_type.md):
  returns the geometry type of an object
  ([\#76](https://github.com/Cidree/duckspatial/issues/76)).

- [`ddbs_as_geojson()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md):
  converts the geometry to geojson format
  ([\#84](https://github.com/Cidree/duckspatial/issues/84)).

- [`ddbs_perimeter()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md):
  calculates the perimeter of polygons
  ([\#89](https://github.com/Cidree/duckspatial/issues/89)).

- New geometry validation/check functions:
  [`ddbs_is_empty()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md),
  [`ddbs_is_ring()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md)
  and
  [`ddbs_is_closed()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md)
  ([\#91](https://github.com/Cidree/duckspatial/issues/91)).

- [`ddbs_sym_difference()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md):
  performs symmetric difference between pairs of geometries
  ([\#91](https://github.com/Cidree/duckspatial/issues/91)).

- [`ddbs_force_2d()`](https://cidree.github.io/duckspatial/reference/ddbs_force_dim.md),
  [`ddbs_force_3d()`](https://cidree.github.io/duckspatial/reference/ddbs_force_dim.md),
  [`ddbs_force_4d()`](https://cidree.github.io/duckspatial/reference/ddbs_force_dim.md):
  force the geometries to have specfic dimensions
  ([\#91](https://github.com/Cidree/duckspatial/issues/91)).

- [`ddbs_has_z()`](https://cidree.github.io/duckspatial/reference/ddbs_has_dim.md)
  and
  [`ddbs_has_m()`](https://cidree.github.io/duckspatial/reference/ddbs_has_dim.md):
  check if the geometry has the dimension
  ([\#91](https://github.com/Cidree/duckspatial/issues/91)).

- [`ddbs_polygonize()`](https://cidree.github.io/duckspatial/reference/ddbs_polygonize.md),
  [`ddbs_build_area()`](https://cidree.github.io/duckspatial/reference/ddbs_build_area.md):
  generates polygons from lines
  ([\#91](https://github.com/Cidree/duckspatial/issues/91)).

- [`ddbs_voronoi()`](https://cidree.github.io/duckspatial/reference/ddbs_voronoi.md):
  generates Voronoi diagrams from point geometries
  ([\#91](https://github.com/Cidree/duckspatial/issues/91)).

- [`ddbs_endpoint()`](https://cidree.github.io/duckspatial/reference/ddbs_endpoint.md)
  and `ddbs_start_point()`: extracts the start/end point of a linestring
  geometry ([\#91](https://github.com/Cidree/duckspatial/issues/91)).

- [`ddbs_flip_coordinates()`](https://cidree.github.io/duckspatial/reference/ddbs_flip_coordinates.md):
  swaps X and Y coordinates
  ([\#91](https://github.com/Cidree/duckspatial/issues/91)).

- [`ddbs_register_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_register_vector.md),
  [`ddbs_write_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)
  and
  [`ddbs_read_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_read_vector.md)
  deprecated in favour of
  [`ddbs_register_table()`](https://cidree.github.io/duckspatial/reference/ddbs_register_table.md),
  [`ddbs_write_table()`](https://cidree.github.io/duckspatial/reference/ddbs_write_table.md)
  and
  [`ddbs_read_table()`](https://cidree.github.io/duckspatial/reference/ddbs_read_table.md)
  ([\#100](https://github.com/Cidree/duckspatial/issues/100)).

- [`ddbs_x()`](https://cidree.github.io/duckspatial/reference/ddbs_xy.md)
  and
  [`ddbs_y()`](https://cidree.github.io/duckspatial/reference/ddbs_xy.md):
  extract the `x` and `y` coordinates of points
  ([\#108](https://github.com/Cidree/duckspatial/issues/108)).

- [`ddbs_drop_geometry()`](https://cidree.github.io/duckspatial/reference/ddbs_drop_geometry.md):
  drops the geometry column of a `duckspatial_df` object.

- [`ddbs_options()`](https://cidree.github.io/duckspatial/reference/ddbs_options.md):
  to set some `duckspatial` default options.

- [`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md):
  dwithin is now implemented for spatial join.

### MINOR CHANGES

- Improve the documentation of the functions
  ([\#85](https://github.com/Cidree/duckspatial/issues/85)).

- [`ddbs_buffer()`](https://cidree.github.io/duckspatial/reference/ddbs_buffer.md):
  warns if the input CRS is not a projected CRS, as the distance uses
  its units.

- [`ddbs_quadkey()`](https://cidree.github.io/duckspatial/reference/ddbs_quadkey.md):
  can aggregate by `field` when output is `polygon` and `tilexy`
  ([\#78](https://github.com/Cidree/duckspatial/issues/78)).

- [`ddbs_crs()`](https://cidree.github.io/duckspatial/reference/ddbs_crs.md):
  accepts CRS codes and `crs` objects as inputs. It returns `NULL` when
  the input doesn’t have a geometry (e.g. a `data.frame`)
  ([\#87](https://github.com/Cidree/duckspatial/issues/87)).

- [`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md):
  now has … that are paseed to
  [`dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) for
  extra configuration.

### BUG FIXES

- [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md),
  [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md)
  and
  [`ddbs_distance()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md)
  were calculating the wrong measure when the CRS was geographic
  ([\#82](https://github.com/Cidree/duckspatial/issues/82)).

- `ddbs_filter(predicate = "dwithin")` and `ddbs_is_within_distance`
  were calculating wrong distances for geographic CRS
  ([\#88](https://github.com/Cidree/duckspatial/issues/88)).

## duckspatial 0.9.0

CRAN release: 2026-01-10

Learn more about this version
[here](https://adrian-cidre.com/posts/014_duckspatial/).

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

- [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md):
  adds a new column with the length of the geometries
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md):
  adds a new column with the area of the geometries
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_distance()`](https://cidree.github.io/duckspatial/reference/ddbs_measure_funs.md):
  calculates the distance between two geometries
  ([\#34](https://github.com/Cidree/duckspatial/issues/34)).

- [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md):
  adds a new logical column asserting the simplicity of the geometries
  ([\#17](https://github.com/Cidree/duckspatial/issues/17)).

- [`ddbs_is_valid()`](https://cidree.github.io/duckspatial/reference/ddbs_geom_validation_funs.md):
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

- [`ddbs_union()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md):
  union of geometries
  ([\#36](https://github.com/Cidree/duckspatial/issues/36)).

- [`ddbs_combine()`](https://cidree.github.io/duckspatial/reference/ddbs_union_funs.md):
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

- [`ddbs_intersects()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md),
  [`ddbs_crosses()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md),
  [`ddbs_touches()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md),
  …: shortcuts for e.g.: `ddbs_predicate(predicate = "intersects")`
  ([\#28](https://github.com/Cidree/duckspatial/issues/28)).

- [`ddbs_transform()`](https://cidree.github.io/duckspatial/reference/ddbs_transform.md):
  transforms from one coordinates reference system to another
  ([\#43](https://github.com/Cidree/duckspatial/issues/43)).

- [`ddbs_as_text()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md):
  converts geometries to well-known text (WKT) format
  ([\#47](https://github.com/Cidree/duckspatial/issues/47)).

- [`ddbs_as_wkb()`](https://cidree.github.io/duckspatial/reference/ddbs_as_format.md):
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
  [`ddbs_intersects()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md),
  [`ddbs_crosses()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md),
  [`ddbs_touches()`](https://cidree.github.io/duckspatial/reference/ddbs_predicate.md),
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

- [`ddbs_difference()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md):
  calculates the geometric difference between two objects

### IMPROVEMENTS

- [`ddbs_intersection()`](https://cidree.github.io/duckspatial/reference/ddbs_binary_funs.md):
  overwrite argument defaults to `FALSE` instead of `NULL`

- Better schemas management. Added support for all functions.

## duckspatial 0.1.0

CRAN release: 2025-04-19

- Initial CRAN submission.

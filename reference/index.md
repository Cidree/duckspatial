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

## Spatial operations (binary)

- [`ddbs_difference()`](https://cidree.github.io/duckspatial/reference/ddbs_difference.md)
  : Calculates the difference of two geometries
- [`ddbs_intersection()`](https://cidree.github.io/duckspatial/reference/ddbs_intersection.md)
  : Calculates the intersection of two geometries
- [`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md)
  : Performs spatial joins of two geometries

## Spatial operations (unary)

- [`ddbs_boundary()`](https://cidree.github.io/duckspatial/reference/ddbs_boundary.md)
  : Returns the boundary of geometries
- [`ddbs_buffer()`](https://cidree.github.io/duckspatial/reference/ddbs_buffer.md)
  : Creates a buffer around geometries
- [`ddbs_centroid()`](https://cidree.github.io/duckspatial/reference/ddbs_centroid.md)
  : Calculates the centroid of geometries

## Spatial operations (measures)

- [`ddbs_area()`](https://cidree.github.io/duckspatial/reference/ddbs_area.md)
  : Calculates the area of geometries
- [`ddbs_length()`](https://cidree.github.io/duckspatial/reference/ddbs_length.md)
  : Calculates the length of geometries

## Data manipulation

- [`ddbs_filter()`](https://cidree.github.io/duckspatial/reference/ddbs_filter.md)
  : Spatial Filter
- [`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md)
  : Performs spatial joins of two geometries

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

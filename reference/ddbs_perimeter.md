# Calculate the perimeter of geometries

Computes the perimeter of polygon geometries in meters.

## Usage

``` r
ddbs_perimeter(
  x,
  conn = NULL,
  name = NULL,
  new_column = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  output = NULL,
  overwrite = FALSE,
  quiet = FALSE
)
```

## Arguments

- x:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

  Data is returned from this object.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- new_column:

  Name of the new column to create on the input data. If NULL, the
  function will return a vector with the result

- crs:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) The coordinates
  reference system of the data. Specify if the data doesn't have a
  `crs_column`, and you know the CRS.

- crs_column:

  [Deprecated](https://rdrr.io/r/base/Deprecated.html) a character
  string of length one specifying the column storing the CRS (created
  automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- output:

  Character. Controls the return type. Options:

  - `"duckspatial_df"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB

  - `"sf"`: Eagerly collected sf object (uses memory)

  - `"tibble"`: Eagerly collected tibble without geometry

  - `"raw"`: Eagerly collected tibble with WKB geometry (list of raw
    vectors)

  - `"geoarrow"`: Eagerly collected tibble with geoarrow geometry
    (geoarrow_vctr)

  Can be set globally via
  [`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)`(output_type = "...")`
  or per-function via this argument. Per-function overrides global
  setting.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

When `new_column = NULL` it returns a `units` vector in \\m\\. When
`new_column` is not NULL, the output depends on the `output` argument
(or global preference set by
[`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)):

- `duckspatial_df` (default): A lazy spatial data frame backed by
  dbplyr/DuckDB.

- `sf`: An eagerly collected `sf` object in R memory.

- `tibble`: An eagerly collected `tibble` without geometry in R memory.

- `raw`: An eagerly collected `tibble` with WKB geometry (no
  conversion).

- `geoarrow`: An eagerly collected `tibble` with geometry converted to
  `geoarrow_vctr`.

When `name` is provided, the result is also written as a table or view
in DuckDB and the function returns `TRUE` (invisibly).

## Details

When the input geometry is in `EPSG:4326`, the function uses
`ST_Perimeter_Spheroid`, which uses the GeographicLib library for
calculating the perimeter using an ellipsoidal model of the earth. This
method is highly accurate for calculating the perimeter of a polygon
geometry considering the curvature of the earth, but it's also the
slowest.

If the input geometry is in a projected CRS, the function will use
`ST_Perimeter` to calculate the perimeter in meters.

In other cases, the function will use `ST_Perimeter_Spheroid` and
display a warning.

## References

<https://geographiclib.sourceforge.io/>

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
argentina_ddbs <- ddbs_open_dataset(
  system.file("spatial/argentina.geojson", 
  package = "duckspatial")
) |>
  ddbs_transform("EPSG:3857")

## store in duckdb
ddbs_write_vector(conn, argentina_ddbs, "argentina")

## calculate perimeter (returns sf object with perimeter column)
ddbs_perimeter("argentina", conn)

## calculate perimeter with custom column name
ddbs_perimeter("argentina", conn, new_column = "perimeter_m")

## create a new table with perimeter calculations
ddbs_perimeter("argentina", conn, name = "argentina_with_perimeter", new_column = "perimeter_m")

## calculate perimeter in a sf object
ddbs_perimeter(argentina_ddbs)
} # }
```

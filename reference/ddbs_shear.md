# Shear geometries

Applies a shear transformation to geometries, shifting coordinates
proportionally in the X and Y directions. By default, shearing is
applied relative to the centroid of all geometries; if
`by_feature = TRUE`, each geometry is sheared relative to its own
centroid.

## Usage

``` r
ddbs_shear(
  x,
  x_shear = 0,
  y_shear = 0,
  by_feature = FALSE,
  conn = NULL,
  name = NULL,
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

- x_shear:

  numeric value specifying the shear factor in the X direction (default
  = 0). For each unit in Y, X coordinates are shifted by this amount

- y_shear:

  numeric value specifying the shear factor in the Y direction (default
  = 0). For each unit in X, Y coordinates are shifted by this amount

- by_feature:

  Logical. If `TRUE`, the geometric operation is applied separately to
  each geometry. If `FALSE`, the geometric operation is applied to the
  data as a whole.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

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

Depends on the `output` argument (or global preference set by
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

## Examples

``` r
if (FALSE) { # \dontrun{
## load packages
library(duckspatial)
library(dplyr)

# create a duckdb database in memory (with spatial extension)
conn <- ddbs_create_conn(dbdir = "memory")

## read data
countries_ddbs <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", 
  package = "duckspatial")
) |>
  filter(CNTR_ID %in% c("PT", "ES", "FR", "IT"))

## store in duckdb
ddbs_write_vector(conn, countries_ddbs, "countries")

## shear in X direction (creates italic-like effect)
ddbs_shear(conn = conn, "countries", x_shear = 0.3, y_shear = 0)

## shear in Y direction
ddbs_shear(conn = conn, "countries", x_shear = 0, y_shear = 0.3)

## shear in both directions
ddbs_shear(conn = conn, "countries", x_shear = 0.2, y_shear = 0.2)

## shear without using a connection
ddbs_shear(countries_ddbs, x_shear = 0.3, y_shear = 0)
} # }
```

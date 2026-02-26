# Generate point geometries from coordinates

Converts a data frame with coordinate columns into spatial point
geometries.

## Usage

``` r
ddbs_as_spatial(
  x,
  coords = c("lon", "lat"),
  crs = "EPSG:4326",
  conn = NULL,
  name = NULL,
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

- coords:

  Character vector of length 2 specifying the names of the longitude and
  latitude columns (or X and Y coordinates). Defaults to
  `c("lon", "lat")`.

- crs:

  Character or numeric. The Coordinate Reference System (CRS) of the
  input coordinates. Can be specified as an EPSG code (e.g.,
  `"EPSG:4326"` or `4326`) or a WKT string. Defaults to `"EPSG:4326"`
  (WGS84 longitude/latitude).

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

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

## create sample data with coordinates
cities_df <- data.frame(
  city = c("Buenos Aires", "Córdoba", "Rosario"),
  lon = c(-58.3816, -64.1811, -60.6393),
  lat = c(-34.6037, -31.4201, -32.9468),
  population = c(3075000, 1391000, 1193605)
)

# option 1: convert data frame to sf object
cities_ddbs <- ddbs_as_spatial(cities_df)

# specify custom coordinate column names
cities_df2 <- data.frame(
  city = c("Mendoza", "Tucumán"),
  longitude = c(-68.8272, -65.2226),
  latitude = c(-32.8895, -26.8241)
)

ddbs_as_spatial(cities_df2, coords = c("longitude", "latitude"))


## option 2: convert table in duckdb to spatial table

# create a duckdb connection and write data
conn <- duckspatial::ddbs_create_conn()
DBI::dbWriteTable(conn, "cities_tbl", cities_df, overwrite = TRUE)

# convert to spatial table in database
ddbs_as_spatial(
    x = "cities_tbl",
    conn = conn,
    name = "cities_spatial",
    overwrite = TRUE
)

# read the spatial table
ddbs_read_vector(conn, "cities_spatial")
} # }
```

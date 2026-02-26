# Calculate the distance between geometries

Computes the distance between two geometries, automatically using an
appropriate measurement based on their coordinate reference system and
geometry type.

## Usage

``` r
ddbs_distance(
  x,
  y,
  dist_type = NULL,
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
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

- y:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

- dist_type:

  Character. Distance type to be calculated. By the default it uses the
  best option for the input CRS (see details).

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- conn_x:

  A `DBIConnection` object to a DuckDB database for the input `x`. If
  `NULL` (default), it is resolved from `conn` or extracted from `x`.

- conn_y:

  A `DBIConnection` object to a DuckDB database for the input `y`. If
  `NULL` (default), it is resolved from `conn` or extracted from `y`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

A `units` matrix in meters

## Details

The `dist_type` argument can take any of the following values:

- `NULL` (default): it will use the best option for the inputs CRS

- `"planar"`: planar distance between two geometries (default for
  projected CRS)

- `"geos"`: planar distance between two geometries using GEOS library

- `"haversine"`: returns the great circle distance. Requires the input
  to be in WGS84 (EPSG:4326) and POINT geometry (default for EPSG:4326).

- `"spheroid"`: returns the distance using an ellipsoidal model of the
  earth's surface using the GeographicLib library. It's highly accurate
  but the slowest

Note that geometries different than POINT are not supported by
"haversine" nor "spheroid".

## Examples

``` r
if (FALSE) { # \dontrun{
# load packages
library(duckspatial)

# create points data
n <- 10
points_sf <- data.frame(
    id = 1:n,
    x = runif(n, min = -180, max = 180),
    y = runif(n, min = -90, max = 90)
) |>
    ddbs_as_spatial(coords = c("x", "y"), crs = "EPSG:4326")

# option 1: passing sf objects
output1 <- duckspatial::ddbs_distance(
    x = points_sf,
    y = points_sf,
    dist_type = "haversine"
)

head(output1)


## option 2: passing the names of tables in a duckdb db and output as sf

# creates a duckdb
conn <- duckspatial::ddbs_create_conn()

# write sf to duckdb
ddbs_write_vector(conn, points_sf, "points", overwrite = TRUE)

output2 <- ddbs_distance(
    conn = conn,
    x = "points",
    y = "points",
    dist_type = "haversine"
)
head(output2)

} # }
```

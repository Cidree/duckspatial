# Returns the distance between two geometries

Returns the planar or haversine distance between two geometries, and
returns a `data.frame` object or creates a new table in a DuckDB
database.

## Usage

``` r
ddbs_distance(x, y, dist_type = "haversine", conn = NULL, quiet = FALSE)
```

## Arguments

- x:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`. Data is returned from this object.

- y:

  An `sf` spatial object. Alternatively, it can be a string with the
  name of a table with geometry column within the DuckDB database
  `conn`.

- dist_type:

  String. One of `c("planar", "haversine")`. Defaults to `"haversine"`
  and returns distance in meters, but the input is expected to be in
  WGS84 (EPSG:4326) coordinates. The option `"haversine"` only accepts
  `POINT` geometries. When `dist_type = "planar"`, distances estimates
  are in the same unit as the coordinate reference system (CRS) of the
  input.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

A `data.frame` object or `TRUE` (invisibly) for table creation

## Examples

``` r
if (FALSE) { # \dontrun{
# load packages
library(duckspatial)
library(sf)

# create points data
n <- 10
points_sf <- data.frame(
    id = 1:n,
    x = runif(n, min = -180, max = 180),
    y = runif(n, min = -90, max = 90)
) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)

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

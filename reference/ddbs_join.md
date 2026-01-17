# Performs spatial joins of two geometries

Performs spatial joins of two geometries, and returns a `sf` object or
creates a new table in a DuckDB database.

## Usage

``` r
ddbs_join(
  x,
  y,
  join = "intersects",
  conn = NULL,
  name = NULL,
  crs = NULL,
  crs_column = "crs_duckspatial",
  overwrite = FALSE,
  quiet = FALSE
)
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

- join:

  A geometry predicate function. Defaults to `"intersects"`. See the
  details for other options.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If it's `NULL` (the default), it will return the result as an
  `sf` object.

- crs:

  The coordinates reference system of the data. Specify if the data
  doesn't have a `crs_column`, and you know the CRS.

- crs_column:

  a character string of length one specifying the column storing the CRS
  (created automatically by
  [`ddbs_write_vector`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)).
  Set to `NULL` if absent.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

an sf object or TRUE (invisibly) for table creation

## Details

Spatial Join Predicates:

A spatial predicate is really just a function that evaluates some
spatial relation between two geometries and returns true or false, e.g.,
“does a contain b” or “is a within distance x of b”. Here is a quick
overview of the most commonly used ones, taking two geometries a and b:

- `"ST_Intersects"`: Whether a intersects b

- `"ST_Contains"`: Whether a contains b

- `"ST_ContainsProperly"`: Whether a contains b without b touching a's
  boundary

- `"ST_Within"`: Whether a is within b

- `"ST_Overlaps"`: Whether a overlaps b

- `"ST_Touches"`: Whether a touches b

- `"ST_Equals"`: Whether a is equal to b

- `"ST_Crosses"`: Whether a crosses b

- `"ST_Covers"`: Whether a covers b

- `"ST_CoveredBy"`: Whether a is covered by b

- `"ST_DWithin"`: x) Whether a is within distance x of b

## Examples

``` r
if (FALSE) { # \dontrun{
# load packages
library(duckspatial)
library(sf)

# read polygons data
countries_sf <- sf::st_read(system.file("spatial/countries.geojson", package = "duckspatial"))

# create points data
n <- 100
points_sf <- data.frame(
    id = 1:n,
    x = runif(n, min = -180, max = 180),
    y = runif(n, min = -90, max = 90)
) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)



# option 1: passing sf objects
output1 <- duckspatial::ddbs_join(
    x = points_sf,
    y = countries_sf,
    join = "within"
)

plot(output1["CNTR_NAME"])


## option 2: passing the names of tables in a duckdb db

# creates a duckdb
conn <- duckspatial::ddbs_create_conn()

# write sf to duckdb
ddbs_write_vector(conn, points_sf, "points", overwrite = TRUE)
ddbs_write_vector(conn, countries_sf, "countries", overwrite = TRUE)

# spatial join
output2 <- ddbs_join(
    conn = conn,
    x = "points",
    y = "countries",
    join = "within"
)

plot(output2["CNTR_NAME"])

} # }
```

# Perform a spatial join of two geometries

Combines two sets of geometries based on spatial relationships, such as
intersection or containment, attaching attributes from one set to the
other.

## Usage

``` r
ddbs_join(
  x,
  y,
  join = "intersects",
  conn = NULL,
  conn_x = NULL,
  conn_y = NULL,
  name = NULL,
  mode = NULL,
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

- y:

  Input spatial data. Can be:

  - A `duckspatial_df` object (lazy spatial data frame via dbplyr)

  - An `sf` object

  - A `tbl_lazy` from dbplyr

  - A character string naming a table/view in `conn`

- join:

  A geometry predicate function. Defaults to `"intersects"`. See the
  details for other options.

- conn:

  A connection object to a DuckDB database. If `NULL`, the function runs
  on a temporary DuckDB database.

- conn_x:

  A `DBIConnection` object to a DuckDB database for the input `x`. If
  `NULL` (default), it is resolved from `conn` or extracted from `x`.

- conn_y:

  A `DBIConnection` object to a DuckDB database for the input `y`. If
  `NULL` (default), it is resolved from `conn` or extracted from `y`.

- name:

  A character string of length one specifying the name of the table, or
  a character string of length two specifying the schema and table
  names. If `NULL` (the default), the function returns the result as an
  `sf` object

- mode:

  Character. Controls the return type. Options:

  - `"duckspatial"` (default): Lazy spatial data frame backed by
    dbplyr/DuckDB

  - `"sf"`: Eagerly collected sf object (uses memory)

  Can be set globally via
  [`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)`(mode = "...")`
  or per-function via this argument. Per-function overrides global
  setting.

- overwrite:

  Boolean. whether to overwrite the existing table if it exists.
  Defaults to `FALSE`. This argument is ignored when `name` is `NULL`.

- quiet:

  A logical value. If `TRUE`, suppresses any informational messages.
  Defaults to `FALSE`.

## Value

Depends on the `mode` argument (or global preference set by
[`ddbs_options`](https://cidree.github.io/duckspatial/reference/ddbs_options.md)):

- `duckspatial` (default): A `duckspatial_df` (lazy spatial data frame)
  backed by dbplyr/DuckDB.

- `sf`: An eagerly collected object in R memory, that will return the
  same data type as the `sf` equivalent (e.g. `sf` or `units` vector).

When `name` is provided, the result is also written as a table or view
in DuckDB and the function returns `TRUE` (invisibly).

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
# RECOMMENDED: Efficient lazy workflow using ddbs_open_dataset
library(duckspatial)

# Load data directly as lazy spatial data frames (CRS auto-detected)
countries <- ddbs_open_dataset(
  system.file("spatial/countries.geojson", package = "duckspatial")
)

# Create random points
n <- 100
points <- data.frame(
    id = 1:n,
    x = runif(n, min = -180, max = 180),
    y = runif(n, min = -90, max = 90)
) |> 
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) |>
  as_duckspatial_df()

# Lazy join - computation stays in DuckDB
result <- ddbs_join(points, countries, join = "within")

# Collect to sf when needed
result_sf <- dplyr::collect(result) |> sf::st_as_sf()
plot(result_sf["CNTR_NAME"])


# Alternative: using sf objects directly (legacy compatibility)
library(sf)

countries_sf <- sf::st_read(system.file("spatial/countries.geojson", package = "duckspatial"))

output <- duckspatial::ddbs_join(
    x = points,
    y = countries_sf,
    join = "within"
)


# Alternative: using table names in a duckdb connection
conn <- duckspatial::ddbs_create_conn()

ddbs_write_table(conn, points, "points", overwrite = TRUE)
ddbs_write_table(conn, countries_sf, "countries", overwrite = TRUE)

output2 <- ddbs_join(
    conn = conn,
    x = "points",
    y = "countries",
    join = "within"
)

} # }
```

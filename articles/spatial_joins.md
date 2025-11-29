# Spatial joins

This vignette shows how to use
[`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md)
to perform fast spatial join operations on large data sets with three
different approaches:

1 **In-memory**: pass `sf` objects and get an `sf` result (DuckDB runs
under the hood, no persistent DataBase). 2. **Connected**: pass table
names stored in an existing DuckDB connection and get an `sf` result. 3.
**Write-to-DB**: same as (2), but write the result to a new DuckDB
table.

Let’s see a few examples. First, let’s load a few libraries and our
sample data:

``` r
library(duckspatial)
# library(mapview)
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

# polygons
countries_sf  <- sf::st_read(
    system.file("spatial/countries.geojson",  package = "duckspatial"),
    quiet = TRUE
    )

# random points
set.seed(42)
n <- 10000
points_sf <- data.frame(
  id = 1:n,
  x  = runif(n, min = -180, max = 180),
  y  = runif(n, min =  -90, max =  90)
) |>
  sf::st_as_sf(coords = c("x","y"), crs = 4326)
```

## 1) In-memory: pass `sf`, return `sf`

The simplest way to perform fast spatial join. You simply pass two `sf`
objects, and
[`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md)
spins up a temporary DuckDB, runs the join, and returns an `sf.`

- **When to use:** quick analysis, prototyping, or when you don’t need
  to persist intermediate tables.

``` r
out_sf1 <- ddbs_join(
  x    = points_sf,
  y    = countries_sf,
  join = "within"
)

# quick peek
# mapview(out_sf1, zcol="NAME_ENGL")
```

## 2) Connected: pass table names in DuckDB, return `sf`

In the second and third approaches, we make use of a connection to an
existing DuckDB database. So let’s create a fresh DuckDB connection
using the
[`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md)
function, which automatically install and load DuckDB spatial extension
to the connection.

``` r
# create a fresh DuckDB connection
conn <- duckspatial::ddbs_create_conn()
```

Now, in this second approach you need first to write your layers to
DuckDB, and perform the spatial join by referencing their table names.
Like this:

``` r
# write data to DuckDB
ddbs_write_vector(conn, points_sf,   "points",    overwrite = TRUE)
ddbs_write_vector(conn, countries_sf, "countries", overwrite = TRUE)

# spatial join inside DuckDB; result returned as sf
out_sf2 <- ddbs_join(
  conn,
  x    = "points",
  y    = "countries",
  join = "within"
)
```

- **When to use:** iterative workflows, larger-than-memory data, or when
  you’ll run multiple queries on the same tables.

## 3) Write-to-DB: create a new table with the join result

The output of approaches 1 and 2 is an `sf` object loaded to your
memory. In this third approach,
[`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md)
writes a new table in the DuckDB database. You simply need to the `name`
of the new table.

``` r
ddbs_join(
    conn = conn,
    x = "points",
    y = "countries",
    join = "within",
    name = "points_in_countries",
    overwrite = TRUE
)

# use the result in SQL (or read back as sf later)
# DBI::dbReadTable(conn, "points_in_countries") |>
#     sf::st_as_sf(wkt = 'geometry') |> 
#     head()
```

- **When to use:** iterative workflows, larger-than-memory data, or when
  you’ll run multiple queries on the same tables.

## Spatial Join Predicates:

A spatial predicate is really just a function that evaluates some
spatial relation between two geometries and returns true or false, e.g.,
“does a contain b” or “is a within distance x of b”. The `join` argument
accepts the spatial predicates:

- `"ST_Intersects"`: Whether a intersects b
- `"ST_Contains"`: Whether a contains b
- `"ST_ContainsProperly"`: Whether a contains b without b touching a’s
  boundary
- `"ST_Within"`: Whether a is within b
- `"ST_Overlaps"`: Whether a overlaps b
- `"ST_Touches"`: Whether a touches b
- `"ST_Equals"`: Whether a is equal to b
- `"ST_Crosses"`: Whether a crosses b
- `"ST_Covers"`: Whether a covers b
- `"ST_CoveredBy"`: Whether a is covered by b
- `"ST_DWithin"`: x) Whether a is within distance x of b

## Clean up

Don’t forget to disconnect from the database.

``` r
duckdb::dbDisconnect(conn)
```

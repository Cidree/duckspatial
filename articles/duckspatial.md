# Intro to duckspatial

The **{duckspatial}** package provides fast and memory-efficient
functions to analyze and manipulate large spatial vector datasets in R.
It allows R users to benefit directly from the analytical power of
[DuckDB and its spatial
extension](https://duckdb.org/docs/stable/core_extensions/spatial/functions),
while remaining fully compatible with R’s spatial ecosystem, especially
**{sf}**.

At its core, **{duckspatial}** bridges two worlds:

- R spatial workflows based on {sf} objects
- Database-backed spatial analytics powered by DuckDB SQL

This design makes **{duckspatial}** especially well suited for:

- Working with large spatial data sets
- Speeding up spatial analysis at scale
- Workflows with larger-than memory data

## Installation

You can install **{duckspatial}** directly from CRAN with:

``` r
install.packages("duckspatial")
```

Or you can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Cidree/duckspatial")
```

## Core idea: flexible spatial workflows

A central design principle of **{duckspatial}** is that the same spatial
operation can be used in different ways, depending on how your data is
stored and how you want to manage memory and performance. Most functions
in **{duckspatial}** support four complementary workflows:

1.  Input`sf` → Output `sf`
2.  Input `sf` → Output DuckDB table
3.  Input DuckDB table → Output `sf`
4.  Input DuckDB table → Output DuckDB table

Let’s see a few examples to illustrate these workflows with a few sample
data sets.

``` r
library(duckspatial)
library(sf)

# polygons
countries_sf  <- sf::st_read(
    system.file("spatial/countries.geojson",  package = "duckspatial"),
    quiet = TRUE
    )

# create random points
set.seed(42)
n <- 10000
points_sf <- data.frame(
  id = 1:n,
  x  = runif(n, min = -180, max = 180),
  y  = runif(n, min =  -90, max =  90)
) |>
  sf::st_as_sf(coords = c("x","y"), crs = 4326)
```

### Workflow 1: `sf` input → `sf` output

The simplest way to perform fast spatial operations. Here you pass `sf`
objects as inputs, and under the hood {duckspatial}:

- Registers them temporarily in DuckDB
- Executes the spatial operation using SQL
- Returns the result as an {sf} object

In this example, we use
[`ddbs_join()`](https://cidree.github.io/duckspatial/reference/ddbs_join.md)
(which is equivalent to
[`sf::st_join`](https://r-spatial.github.io/sf/reference/st_join.html))
to determine which country is intersected by each point.

``` r
result_sf <- ddbs_join(
  x = points_sf,
  y = countries_sf,
  join = "intersects"
)

head(result_sf)
#> Simple feature collection with 6 features and 8 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -72.75607 ymin: -50.15994 xmax: -67.79479 ymax: -43.01591
#> Geodetic CRS:  WGS 84
#>     id CNTR_ID NAME_ENGL ISO3_CODE CNTR_NAME FID       date crs_duckspatial
#> 1  708      AR Argentina       ARG Argentina  AR 2021-01-01       EPSG:4326
#> 2 3041      AR Argentina       ARG Argentina  AR 2021-01-01       EPSG:4326
#> 3 9309      AR Argentina       ARG Argentina  AR 2021-01-01       EPSG:4326
#> 4 2446      AR Argentina       ARG Argentina  AR 2021-01-01       EPSG:4326
#> 5 8456      AR Argentina       ARG Argentina  AR 2021-01-01       EPSG:4326
#> 6 2707      AR Argentina       ARG Argentina  AR 2021-01-01       EPSG:4326
#>                      geometry
#> 1 POINT (-72.75607 -50.15994)
#> 2 POINT (-68.77194 -48.12303)
#> 3 POINT (-70.82253 -46.23146)
#> 4  POINT (-71.6444 -44.25323)
#> 5 POINT (-71.18679 -44.60882)
#> 6 POINT (-67.79479 -43.01591)
```

- **When to use:** quick analysis, prototyping, or when you don’t need
  to persist intermediate tables.

### Creating a DuckDB connection

The next workflows use a DuckDB connection, which makes these workflows
much more efficient for working with large spatial data sets in general.
To create a DuckDB connection, we use the
[`ddbs_create_conn()`](https://cidree.github.io/duckspatial/reference/ddbs_create_conn.md)
function, which automatically creates the database connection and
installs / loads DuckDB’s spatial extension in a single call.

``` r
# create duckdb con and install / load spatial extension
conn <- duckspatial::ddbs_create_conn()
```

### Workflow 2: `sf` input → DuckDB table

This workflow is ideal when you start in R, but want to persist results
efficiently in the database without load the results to memory. You pass
`sf` objects as input, and {duckspatial} writes the output directly to
DuckDB.

The only difference is that here you also pass the `name` of the table
that should be written with the output to DuckDB and the database `conn`
where the table should saved.

``` r
ddbs_join(
    conn = conn,
    x = points_sf,
    y = countries_sf,
    join = "intersects", 
    name = "points_in_countries_tbl"
)
```

And if you want to fetch the table to memory, the
[`ddbs_read_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_read_vector.md)
allows you to read a table and return it as a `sf` object.

``` r
tbl <- ddbs_read_vector(
    conn = conn,
    name = "points_in_countries_tbl"
    )

head(tbl)
#> Simple feature collection with 6 features and 8 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -72.75607 ymin: -50.15994 xmax: -67.79479 ymax: -43.01591
#> Geodetic CRS:  WGS 84
#>     id CNTR_ID NAME_ENGL ISO3_CODE CNTR_NAME FID       date crs_duckspatial_1
#> 1  708      AR Argentina       ARG Argentina  AR 2021-01-01         EPSG:4326
#> 2 3041      AR Argentina       ARG Argentina  AR 2021-01-01         EPSG:4326
#> 3 9309      AR Argentina       ARG Argentina  AR 2021-01-01         EPSG:4326
#> 4 2446      AR Argentina       ARG Argentina  AR 2021-01-01         EPSG:4326
#> 5 8456      AR Argentina       ARG Argentina  AR 2021-01-01         EPSG:4326
#> 6 2707      AR Argentina       ARG Argentina  AR 2021-01-01         EPSG:4326
#>                      geometry
#> 1 POINT (-72.75607 -50.15994)
#> 2 POINT (-68.77194 -48.12303)
#> 3 POINT (-70.82253 -46.23146)
#> 4  POINT (-71.6444 -44.25323)
#> 5 POINT (-71.18679 -44.60882)
#> 6 POINT (-67.79479 -43.01591)
```

### Workflow 3: DuckDB tables → `sf` output

In this workflow, your spatial data lives inside DuckDB as tables but
you want to return the output of `sf` objects to memory.

You can easily write `sf` objects as tables to duckdb with the
[`ddbs_write_vector()`](https://cidree.github.io/duckspatial/reference/ddbs_write_vector.md)
function by passing the `sf` data and indicating the `name` of the table
to be written in the database.

``` r
# write `sf` objects as tables to duckdb
duckspatial::ddbs_write_vector(
    conn = conn, 
    data = countries_sf, 
    name = "countries"
    )

duckspatial::ddbs_write_vector(
    conn = conn, 
    data = points_sf, 
    name = "points"
    )
```

To perform a spatial operation, you pass the table names and you get an
`sf` object back.

``` r
result_sf <- ddbs_join(
  conn = conn,
  x = "points",
  y = "countries",
  join = "intersects"
  )
```

- **When to use:** iterative workflows, larger-than-memory data, or when
  you’ll run multiple queries on the same tables.

### Workflow 4: DuckDB tables → DuckDB table

This is the fastest and most scalable workflow. The entire computation
happens inside DuckDB, and the result is written to a new database
table.

``` r
ddbs_join(
  conn = conn,
  x = "points",
  y = "countries",
  join = "intersects", 
  name = "points_in_countries_tbl", 
  overwrite = TRUE
  )


# and read the table to memory as sf
# tbl <- ddbs_read_vector(
#     conn = conn,
#     name = "points_in_countries_tbl"
#     )
```

- **When to use:** Very large datasets, Production pipelines with
  multiple steps and when results will be reused downstream in DuckDB

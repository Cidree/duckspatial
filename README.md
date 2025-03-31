
<!-- README.md is generated from README.Rmd. Please edit that file -->

# duckspatial

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/duckspatial)](https://CRAN.R-project.org/package=duckspatial)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/Cidree/duckspatial/graph/badge.svg)](https://app.codecov.io/gh/Cidree/duckspatial)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

**duckspatial** is an R package that simplifies the process of reading
and writing vector spatial data (e.g., `sf` objects) in a
[DuckDB](https://duckdb.org/) database. This package is designed for
users working with geospatial data who want to leverage DuckDB’s fast
analytical capabilities while maintaining compatibility with R’s spatial
data ecosystem.

## Installation

You can install the development version of duckspatial from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Cidree/duckspatial")
```

## Example

This is a basic example which shows how to set up DuckDB for spatial
data manipulation, and how to write/read vector data.

``` r
library(duckdb)
#> Warning: package 'duckdb' was built under R version 4.4.3
#> Cargando paquete requerido: DBI
library(duckspatial)
library(sf)
#> Linking to GEOS 3.12.2, GDAL 3.9.3, PROJ 9.4.1; sf_use_s2() is TRUE
```

First, we create a connection with a DuckDB database (in this case in
memory database), and we make sure that the spatial extension is
installed, and we load it:

``` r
## create connection
conn <- dbConnect(duckdb())

## install and load spatial extension
ddbs_install(conn)
#> ℹ spatial extension version <76dc6da> is already installed in this database
ddbs_load(conn)
#> ✔ Spatial extension loaded
```

Now we can get some data to insert into the database. We are creating
1,000,000 random points.

``` r
## create n points
n <- 1000000
random_points <- data.frame(
  id = 1:n,
  x = runif(n, min = -180, max = 180),  # Random longitude values
  y = runif(n, min = -90, max = 90)     # Random latitude values
)

## convert to sf
sf_points <- st_as_sf(random_points, coords = c("x", "y"), crs = 4326)

## view first rows
head(sf_points)
#> Simple feature collection with 6 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -170.0372 ymin: -77.78096 xmax: 154.213 ymax: 35.45508
#> Geodetic CRS:  WGS 84
#>   id                    geometry
#> 1  1   POINT (-127.601 35.45508)
#> 2  2   POINT (154.213 -77.78096)
#> 3  3 POINT (-76.63392 -40.25935)
#> 4  4  POINT (-170.0372 22.87667)
#> 5  5 POINT (-86.79235 -22.43082)
#> 6  6  POINT (87.95789 -9.292341)
```

Now we can insert the data into the database using the
`ddbs_write_vector()` function. We use the `proc.time()` function to
calculate how long does it take, and we can compare it with writing a
shapefile with the `write_sf()` function:

``` r
## write data monitoring processing time
start_time <- proc.time()
ddbs_write_vector(conn, sf_points, "test_points")
#> ✔ Table test_points successfully imported
end_time <- proc.time()

## print elapsed time
elapsed_duckdb <- end_time["elapsed"] - start_time["elapsed"]
print(elapsed_duckdb)
#> elapsed 
#>    3.88
```

``` r
## write data monitoring processing time
start_time <- proc.time()
shpfile <- tempfile(fileext = ".shp")
write_sf(sf_points, shpfile)
end_time <- proc.time()

## print elapsed time
elapsed_shp <- end_time["elapsed"] - start_time["elapsed"]
print(elapsed_shp)
#> elapsed 
#>   13.97
```

In this case, we can see that DuckDB was 3.6 times faster. Now we will
do the same exercise but reading the data back into R:

``` r
## write data monitoring processing time
start_time <- proc.time()
sf_points_ddbs <- ddbs_read_vector(conn, "test_points", crs = 4326)
#> ✔ Table test_points successfully imported.
end_time <- proc.time()

## print elapsed time
elapsed_duckdb <- end_time["elapsed"] - start_time["elapsed"]
print(elapsed_duckdb)
#> elapsed 
#>   15.61
```

``` r
## write data monitoring processing time
start_time     <- proc.time()
sf_points_ddbs <- read_sf(shpfile)
end_time       <- proc.time()

## print elapsed time
elapsed_shp <- end_time["elapsed"] - start_time["elapsed"]
print(elapsed_shp)
#> elapsed 
#>      32
```

For reading, we get a factor of 2 times faster for DuckDB. Finally,
don’t forget to disconnect from the database:

``` r
dbDisconnect(conn)
```

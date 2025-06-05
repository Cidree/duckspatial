
<!-- README.md is generated from README.Rmd. Please edit that file -->

# duckspatial <a href="https://cidree.github.io/duckspatial/"><img src="man/figures/logo.png" align="right" height="138" alt="duckspatial website" /></a>

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
[![Last Month
Downloads](https://cranlogs.r-pkg.org/badges/last-month/duckspatial?color=green)](https://CRAN.R-project.org/package=duckspatial)
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
#> Cargando paquete requerido: DBI
library(duckspatial)
library(sf)
#> Linking to GEOS 3.13.1, GDAL 3.10.2, PROJ 9.5.1; sf_use_s2() is TRUE
```

First, we create a connection with a DuckDB database (in this case in
memory database), and we make sure that the spatial extension is
installed, and we load it:

``` r
## create connection
conn <- dbConnect(duckdb())

## install and load spatial extension
ddbs_install(conn)
#> ℹ spatial extension version <2905968> is already installed in this database
ddbs_load(conn)
#> ✔ Spatial extension loaded
```

Now we can get some data to insert into the database. We are creating
10,000,000 random points.

``` r
## random word generator
random_word <- function(length = 5) {
    paste0(sample(letters, length, replace = TRUE), collapse = "")
}

## create n points
n <- 10000000
random_points <- data.frame(
  id = 1:n,
  x = runif(n, min = -180, max = 180),  
  y = runif(n, min = -90, max = 90),
  a = sample(1:1000000, size = n, replace = TRUE),
  b = sample(replicate(10, random_word(7)), size = n, replace = TRUE),
  c = sample(replicate(10, random_word(9)), size = n, replace = TRUE)
)

## convert to sf
sf_points <- st_as_sf(random_points, coords = c("x", "y"), crs = 4326)

## view first rows
head(sf_points)
#> Simple feature collection with 6 features and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -123.6892 ymin: -81.28037 xmax: 161.5825 ymax: 42.83173
#> Geodetic CRS:  WGS 84
#>   id      a       b         c                    geometry
#> 1  1 458064 svdtjpt fmuwkbvzb   POINT (96.27221 42.83173)
#> 2  2 183934 kugswkz fmuwkbvzb POINT (-98.39448 -52.03544)
#> 3  3 101830 ewtbqed whwecpqsj POINT (-108.6723 -21.72314)
#> 4  4 471166 kugswkz myfcqkndt  POINT (-123.6892 20.54316)
#> 5  5 672502 jxkzoyf xnrnbcigo POINT (-91.60747 -56.17601)
#> 6  6 108727 aupymig xamjuqius  POINT (161.5825 -81.28037)
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
#>   15.73
```

``` r
## write data monitoring processing time
start_time <- proc.time()
gpkg_file <- tempfile(fileext = ".gpkg")
write_sf(sf_points, gpkg_file)
end_time <- proc.time()

## print elapsed time
elapsed_gpkg <- end_time["elapsed"] - start_time["elapsed"]
print(elapsed_gpkg)
#> elapsed 
#>  180.51
```

In this case, we can see that DuckDB was 11.5 times faster. Now we will
do the same exercise but reading the data back into R:

``` r
## write data monitoring processing time
start_time <- proc.time()
sf_points_ddbs <- ddbs_read_vector(conn, "test_points")
#> ✔ Table test_points successfully imported.
end_time <- proc.time()

## print elapsed time
elapsed_duckdb <- end_time["elapsed"] - start_time["elapsed"]
print(elapsed_duckdb)
#> elapsed 
#>   56.29
```

``` r
## write data monitoring processing time
start_time     <- proc.time()
sf_points_ddbs <- read_sf(gpkg_file)
end_time       <- proc.time()

## print elapsed time
elapsed_gpkg <- end_time["elapsed"] - start_time["elapsed"]
print(elapsed_gpkg)
#> elapsed 
#>    50.7
```

For reading, we got similar results. Finally, don’t forget to disconnect
from the database:

``` r
dbDisconnect(conn)
```

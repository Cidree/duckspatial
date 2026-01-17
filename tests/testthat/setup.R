# skip tests on CRAN because they take too much time
# skip_if(Sys.getenv("TEST_ONE") != "")
# testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

# read polygons data from duckspatial package
countries_sf <- sf::st_read(system.file("spatial/countries.geojson", package = "duckspatial"))
countries_sf <- subset(countries_sf, CNTR_ID %in% c("AR", "BR", "BO", "PE", "PY", "UY", "CL"))
argentina_sf <- sf::st_read(system.file("spatial/argentina.geojson", package = "duckspatial"))
argentina_ddbs <- duckspatial::ddbs_open_dataset(system.file("spatial/argentina.geojson", package = "duckspatial"))

# read lines data
rivers_sf <- sf::st_read(system.file("spatial/rivers.geojson", package = "duckspatial"))
rivers_ddbs <- duckspatial::ddbs_open_dataset(system.file("spatial/rivers.geojson", package = "duckspatial"))

## create points data
set.seed(42)
n <- 1000
points_sf <- data.frame(
    id = 1:n,
    x = runif(n, min = -180, max = 180),
    y = runif(n, min = -90, max = 90)
) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)

points_ddbs <- duckspatial::as_duckspatial_df(points_sf)

# North Carolina data from sf package - used by duckspatial_df tests
nc_sf   <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
nc_ddbs <- duckspatial::ddbs_open_dataset(system.file("shape/nc.shp", package = "sf"))

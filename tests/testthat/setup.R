# skip tests on CRAN because they take too much time
# skip_if(Sys.getenv("TEST_ONE") != "")
# testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

# read polygons data from duckspatial package
countries_sf <- sf::st_read(system.file("spatial/countries.geojson", package = "duckspatial"), quiet = TRUE)
countries_sf <- subset(countries_sf, CNTR_ID %in% c("AR", "BR", "BO", "PE", "PY", "UY", "CL"))
argentina_sf <- sf::st_read(system.file("spatial/argentina.geojson", package = "duckspatial"), quiet = TRUE)

# read lines data
rivers_sf <- sf::st_read(system.file("spatial/rivers.geojson", package = "duckspatial"), quiet = TRUE)

## create points data
set.seed(42)
n <- 1000
points_sf <- data.frame(
    id = 1:n,
    x = runif(n, min = -180, max = 180),
    y = runif(n, min = -90, max = 90)
) |>
    sf::st_as_sf(coords = c("x", "y"), crs = 4326)

# North Carolina data from sf package - used by duckspatial_df tests
# North Carolina data from sf package - used by duckspatial_df tests
nc_sf   <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
# Set AGR to constant to silence sf warnings during geometry transformations (like centroid)
sf::st_agr(nc_sf) <- "constant"

nc_ddbs <- duckspatial::ddbs_open_dataset(system.file("shape/nc.shp", package = "sf"))

# Projected NC for accurate distance calculations (EPSG:5070)
nc_sf_5070 <- sf::st_transform(nc_sf, 5070)

# DuckDB uses GEOS (planar), disable S2 for consistent comparison with sf
# DuckDB uses GEOS (planar), disable S2 for consistent comparison with sf
sf::sf_use_s2(FALSE)

# Silence duckspatial messages globally during tests
options(duckspatial.quiet = TRUE)


#' Compare ddbs result with sf result
#' @param ddbs_result Result from ddbs_filter/ddbs_join
#' @param sf_result Result from sf::st_filter/st_join
#' @param id_col Column to sort by for order-independent comparison
#' @param check_geom Whether to verify geometry equivalence
#' @noRd
expect_ddbs_sf_equal <- function(ddbs_result, sf_result, id_col = "NAME", check_geom = TRUE) {
  testthat::expect_equal(nrow(ddbs_result), nrow(sf_result))
  
  if (nrow(ddbs_result) > 0 && id_col %in% names(ddbs_result)) {
    ddbs_sorted <- ddbs_result[order(ddbs_result[[id_col]]), ]
    sf_sorted <- sf_result[order(sf_result[[id_col]]), ]
    testthat::expect_equal(ddbs_sorted[[id_col]], sf_sorted[[id_col]])
    
    if (check_geom) {
      geom_equal <- sf::st_equals_exact(ddbs_sorted, sf_sorted, par = 1e-6, sparse = FALSE)
      testthat::expect_true(all(diag(geom_equal)))
    }
  }
}

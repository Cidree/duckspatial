

# skip tests on CRAN
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

# helpers --------------------------------------------------------------

# create duckdb connection
conn_test <- ddbs_create_conn()

## store countries
ddbs_write_vector(conn_test, countries_sf, "countries")

## store countries with different CRS
countries_3857_sf <- sf::st_transform(countries_sf, "EPSG:3857")
ddbs_write_vector(conn_test, countries_3857_sf, "countries_3857_test")

# expected behavior --------------------------------------------------------------

testthat::test_that("transform CRS to AUTH:CODE", {
    
    ## transform sf - code
    countries_3857_sf <- ddbs_transform(
        x = countries_sf,
        y = "EPSG:3857"
    )
  
    ## transforms table - code
    countries_conn_3857_sf <- ddbs_transform(
        x = "countries",
        y = "EPSG:3857",
        conn = conn_test
    )
  
    ## transform table - code - creating new table
    conn_res <- ddbs_transform(
        x    = "countries",
        y    = "EPSG:3857",
        conn = conn_test,
        name = "countries_3857"
    )

    ## checks
    testthat::expect_equal(sf::st_crs(countries_3857_sf), sf::st_crs("EPSG:3857"))
    testthat::expect_equal(sf::st_crs(countries_conn_3857_sf), sf::st_crs("EPSG:3857"))
    testthat::expect_true(conn_res)
  
    ## create new table overwritting
    conn_res <- ddbs_transform(
        x         = "countries",
        y         = "EPSG:3857",
        conn      = conn_test,
        name      = "countries_3857",
        overwrite = TRUE,
        quiet     = TRUE
    )
  
    ## check
    testthat::expect_true(conn_res)


})


testthat::test_that("transform CRS to SF", {
    
    ## transform sf - sf
    countries_3857_sf <- ddbs_transform(
        x = countries_sf,
        y = countries_3857_sf
    )
  
    ## transforms table - sf
    countries_conn_3857_sf <- ddbs_transform(
        x = "countries",
        y = countries_3857_sf,
        conn = conn_test
    )
  
    ## transform table - sf - creating new table
    conn_res <- ddbs_transform(
        x    = "countries",
        y    = countries_3857_sf,
        conn = conn_test,
        name = "countries_3857_sf"
    )

    ## checks
    testthat::expect_equal(sf::st_crs(countries_3857_sf), sf::st_crs("EPSG:3857"))
    testthat::expect_equal(sf::st_crs(countries_conn_3857_sf), sf::st_crs("EPSG:3857"))
    testthat::expect_true(conn_res)
  
    ## create new table overwritting
    conn_res <- ddbs_transform(
        x         = "countries",
        y         = countries_3857_sf,
        conn      = conn_test,
        name      = "countries_3857_sf",
        overwrite = TRUE,
        quiet     = TRUE
    )
  
    ## check
    testthat::expect_true(conn_res)


})



testthat::test_that("transform CRS to DuckDB table", {
    
    ## transform sf - DuckDB table
    countries_3857_sf <- ddbs_transform(
        x = countries_sf,
        y = "countries_3857_test",
        conn = conn_test
    )
  
    ## transforms table - DuckDB table
    countries_conn_3857_sf <- ddbs_transform(
        x = "countries",
        y = countries_3857_sf,
        conn = conn_test
    )
  
    ## transform table - DuckDB table - creating new table
    conn_res <- ddbs_transform(
        x    = "countries",
        y    = "countries_3857_test",
        conn = conn_test,
        name = "countries_3857_table"
    )

    ## checks
    testthat::expect_equal(sf::st_crs(countries_3857_sf), sf::st_crs("EPSG:3857"))
    testthat::expect_equal(sf::st_crs(countries_conn_3857_sf), sf::st_crs("EPSG:3857"))
    testthat::expect_true(conn_res)
  
    ## create new table overwritting
    conn_res <- ddbs_transform(
        x         = "countries",
        y         = "countries_3857_test",
        conn      = conn_test,
        name      = "countries_3857_table",
        overwrite = TRUE,
        quiet     = TRUE
    )
  
    ## check
    testthat::expect_true(conn_res)


})



testthat::test_that("transform CRS to CRS object", {
    
    ## transform sf - DuckDB table
    rivers_4326_sf <- ddbs_transform(
        x = rivers_sf,
        y = sf::st_crs(countries_sf)
    )
  
    ## transforms table - DuckDB table
    countries_conn_3035_sf <- ddbs_transform(
        x = "countries",
        y = sf::st_crs(rivers_sf),
        conn = conn_test
    )
  
    ## transform table - DuckDB table - creating new table
    conn_res <- ddbs_transform(
        x    = "countries",
        y = sf::st_crs(rivers_sf),
        conn = conn_test,
        name = "countries_3035_table"
    )

    ## checks
    testthat::expect_equal(ddbs_crs(rivers_4326_sf), sf::st_crs(countries_sf))
    testthat::expect_equal(ddbs_crs(countries_conn_3035_sf), sf::st_crs(rivers_sf))
    testthat::expect_true(conn_res)
  
    ## create new table overwritting
    conn_res <- ddbs_transform(
        x         = "countries",
        y = sf::st_crs(rivers_sf),
        conn      = conn_test,
        name = "countries_3035_table",
        overwrite = TRUE,
        quiet     = TRUE
    )
  
    ## check
    testthat::expect_true(conn_res)


})

# expected warnings ------------------------------------------------------------


testthat::test_that("ddbs_transform warns on same CRS", {
  
    ## transform sf - DuckDB table
    testthat::expect_warning(
        ddbs_transform(
            x    = countries_sf,
            y    = "countries",
            conn = conn_test
        )
    )
  
    ## transforms table - DuckDB table
    testthat::expect_warning(
        ddbs_transform(
            x    = "countries",
            y    = countries_sf,
            conn = conn_test
        )
    )
  
    ## transform table - sf - creating new table
    testthat::expect_warning(
        ddbs_transform(
            x    = "countries",
            y    = countries_sf,
            conn = conn_test,
            name = "countries_3857_table_warn"
        )
    )
  
  
    ## create new table overwritting
    testthat::expect_warning(
        ddbs_transform(
            x         = "countries",
            y         = countries_sf,
            conn      = conn_test,
            name      = "countries_3857_table",
            overwrite = TRUE,
            quiet     = TRUE
        )
    )

})



# expected errors --------------------------------------------------------------

testthat::test_that("errors with incorrect input", {

    testthat::expect_error(ddbs_transform(x = 999))
    testthat::expect_error(ddbs_transform(y = 999))
    testthat::expect_error(ddbs_transform(conn = 999))
    testthat::expect_error(ddbs_transform(overwrite = 999))
    testthat::expect_error(ddbs_transform(quiet = 999))

    testthat::expect_error(ddbs_transform(x = "999", conn = conn_test))

    testthat::expect_error(ddbs_transform(conn = conn_test, name = c('banana', 'banana')))


})


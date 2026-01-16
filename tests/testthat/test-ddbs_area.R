
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## helper functions
tester_sf <- function(x = nc_sf,
                    conn = NULL,
                    name = NULL,
                    new_column = NULL,
                    output = NULL,
                    overwrite = FALSE,
                    quiet = FALSE) {
    ddbs_area(
        x = x,
        conn = conn,
        name = name,
        new_column = new_column,
        output = output,
        overwrite = overwrite,
        quiet = quiet
    )
}

tester_ddbs <- function(x = nc_ddbs,
                        conn = NULL,
                        name = NULL,
                        new_column = NULL,
                        output = NULL,
                        overwrite = FALSE,
                        quiet = FALSE) {
    ddbs_area(
        x = x,
        conn = conn,
        name = name,
        new_column = new_column,
        output = output,
        overwrite = overwrite,
        quiet = quiet
    )
}

tester_conn <- function(x = "nc",
                        conn = conn_test,
                        name = NULL,
                        new_column = NULL,
                        output = NULL,
                        overwrite = FALSE,
                        quiet = FALSE) {
    ddbs_area(
        x = x,
        conn = conn,
        name = name,
        new_column = new_column,
        output = output,
        overwrite = overwrite,
        quiet = quiet
    )
}

# 1. Input SF ------------------------------------------------------------

## expected behaviour for inherits(x, "sf")
## - CHECK 1.1: returns a vector by default
## - CHECK 1.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: sf is written into the database
## - CHECK 1.4: message is shown with quiet = FALSE
## - CHECK 1.5: no message is shown with quiet = TRUE
## - CHECK 1.6: area is calculated properly
## - CHECK 1.7: materialize data, same output
## - CHECK 1.8: area calculated as vector, and added as column must be the same
testthat::test_that("ddbs_area(): expected behavior on sf", {
    
    ## CHECK 1.1
    output1 <- tester_sf()
    testthat::expect_true(is(output1 , 'vector'))

    ## CHECK 1.2
    output2 <- tester_sf(new_column = "area_calc", output = NULL)
    output3 <- tester_sf(new_column = "area_calc", output = "geoarrow")
    output4 <- tester_sf(new_column = "area_calc", output = "sf")
    output5 <- tester_sf(new_column = "area_calc", output = "raw")

    testthat::expect_s3_class(output2, "duckspatial_df")
    testthat::expect_s3_class(output3$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output4, "sf")
    testthat::expect_s3_class(output5, "tbl_df")

    ## CHECK 1.3
    output6 <- tester_sf(conn = conn_test, name = "area_tbl", new_column = "area_calc")
    testthat::expect_true(output6)

    ## CHECK 1.4
    testthat::expect_message(tester_sf(new_column = "area_calc"))
    testthat::expect_message(tester_sf(conn = conn_test, name = "area_tbl2", new_column = "area_calc"))

    ## CHECK 1.5
    testthat::expect_no_message(tester_sf(new_column = "area_calc", quiet = TRUE))
    testthat::expect_no_message(tester_sf(conn = conn_test, name = "area_tbl3", new_column = "area_calc", quiet = TRUE))

    ## CHECK 1.6 - calculate area on projected CRS
    argentina_3857_sf <- sf::st_transform(argentina_sf, "EPSG:3857")
    area_ddbs <- ddbs_area(argentina_3857_sf)
    area_sf   <- sf::st_area(argentina_3857_sf) |> as.numeric()
    expect_equal(area_ddbs, area_sf)

    ## CHECK 1.7  
    output7 <- output2 |> st_as_sf()
    output8 <- output2 |> collect()
    output9 <- output2 |> ddbs_collect()
    testthat::expect_identical(output7, output8)
    testthat::expect_identical(output8, output9)
    testthat::expect_s3_class(output7, "sf")

    ## CHECK 1.8
    ## - The area in the column should be the same as the calculated vector
    testthat::expect_identical(output1, output7$area_calc)

})


# 2. Input duckspatial_df ------------------------------------------------

## expected behaviour for inherits(x, "duckspatial_df")
## - CHECK 2.1: returns a vector by default
## - CHECK 2.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 2.3: duckspatial is written into the database
## - CHECK 2.4: message is shown with quiet = FALSE
## - CHECK 2.5: no message is shown with quiet = TRUE
## - CHECK 2.6: area is calculated properly
## - CHECK 2.7: when creating a new duckdb table, it shows warning if they come from 
##   different connections
## - CHECK 2.8 - materialize data, same output
## - CHECK 2.9 - area calculated as vector, and added as column must be the same
testthat::test_that("ddbs_area(): expected behavior on duckspatial", {
    
    ## CHECK 3.1
    output1 <- tester_ddbs()
    testthat::expect_true(is(output1 , 'vector'))

    ## CHECK 2.2
    output2 <- tester_ddbs(new_column = "area_calc", output = NULL)
    output3 <- tester_ddbs(new_column = "area_calc", output = "geoarrow")
    output4 <- tester_ddbs(new_column = "area_calc", output = "sf")
    output5 <- tester_ddbs(new_column = "area_calc", output = "raw")

    testthat::expect_s3_class(output2, "duckspatial_df")
    testthat::expect_s3_class(output3$geom, "geoarrow_vctr")
    testthat::expect_s3_class(output4, "sf")
    testthat::expect_s3_class(output5, "tbl_df")

    ## CHECK 2.3
    output6 <- tester_ddbs(conn = conn_test, name = "ddbs_area_tbl", new_column = "area_calc") |> suppressWarnings()
    testthat::expect_true(output6)

    ## CHECK 2.4
    testthat::expect_message(tester_ddbs(new_column = "area_calc"))
    testthat::expect_message(tester_ddbs(conn = conn_test, name = "ddbs_area_tbl2", new_column = "area_calc") |> suppressWarnings())
    testthat::expect_message(tester_ddbs(conn = conn_test, name = "ddbs_area_tbl3", new_column = "area_calc", quiet = TRUE) |> suppressWarnings())

    ## CHECK 2.5
    testthat::expect_no_message(tester_ddbs(new_column = "area_calc", quiet = TRUE))

    ## CHECK 2.6 - calculate area on projected CRS
    argentina_3857_sf <- sf::st_transform(argentina_sf, "EPSG:3857")
    area_ddbs <- ddbs_area(argentina_3857_sf)
    area_sf   <- sf::st_area(argentina_3857_sf) |> as.numeric()
    testthat::expect_equal(area_ddbs, area_sf)

    ## CHECK 2.7
    testthat::expect_warning(tester_ddbs(conn = conn_test, name = "ddbs_area_tbl4", new_column = "area_calc"))

    ## CHECK 2.8    
    output7 <- output2 |> st_as_sf()
    output8 <- output2 |> collect()
    output9 <- output2 |> ddbs_collect()
    testthat::expect_identical(output7, output8)
    testthat::expect_identical(output8, output9)
    testthat::expect_s3_class(output7, "sf")

    ## CHECK 2.9
    ## - The area in the column should be the same as the calculated vector
    testthat::expect_identical(output1, output7$area_calc)

})

# 3. Input DuckDB table --------------------------------------------------


## expected behaviour for inherits(x, "character")
## - CHECK 3.1: returns a vector by default
## - CHECK 3.2: returns the correct output (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 3.3: duckspatial is written into the database
## - CHECK 3.4: message is shown with quiet = FALSE
## - CHECK 3.5: no message is shown with quiet = TRUE
## - CHECK 3.6: area is calculated properly
## - CHECK 3.7: materialize data, same output
## - CHECK 3.8: area calculated as vector, and added as column must be the same
## - CHECK 3.9: error if conn = NULL, and x = duckdb table
testthat::test_that("ddbs_area(): expected behavior on DuckDB table", {

    ## import table
    duckspatial::ddbs_write_vector(conn_test, nc_ddbs, "nc")
    
    ## CHECK 3.1
    output1 <- tester_conn(x = "nc", conn = conn_test)
    testthat::expect_true(is(output1 , 'vector'))

    ## CHECK 3.2
    output2 <- tester_conn(new_column = "area_calc", output = NULL)
    output3 <- tester_conn(new_column = "area_calc", output = "geoarrow")
    output4 <- tester_conn(new_column = "area_calc", output = "sf")
    output5 <- tester_conn(new_column = "area_calc", output = "raw")

    testthat::expect_s3_class(output2, "duckspatial_df")
    testthat::expect_s3_class(output3$geom, "geoarrow_vctr")
    testthat::expect_s3_class(output4, "sf")
    testthat::expect_s3_class(output5, "tbl_df")

    ## CHECK 3.3
    output6 <- tester_conn(name = "conn_area_tbl", new_column = "area_calc")
    testthat::expect_true(output6)

    ## CHECK 3.4
    testthat::expect_message(tester_conn(new_column = "area_calc"))
    testthat::expect_message(tester_conn(name = "conn_area_tbl2", new_column = "area_calc"))

    ## CHECK 3.5
    testthat::expect_no_message(tester_conn(new_column = "area_calc", quiet = TRUE))
    testthat::expect_no_message(tester_conn(name = "conn_area_tbl3", new_column = "area_calc", quiet = TRUE))

    ## CHECK 3.6 - calculate area on projected CRS
    argentina_3857_sf <- sf::st_transform(argentina_sf, "EPSG:3857")
    duckspatial::ddbs_write_vector(conn_test, argentina_3857_sf, "argentina")
    area_ddbs <- ddbs_area("argentina", conn_test)
    area_sf   <- sf::st_area(argentina_3857_sf) |> as.numeric()
    testthat::expect_equal(area_ddbs, area_sf)

    ## CHECK 3.7    
    output7 <- output2 |> st_as_sf()
    output8 <- output2 |> collect()
    output9 <- output2 |> ddbs_collect()
    testthat::expect_identical(output7, output8)
    testthat::expect_identical(output8, output9)
    testthat::expect_s3_class(output7, "sf")

    ## CHECK 3.8
    ## - The area in the column should be the same as the calculated vector
    testthat::expect_identical(output1, output7$area_calc)

    ## CHECK 3.9
    testthat::expect_error(tester_conn(x = "nc", conn = NULL))

})



# 4. Errors --------------------------------------------------------------

## Check that errors work
## - CHECK 4.1: if name is specified, new_column cannot be NULL
## - CHECK 4.2: if overwrite = FALSE, it won't delete an existing table
## - CHECK 4.3: incorrect inputs
testthat::test_that("ddbs_area(): errors work", {


    ## CHECK 4.1
    testthat::expect_error(tester_sf(name = "new_tbl"))

    ## CHECK 4.2
    ddbs_write_vector(conn_test, countries_sf, "countries")
    testthat::expect_error(
        tester_sf(conn = conn_test, name = "countries", new_column = "area_calc")
    )

    ## CHECK 4.3
    testthat::expect_error(tester_sf(x = 999))
    testthat::expect_error(tester_sf(conn = 999))
    testthat::expect_error(tester_sf(new_column = 999))
    testthat::expect_error(tester_sf(overwrite = 999))
    testthat::expect_error(tester_sf(quiet = 999))
    testthat::expect_error(tester_sf(x = "999", conn = conn_test))
    testthat::expect_error(tester_sf(conn = conn_test, name = c('banana', 'banana')))

})


## stop connection
ddbs_stop_conn(conn_test)

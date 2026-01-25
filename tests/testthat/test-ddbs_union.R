
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## add a grouping column to cuntries
set.seed(123)
countries_group_sf <- countries_sf |> 
  dplyr::mutate(n = sample(1:2, nrow(countries_sf), replace = TRUE)) |> 
  dplyr::mutate(n_2 = sample(c("A", "B"), nrow(countries_sf), replace = TRUE)) 

countries_group_ddbs <- as_duckspatial_df(countries_group_sf)

## write data
duckspatial::ddbs_write_vector(conn_test, points_sf, "points")
duckspatial::ddbs_write_vector(conn_test, countries_group_sf, "countries")


# 1. ddbs_union_agg() ----------------------------------------------------

## 1.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1.5: there must be the same number of rows as the number of groups (2)
## - CHECK 1.6: grouping with more than 1 column
testthat::test_that("ddbs_union_agg(): expected behavior", {
  
    ## CHECK 1.1
    output_ddbs <- ddbs_union_agg(countries_group_ddbs, by = "n")
    output_sf   <- ddbs_union_agg(countries_group_sf, by = "n")
    output_conn <- ddbs_union_agg("countries", by = "n", conn = conn_test)

    testthat::expect_s3_class(output_ddbs, "duckspatial_df")
    testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
    ## the next need to be sorted, as the results aren't sorted automatically
    testthat::expect_equal(
        ddbs_collect(output_ddbs) |> dplyr::arrange(n), 
        ddbs_collect(output_conn)
    )


    ## CHECK 1.2
    output_geoarrow_fmt <- ddbs_union_agg(countries_group_ddbs, "n", output = "geoarrow")
    output_sf_fmt       <- ddbs_union_agg(countries_group_ddbs, "n", output = "sf")
    output_raw_fmt      <- ddbs_union_agg(countries_group_ddbs, "n", output = "raw")

    testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_fmt, "sf")
    testthat::expect_s3_class(output_raw_fmt, "tbl_df")


    ## CHECK 1.3
    testthat::expect_message(ddbs_union_agg(countries_group_ddbs, "n"))
    testthat::expect_message(ddbs_union_agg("countries", "n", conn = conn_test, name = "union_agg"))
    testthat::expect_message(ddbs_union_agg("countries", "n", conn = conn_test, name = "union_agg", overwrite = TRUE))
    testthat::expect_true(ddbs_union_agg("countries", "n", conn = conn_test, name = "union_agg2"))

    testthat::expect_no_message(ddbs_union_agg(countries_group_ddbs, "n", quiet = TRUE))
    testthat::expect_no_message(ddbs_union_agg("countries", "n", conn = conn_test, name = "union_agg", overwrite = TRUE, quiet = TRUE))


    ## CHECK 1.4
    output_tbl <- ddbs_read_vector(conn_test, "union_agg")
    testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
    )


    ## CHECK 1.5
    n_rows <- ddbs_collect(output_ddbs) |> nrow()
    testthat::expect_equal(n_rows, length(unique(countries_group_sf$n)))


    ## CHECK 1.6
    output_ddbs_2 <- ddbs_union_agg(countries_group_ddbs, by = c("n", "n_2"))
    testthat::expect_s3_class(output_ddbs, "duckspatial_df")

})






## 1.2. Expected errors --------------------------------------------------------------

## CHECK 2.1: specific errors
## CHECK 2.2: general errors
testthat::test_that("ddbs_union_agg(): errors work", {

  ## CHECK 2.1
  testthat::expect_error(ddbs_union_agg(countries_group_ddbs, by = 3))
  testthat::expect_error(ddbs_union_agg(countries_group_ddbs, by = NULL))
  testthat::expect_error(ddbs_union_agg(countries_group_ddbs, by = "banana"))
  testthat::expect_error(ddbs_union_agg(countries_group_ddbs, by = c("n", "banana")))
  
  ## CHECK 2.2
  testthat::expect_error(ddbs_union_agg("countries", conn = NULL))
  testthat::expect_error(ddbs_union_agg(x = 999))
  testthat::expect_error(ddbs_union_agg(countries_group_ddbs, conn = 999))
  testthat::expect_error(ddbs_union_agg(countries_group_ddbs, quiet = 999))
  testthat::expect_error(ddbs_union_agg(x = "999", conn = conn_test))
  
})



# 2. ddbs_union() --------------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats (3 function ways)
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1.5: check number of rows of the result
testthat::test_that("ddbs_union(): expected behavior", {
  
    ## CHECK 1.1
    ## - Way 1: only `x`
    output_ddbs <- ddbs_union(countries_ddbs)
    output_sf   <- ddbs_union(countries_sf)
    output_conn <- ddbs_union("countries", conn = conn_test)

    testthat::expect_s3_class(output_ddbs, "duckspatial_df")
    testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
    testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    ## - Way 2: `x` and `x`, by_feature = FALSE
    output_ddbs_2 <- ddbs_union(countries_ddbs, countries_sf)
    output_sf_2   <- ddbs_union(countries_sf, countries_ddbs)
    output_conn_2 <- ddbs_union("countries", countries_sf, conn = conn_test)

    testthat::expect_s3_class(output_ddbs_2, "duckspatial_df")
    testthat::expect_equal(ddbs_collect(output_ddbs_2), ddbs_collect(output_sf_2))
    testthat::expect_equal(ddbs_collect(output_ddbs_2), ddbs_collect(output_conn_2))
    ## - Way 3: `x` and `x`, by_feature = TRUE
    output_ddbs_3 <- ddbs_union(countries_ddbs, countries_sf, by_feature = TRUE)
    output_sf_3   <- ddbs_union(countries_sf, countries_ddbs, by_feature = TRUE)
    output_conn_3 <- ddbs_union(countries_sf, "countries", conn = conn_test, by_feature = TRUE)

    testthat::expect_s3_class(output_ddbs_3, "duckspatial_df")
    testthat::expect_equal(ddbs_collect(output_ddbs_3), ddbs_collect(output_sf_3))
    testthat::expect_equal(ddbs_collect(output_ddbs_3), ddbs_collect(output_conn_3))

    ## CHECK 1.2
    output_geoarrow_fmt <- ddbs_union(countries_ddbs, output = "geoarrow")
    output_sf_fmt       <- ddbs_union(countries_ddbs, output = "sf")
    output_raw_fmt      <- ddbs_union(countries_ddbs, output = "raw")

    testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_fmt, "sf")
    testthat::expect_s3_class(output_raw_fmt, "tbl_df")


    ## CHECK 1.3
    testthat::expect_message(ddbs_union(countries_ddbs))
    testthat::expect_warning(ddbs_union(countries_ddbs, by_feature = TRUE))
    testthat::expect_message(ddbs_union("countries", conn = conn_test, name = "union_test"))
    testthat::expect_message(ddbs_union("countries", conn = conn_test, name = "union_test", overwrite = TRUE))
    testthat::expect_true(ddbs_union("countries", conn = conn_test, name = "union_test2"))

    testthat::expect_no_message(ddbs_union(countries_ddbs, quiet = TRUE))
    testthat::expect_no_message(ddbs_union("countries", conn = conn_test, name = "union_test", overwrite = TRUE, quiet = TRUE))


    ## CHECK 1.4
    output_tbl <- ddbs_read_vector(conn_test, "union_test")
    testthat::expect_equal(
    ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
    )


    ## CHECK 1.5
    ## - Way 1: always 1 row
    n_rows <- ddbs_collect(output_ddbs) |> nrow()
    testthat::expect_equal(n_rows, 1)
    ## - Way 2: always 1 row
    n_rows_2 <- ddbs_collect(output_ddbs_2) |> nrow()
    testthat::expect_equal(n_rows_2, 1)
    ## - Way 3: minimum number of rows between x and y
    ## in this example they have the same, since its the same dataset
    n_rows_3 <- ddbs_collect(output_ddbs_3) |> nrow()
    testthat::expect_equal(n_rows_3, nrow(countries_sf))

})






## 2.2. Expected errors --------------------------------------------------------------

## CHECK 2.1: specific errors
## CHECK 2.2: general errors
testthat::test_that("ddbs_union(): errors work", {

  ## CHECK 2.1
  testthat::expect_error(ddbs_union(countries_ddbs, by_feature = 3))
  testthat::expect_error(ddbs_union(countries_ddbs, by_feature = NULL))
  testthat::expect_error(ddbs_union(countries_ddbs, by_feature = "banana"))
  testthat::expect_error(ddbs_union(y = countries_ddbs))
  
  ## CHECK 2.2
  testthat::expect_error(ddbs_union("countries", conn = NULL))
  testthat::expect_error(ddbs_union(x = 999))
  testthat::expect_error(ddbs_union(countries_ddbs, conn = 999))
  testthat::expect_error(ddbs_union(countries_ddbs, quiet = 999))
  testthat::expect_error(ddbs_union(x = "999", conn = conn_test))
  
})



# 3. ddbs_combine() -----------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1:5: always returns 1 row
testthat::test_that("ddbs_combine(): expected behavior", {
    
    ## CHECK 1.1
    output_ddbs <- ddbs_combine(countries_ddbs)
    output_sf   <- ddbs_combine(countries_sf)
    output_conn <- ddbs_combine("countries", conn = conn_test)

    testthat::expect_s3_class(output_ddbs, "duckspatial_df")
    testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
    testthat::expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    

    ## CHECK 1.2
    output_geoarrow_fmt <- ddbs_combine(countries_ddbs, output = "geoarrow")
    output_sf_fmt       <- ddbs_combine(countries_ddbs, output = "sf")
    output_raw_fmt      <- ddbs_combine(countries_ddbs, output = "raw")

    testthat::expect_s3_class(output_geoarrow_fmt$geometry, "geoarrow_vctr")
    testthat::expect_s3_class(output_sf_fmt, "sf")
    testthat::expect_s3_class(output_raw_fmt, "tbl_df")


    ## CHECK 1.3
    testthat::expect_message(ddbs_combine(countries_ddbs))
    testthat::expect_message(ddbs_combine("countries", conn = conn_test, name = "combine"))
    testthat::expect_message(ddbs_combine("countries", conn = conn_test, name = "combine", overwrite = TRUE))
    testthat::expect_true(ddbs_combine("countries", conn = conn_test, name = "combine2"))

    testthat::expect_no_message(ddbs_combine(countries_ddbs, quiet = TRUE))
    testthat::expect_no_message(ddbs_combine("countries", conn = conn_test, name = "combine", overwrite = TRUE, quiet = TRUE))
        
    ## CHECK 1.4
    output_tbl <- ddbs_read_vector(conn_test, "combine")
    testthat::expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
    )


    ## CHECK 1.5
    n_rows <- ddbs_collect(output_ddbs) |> nrow()
    testthat::expect_equal(n_rows, 1)

})

## 2.2. Errors -------------------------

## CHECK 2.1: errors
testthat::test_that("ddbs_combine(): errors work", {
  
    ## CHECK 2.1
    testthat::expect_error(ddbs_combine("countries", conn = NULL))
    testthat::expect_error(ddbs_combine(x = 999))
    testthat::expect_error(ddbs_combine(countries_ddbs, conn = 999))
    testthat::expect_error(ddbs_combine(countries_ddbs, new_column = 999))
    testthat::expect_error(ddbs_combine(countries_ddbs, overwrite = 999))
    testthat::expect_error(ddbs_combine(countries_ddbs, quiet = 999))
    testthat::expect_error(ddbs_combine(x = "999", conn = conn_test))
    testthat::expect_error(ddbs_combine(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
  
})

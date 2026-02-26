
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
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_union_agg()", {
  
  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_union_agg(countries_group_ddbs, by = "n")
      output_sf   <- ddbs_union_agg(countries_group_sf, by = "n")
      output_conn <- ddbs_union_agg("countries", by = "n", conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(
        ddbs_collect(output_ddbs) |> dplyr::arrange(n), 
        ddbs_collect(output_conn)
      )
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_union_agg(countries_group_ddbs, "n", output = NULL)
      output_geoarrow <- ddbs_union_agg(countries_group_ddbs, "n", output = "geoarrow")
      output_sf       <- ddbs_union_agg(countries_group_ddbs, "n", output = "sf")
      output_raw      <- ddbs_union_agg(countries_group_ddbs, "n", output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_union_agg(countries_group_ddbs, "n"))
      expect_message(ddbs_union_agg("countries", "n", conn = conn_test, name = "union_agg"))
      expect_message(ddbs_union_agg("countries", "n", conn = conn_test, name = "union_agg", overwrite = TRUE))
      
      expect_no_message(ddbs_union_agg(countries_group_ddbs, "n", quiet = TRUE))
      expect_no_message(ddbs_union_agg("countries", "n", conn = conn_test, name = "union_agg", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output <- ddbs_union_agg("countries", "n", conn = conn_test, name = "union_agg2")
      expect_true(output)
    })
    
    it("written table matches computed output", {
      output_conn <- ddbs_union_agg("countries", by = "n", conn = conn_test)
      output_tbl  <- ddbs_read_vector(conn_test, "union_agg")
      
      expect_equal(
        ddbs_collect(output_conn)$geometry,
        output_tbl$geometry
      )
    })
    
    it("returns same number of rows as number of groups", {
      output <- ddbs_union_agg(countries_group_ddbs, by = "n")
      n_rows <- ddbs_collect(output) |> nrow()
      
      expect_equal(n_rows, length(unique(countries_group_sf$n)))
    })
    
    it("supports grouping by multiple columns", {
      output <- ddbs_union_agg(countries_group_ddbs, by = c("n", "n_2"))
      
      expect_s3_class(output, "duckspatial_df")
    })
  })
  
  ### EXPECTED ERRORS
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_union_agg("countries", conn = NULL))
    })
    
    it("requires by argument", {
      expect_error(ddbs_union_agg(countries_group_ddbs, by = NULL))
    })
    
    it("validates by argument type", {
      expect_error(ddbs_union_agg(countries_group_ddbs, by = 3))
    })
    
    it("validates by argument column existence", {
      expect_error(ddbs_union_agg(countries_group_ddbs, by = "banana"))
      expect_error(ddbs_union_agg(countries_group_ddbs, by = c("n", "banana")))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_union_agg(x = 999))
      expect_error(ddbs_union_agg(x = "999", conn = conn_test))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_union_agg(countries_group_ddbs, conn = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_union_agg(countries_group_ddbs, quiet = 999))
    })
  })
})


# 2. ddbs_union() --------------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats (3 function ways)
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1.5: check number of rows of the result
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_union()", {
  
  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("works on all formats with only x argument", {
      output_ddbs <- ddbs_union(countries_ddbs)
      output_sf   <- ddbs_union(countries_sf)
      output_conn <- ddbs_union("countries", conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("works on all formats with x and y, by_feature = FALSE", {
      output_ddbs <- ddbs_union(countries_ddbs, countries_sf)
      output_sf   <- ddbs_union(countries_sf, countries_ddbs)
      output_conn <- ddbs_union("countries", countries_sf, conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("works on all formats with x and y, by_feature = TRUE", {
      output_ddbs <- ddbs_union(countries_ddbs, countries_sf, by_feature = TRUE)
      output_sf   <- ddbs_union(countries_sf, countries_ddbs, by_feature = TRUE)
      output_conn <- ddbs_union(countries_sf, "countries", conn = conn_test, by_feature = TRUE)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_union(countries_ddbs, output = NULL)
      output_geoarrow <- ddbs_union(countries_ddbs, output = "geoarrow")
      output_sf       <- ddbs_union(countries_ddbs, output = "sf")
      output_raw      <- ddbs_union(countries_ddbs, output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_union(countries_ddbs))
      expect_message(ddbs_union("countries", conn = conn_test, name = "union_test"))
      expect_message(ddbs_union("countries", conn = conn_test, name = "union_test", overwrite = TRUE))
      
      expect_no_message(ddbs_union(countries_ddbs, quiet = TRUE))
      expect_no_message(ddbs_union("countries", conn = conn_test, name = "union_test", overwrite = TRUE, quiet = TRUE))
    })
    
    it("warns when using by_feature = TRUE with only x argument", {
      expect_warning(ddbs_union(countries_ddbs, by_feature = TRUE))
    })
    
    it("writes tables to the database", {
      output <- ddbs_union("countries", conn = conn_test, name = "union_test2")
      expect_true(output)
    })
    
    it("written table matches computed output", {
      output_ddbs <- ddbs_union(countries_ddbs)
      output_tbl  <- ddbs_read_vector(conn_test, "union_test")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
    
    it("returns 1 row when using only x argument", {
      output <- ddbs_union(countries_ddbs)
      n_rows <- ddbs_collect(output) |> nrow()
      
      expect_equal(n_rows, 1)
    })
    
    it("returns 1 row when using x and y with by_feature = FALSE", {
      output <- ddbs_union(countries_ddbs, countries_sf)
      n_rows <- ddbs_collect(output) |> nrow()
      
      expect_equal(n_rows, 1)
    })
    
    it("returns minimum number of rows between x and y when by_feature = TRUE", {
      output <- ddbs_union(countries_ddbs, countries_sf, by_feature = TRUE)
      n_rows <- ddbs_collect(output) |> nrow()
      
      expect_equal(n_rows, nrow(countries_sf))
    })
  })
  
  ### EXPECTED ERRORS
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_union("countries", conn = NULL))
    })
    
    it("requires x argument when y is provided", {
      expect_error(ddbs_union(y = countries_ddbs))
    })
    
    it("validates by_feature argument type", {
      expect_error(ddbs_union(countries_ddbs, by_feature = 3))
      expect_error(ddbs_union(countries_ddbs, by_feature = NULL))
      expect_error(ddbs_union(countries_ddbs, by_feature = "banana"))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_union(x = 999))
      expect_error(ddbs_union(x = "999", conn = conn_test))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_union(countries_ddbs, conn = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_union(countries_ddbs, quiet = 999))
    })
  })
})



# 3. ddbs_combine() -----------------------------------------------------

## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, geoarrow, sf, tbl)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1:5: always returns 1 row
## - CHECK 2.1: errors
describe("ddbs_combine()", {
  
  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_combine(countries_ddbs)
      output_sf   <- ddbs_combine(countries_sf)
      output_conn <- ddbs_combine("countries", conn = conn_test)
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, geoarrow, sf, tbl)", {
      output_ddbs     <- ddbs_combine(countries_ddbs, output = NULL)
      output_geoarrow <- ddbs_combine(countries_ddbs, output = "geoarrow")
      output_sf       <- ddbs_combine(countries_ddbs, output = "sf")
      output_raw      <- ddbs_combine(countries_ddbs, output = "raw")
      
      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_s3_class(output_geoarrow$geometry, "geoarrow_vctr")
      expect_s3_class(output_sf, "sf")
      expect_s3_class(output_raw, "tbl_df")
    })
    
    it("shows and suppresses messages correctly", {
      expect_message(ddbs_combine(countries_ddbs))
      expect_message(ddbs_combine("countries", conn = conn_test, name = "combine"))
      expect_message(ddbs_combine("countries", conn = conn_test, name = "combine", overwrite = TRUE))
      
      expect_no_message(ddbs_combine(countries_ddbs, quiet = TRUE))
      expect_no_message(ddbs_combine("countries", conn = conn_test, name = "combine", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output <- ddbs_combine("countries", conn = conn_test, name = "combine2")
      expect_true(output)
    })
    
    it("written table matches computed output", {
      output_ddbs <- ddbs_combine(countries_ddbs)
      output_tbl  <- ddbs_read_vector(conn_test, "combine")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
    
    it("always returns 1 row", {
      output <- ddbs_combine(countries_ddbs)
      n_rows <- ddbs_collect(output) |> nrow()
      
      expect_equal(n_rows, 1)
    })
  })
  
  ### EXPECTED ERRORS
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_combine("countries", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_combine(x = 999))
      expect_error(ddbs_combine(x = "999", conn = conn_test))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_combine(countries_ddbs, conn = 999))
    })
    
    it("validates new_column argument type", {
      expect_error(ddbs_combine(countries_ddbs, new_column = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_combine(countries_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_combine(countries_ddbs, quiet = 999))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_combine(countries_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})


## stop connection
ddbs_stop_conn(conn_test)

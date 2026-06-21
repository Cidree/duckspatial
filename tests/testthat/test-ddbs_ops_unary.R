
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## write data
duckspatial::ddbs_write_table(conn_test, points_sf, "points")
duckspatial::ddbs_write_table(conn_test, argentina_sf, "argentina")
duckspatial::ddbs_write_table(conn_test, countries_sf, "countries")


# 1. ddbs_buffer() -------------------------------------------------------

describe("ddbs_buffer()", {
  
  ### EXPECTED BEHAVIOUR

  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_buffer(points_ddbs, 50)
      output_sf   <- ddbs_buffer(points_sf, 50)
      output_conn <- ddbs_buffer("points", 50, conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_buffer(points_ddbs, 10, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })
    
    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_buffer(points_ddbs, 10))
      expect_message(ddbs_buffer("points", 50, conn = conn_test, name = "buffer"))
      expect_message(ddbs_buffer("points", 50, conn = conn_test, name = "buffer", overwrite = TRUE))
      expect_true(ddbs_buffer("points", 50, conn = conn_test, name = "buffer2"))

      expect_no_message(ddbs_buffer(points_ddbs, 50, quiet = TRUE))
      expect_no_message(ddbs_buffer("points", 50, conn = conn_test, name = "buffer", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output_ddbs <- ddbs_buffer(points_ddbs, 50)
      output_tbl <- ddbs_read_table(conn_test, "buffer")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
    
    it("matches sf::st_buffer results", {
      point_planar <- sf::st_transform(points_sf[1, ], "EPSG:3857")
      sf_output   <- sf::st_buffer(point_planar, 100, nQuadSegs = 8)
      ddbs_output <- ddbs_buffer(point_planar, 100) |> 
        sf::st_as_sf()

      expect_equal(sf_output$geometry, ddbs_output$geometry)
    })
    
    describe("num_triangles parameter", {
      
      it("works with default value", {
        output <- ddbs_buffer(points_ddbs, 50)
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("works with custom values", {
        output_16 <- ddbs_buffer(points_ddbs, 50, num_triangles = 16)
        output_4 <- ddbs_buffer(points_ddbs, 50, num_triangles = 4)
        
        expect_s3_class(output_16, "duckspatial_df")
        expect_s3_class(output_4, "duckspatial_df")
      })
      
      it("matches sf nQuadSegs parameter", {
        point_planar <- sf::st_transform(points_sf[1, ], "EPSG:3857")
        sf_output <- sf::st_buffer(point_planar, 100, nQuadSegs = 16)
        ddbs_output <- ddbs_buffer(point_planar, 100, num_triangles = 16) |> 
          sf::st_as_sf()
        
        expect_equal(sf_output$geometry, ddbs_output$geometry)
      })
    })
    
    describe("cap_style parameter", {
      
      it("accepts CAP_ROUND (default)", {
        output <- ddbs_buffer(points_ddbs, 50, cap_style = "CAP_ROUND")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("accepts CAP_FLAT on line geometries", {
        output <- ddbs_buffer(rivers_ddbs, 50, cap_style = "CAP_FLAT")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("accepts CAP_SQUARE", {
        output <- ddbs_buffer(points_ddbs, 50, cap_style = "CAP_SQUARE")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("is case-insensitive", {
        output <- ddbs_buffer(points_ddbs, 50, cap_style = "cap_round")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("matches sf endCapStyle parameter", {
        point_planar <- sf::st_transform(points_sf[1, ], "EPSG:3857")
        sf_output <- sf::st_buffer(point_planar, 100, nQuadSegs = 8, endCapStyle = "SQUARE")
        ddbs_output <- ddbs_buffer(point_planar, 100, num_triangles = 8, cap_style = "CAP_SQUARE") |> 
          sf::st_as_sf()
        
        expect_equal(sf_output$geometry, ddbs_output$geometry)
      })
    })
    
    describe("join_style parameter", {
      
      it("accepts JOIN_ROUND (default)", {
        output <- ddbs_buffer(points_ddbs, 50, join_style = "JOIN_ROUND")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("accepts JOIN_MITRE", {
        output <- ddbs_buffer(points_ddbs, 50, join_style = "JOIN_MITRE")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("accepts JOIN_BEVEL", {
        output <- ddbs_buffer(points_ddbs, 50, join_style = "JOIN_BEVEL")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("is case-insensitive", {
        output <- ddbs_buffer(points_ddbs, 50, join_style = "join_round")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("matches sf joinStyle parameter", {
        point_planar <- sf::st_transform(points_sf[1, ], "EPSG:3857")
        sf_output <- sf::st_buffer(point_planar, 100, nQuadSegs = 8, joinStyle = "MITRE")
        ddbs_output <- ddbs_buffer(point_planar, 100, num_triangles = 8, join_style = "JOIN_MITRE") |> 
          sf::st_as_sf()
        
        expect_equal(sf_output$geometry, ddbs_output$geometry)
      })
    })
    
    describe("mitre_limit parameter", {
      
      it("works with default value", {
        output <- ddbs_buffer(points_ddbs, 50, join_style = "JOIN_MITRE")
        expect_s3_class(output, "duckspatial_df")
      })
      
      it("works with custom values", {
        output <- ddbs_buffer(points_ddbs, 50, join_style = "JOIN_MITRE", mitre_limit = 5.0)
        expect_s3_class(output, "duckspatial_df")
      })
    })
    
    it("works with all parameters combined", {
      output <- ddbs_buffer(
        points_ddbs, 
        distance = 50, 
        num_triangles = 16,
        cap_style = "CAP_SQUARE",
        join_style = "JOIN_MITRE",
        mitre_limit = 2.5
      )
      
      expect_s3_class(output, "duckspatial_df")
    })
    
    it("works when creating tables with custom parameters", {
      expect_true(
        ddbs_buffer(
          "points", 
          50, 
          conn = conn_test, 
          name = "buffer_custom",
          num_triangles = 12,
          cap_style = "CAP_SQUARE"
        )
      )
      
      output_custom_tbl <- ddbs_read_table(conn_test, "buffer_custom")
      expect_s3_class(output_custom_tbl, "sf")
    })
  })

  ### EXPECTED ERRORS
  
  describe("errors", {
    
    describe("basic argument validation", {
      
      it("requires distance argument", {
        expect_error(ddbs_buffer(points_ddbs))
      })
      
      it("requires distance to be numeric", {
        expect_error(ddbs_buffer(points_ddbs, distance = "12"))
      })
      
      it("requires connection when using table names", {
        expect_error(ddbs_buffer("points", conn = NULL))
      })
      
      it("validates x argument type", {
        expect_error(ddbs_buffer(x = 999))
      })
      
      it("validates conn argument type", {
        expect_error(ddbs_buffer(points_ddbs, conn = 999))
      })
      
      it("validates overwrite argument type", {
        expect_error(ddbs_buffer(points_ddbs, overwrite = 999))
      })
      
      it("validates quiet argument type", {
        expect_error(ddbs_buffer(points_ddbs, quiet = 999))
      })
      
      it("validates table name exists", {
        expect_error(ddbs_buffer(x = "999", conn = conn_test))
      })
      
      it("requires name to be single character string", {
        expect_error(ddbs_buffer(points_ddbs, conn = conn_test, name = c('banana', 'banana')))
      })
    })
    
    describe("num_triangles validation", {
      
      it("rejects non-numeric values", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, num_triangles = "8"),
          "must be a single integer value"
        )
      })
      
      it("rejects vector inputs", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, num_triangles = c(8, 16)),
          "must be a single integer value"
        )
      })
      
      it("rejects decimal values", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, num_triangles = 8.5),
          "must be a single integer value"
        )
      })
      
      it("rejects zero", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, num_triangles = 0),
          "`num_triangles` must be a positive integer"
        )
      })
      
      it("rejects negative values", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, num_triangles = -5),
          "`num_triangles` must be a positive integer"
        )
      })
    })
    
    describe("cap_style validation", {
      
      it("rejects non-character values", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, cap_style = 123),
          "must be a single character string"
        )
      })
      
      it("rejects vector inputs", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, cap_style = c("CAP_ROUND", "CAP_FLAT")),
          "must be a single character string"
        )
      })
      
      it("rejects invalid cap style names", {
        expect_error(ddbs_buffer(points_ddbs, 50, cap_style = "INVALID_STYLE"))
        expect_error(ddbs_buffer(points_ddbs, 50, cap_style = "ROUND"))
      })
    })
    
    describe("join_style validation", {
      
      it("rejects non-character values", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, join_style = 123),
          "must be a single character string"
        )
      })
      
      it("rejects vector inputs", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, join_style = c("JOIN_ROUND", "JOIN_MITRE")),
          "must be a single character string"
        )
      })
      
      it("rejects invalid join style names", {
        expect_error(ddbs_buffer(points_ddbs, 50, join_style = "INVALID_JOIN"))
        expect_error(ddbs_buffer(points_ddbs, 50, join_style = "ROUND"))
      })
    })
    
    describe("mitre_limit validation", {
      
      it("rejects non-numeric values", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, mitre_limit = "1.0"),
          "must be a single numeric value"
        )
      })
      
      it("rejects vector inputs", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, mitre_limit = c(1.0, 2.0)),
          "must be a single numeric value"
        )
      })
      
      it("rejects zero", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, mitre_limit = 0),
          "`mitre_limit` must be a positive number"
        )
      })
      
      it("rejects negative values", {
        expect_error(
          ddbs_buffer(points_ddbs, 50, mitre_limit = -1.5),
          "`mitre_limit` must be a positive number"
        )
      })
    })
  })
})


# 2. ddbs_centroid() -----------------------------------------------------


## 2.1. Expected behaviour -------------------

## expected behaviour
## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, sf)
## - CHECK 1.3: messages work
#  - CHECK 1.4: writting table works
## - CHECK 1.5: expected errors
describe("ddbs_centroid()", {

  ### EXPECTED BEHAVIOUR
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_centroid(argentina_ddbs)
      output_sf   <- ddbs_centroid(argentina_sf)
      output_conn <- ddbs_centroid("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_centroid(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })
    
    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_centroid(argentina_ddbs))
      expect_message(ddbs_centroid("argentina", conn = conn_test, name = "centroid"))
      expect_message(ddbs_centroid("argentina", conn = conn_test, name = "centroid", overwrite = TRUE))
      expect_true(ddbs_centroid("argentina", conn = conn_test, name = "centroid2"))

      expect_no_message(ddbs_centroid(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_centroid("argentina", conn = conn_test, name = "centroid", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output_ddbs <- ddbs_centroid(argentina_ddbs)
      output_tbl <- ddbs_read_table(conn_test, "centroid")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
  })
  
  ### EXPECTED ERRORS

  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_centroid("argentina", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_centroid(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_centroid(argentina_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_centroid(argentina_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_centroid(argentina_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_centroid(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_centroid(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})




# 3. ddbs_make_valid() -----------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, sf)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 2.1: errors

describe("ddbs_make_valid()", {
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_make_valid(argentina_ddbs)
      output_sf   <- ddbs_make_valid(argentina_sf)
      output_conn <- ddbs_make_valid("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_make_valid(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })
    
    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_make_valid(argentina_ddbs))
      expect_message(ddbs_make_valid("argentina", conn = conn_test, name = "make_valid"))
      expect_message(ddbs_make_valid("argentina", conn = conn_test, name = "make_valid", overwrite = TRUE))
      expect_true(ddbs_make_valid("argentina", conn = conn_test, name = "make_valid2"))

      expect_no_message(ddbs_make_valid(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_make_valid("argentina", conn = conn_test, name = "make_valid", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output_ddbs <- ddbs_make_valid(argentina_ddbs)
      output_tbl <- ddbs_read_table(conn_test, "make_valid")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
  })
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_make_valid("argentina", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_make_valid(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_make_valid(argentina_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_make_valid(argentina_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_make_valid(argentina_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_make_valid(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_make_valid(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})





# 4. ddbs_simplify() -----------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, sf)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 2.1: errors
describe("ddbs_simplify()", {
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_simplify(argentina_ddbs, tolerance = 0.01)
      output_sf   <- ddbs_simplify(argentina_sf, tolerance = 0.01)
      output_conn <- ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_simplify(argentina_ddbs, tolerance = 0.01, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })
    
    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_simplify(argentina_ddbs, tolerance = 0.01))
      expect_message(ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test, name = "simplify"))
      expect_message(ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test, name = "simplify", overwrite = TRUE))
      expect_true(ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test, name = "simplify2"))

      expect_no_message(ddbs_simplify(argentina_ddbs, tolerance = 0.01, quiet = TRUE))
      expect_no_message(ddbs_simplify("argentina", tolerance = 0.01, conn = conn_test, name = "simplify", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output_ddbs <- ddbs_simplify(argentina_ddbs, tolerance = 0.01)
      output_tbl <- ddbs_read_table(conn_test, "simplify")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("works with preserve_topology = TRUE", {
      output_ddbs <- ddbs_simplify(argentina_ddbs, preserve_topology = TRUE)
      expect_s3_class(output_ddbs, "duckspatial_df")
    })

  })
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_simplify("argentina", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_simplify(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_simplify(argentina_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_simplify(argentina_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_simplify(argentina_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_simplify(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_simplify(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})



# 5. ddbs_exterior_ring() -----------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, sf)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1.5: geometry type
## - CHECK 2.1: errors
describe("ddbs_exterior_ring()", {
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_exterior_ring(argentina_ddbs)
      output_sf   <- ddbs_exterior_ring(argentina_sf)
      output_conn <- ddbs_exterior_ring("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_exterior_ring(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })
    
    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_exterior_ring(argentina_ddbs))
      expect_message(ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring"))
      expect_message(ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring", overwrite = TRUE))
      expect_true(ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring2"))

      expect_no_message(ddbs_exterior_ring(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_exterior_ring("argentina", conn = conn_test, name = "exterior_ring", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output_ddbs <- ddbs_exterior_ring(argentina_ddbs)
      output_tbl <- ddbs_read_table(conn_test, "exterior_ring")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
    
    it("returns LINESTRING geometry type", {
      output_ddbs <- ddbs_exterior_ring(argentina_ddbs)
      geom_type <- ddbs_collect(output_ddbs) |> sf::st_geometry_type() |> as.character()
      
      expect_equal(geom_type, "LINESTRING")
    })
  })
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_exterior_ring("argentina", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_exterior_ring(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_exterior_ring(argentina_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_exterior_ring(argentina_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_exterior_ring(argentina_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_exterior_ring(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_exterior_ring(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})




# 6. ddbs_convex_hull() -----------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, sf)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 2.1: errors
describe("ddbs_convex_hull()", {
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_convex_hull(argentina_ddbs)
      output_sf   <- ddbs_convex_hull(argentina_sf)
      output_conn <- ddbs_convex_hull("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_convex_hull(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })
    
    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_convex_hull(argentina_ddbs))
      expect_message(ddbs_convex_hull("argentina", conn = conn_test, name = "convex_hull"))
      expect_message(ddbs_convex_hull("argentina", conn = conn_test, name = "convex_hull", overwrite = TRUE))
      expect_true(ddbs_convex_hull("argentina", conn = conn_test, name = "convex_hull2"))

      expect_no_message(ddbs_convex_hull(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_convex_hull("argentina", conn = conn_test, name = "convex_hull", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output_ddbs <- ddbs_convex_hull(argentina_ddbs)
      output_tbl <- ddbs_read_table(conn_test, "convex_hull")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
  })
  
  describe("errors", {
    
    it("requires connection when using table names", {
      expect_error(ddbs_convex_hull("argentina", conn = NULL))
    })
    
    it("validates x argument type", {
      expect_error(ddbs_convex_hull(x = 999))
    })
    
    it("validates conn argument type", {
      expect_error(ddbs_convex_hull(argentina_ddbs, conn = 999))
    })
    
    it("validates overwrite argument type", {
      expect_error(ddbs_convex_hull(argentina_ddbs, overwrite = 999))
    })
    
    it("validates quiet argument type", {
      expect_error(ddbs_convex_hull(argentina_ddbs, quiet = 999))
    })
    
    it("validates table name exists", {
      expect_error(ddbs_convex_hull(x = "999", conn = conn_test))
    })
    
    it("requires name to be single character string", {
      expect_error(ddbs_convex_hull(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})



# 7. ddbs_concave_hull() ---------------------------------------------------

## - CHECK 1.1: works on all formats
## - CHECK 1.2: ddbs returns different outputs (duckspatial_df, sf)
## - CHECK 1.3: messages work
## - CHECK 1.4: writting table works
## - CHECK 1.5: ratio work
## - CHECK 1.6: allow_holes work
## - CHECK 1.7: same result as sf
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_concave_hull()", {
  
  describe("expected behavior", {
    
    it("works on all formats", {
      output_ddbs <- ddbs_concave_hull(argentina_ddbs)
      output_sf   <- ddbs_concave_hull(argentina_sf)
      output_conn <- ddbs_concave_hull("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })
    
    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_concave_hull(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })
    
    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_concave_hull(argentina_ddbs))
      expect_message(ddbs_concave_hull("argentina", conn = conn_test, name = "concave_hull"))
      expect_message(ddbs_concave_hull("argentina", conn = conn_test, name = "concave_hull", overwrite = TRUE))
      expect_true(ddbs_concave_hull("argentina", conn = conn_test, name = "concave_hull2"))

      expect_no_message(ddbs_concave_hull(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_concave_hull("argentina", conn = conn_test, name = "concave_hull", overwrite = TRUE, quiet = TRUE))
    })
    
    it("writes tables to the database", {
      output_ddbs <- ddbs_concave_hull(argentina_ddbs)
      output_tbl <- ddbs_read_table(conn_test, "concave_hull")
      
      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
    
    describe("ratio parameter", {
      
      it("produces different results with different ratios", {
        output_ratio_1 <- ddbs_concave_hull(argentina_ddbs, ratio = 1, mode = "sf")
        output_ratio_2 <- ddbs_concave_hull(argentina_ddbs, ratio = 0.2, mode = "sf")

        expect_false(identical(output_ratio_1, output_ratio_2))
      })
    })
    
    describe("allow_holes parameter", {
      
      it("produces different results with different allow_holes values", {
        output_holes_1 <- ddbs_concave_hull(argentina_ddbs, allow_holes = TRUE, mode = "sf")
        output_holes_2 <- ddbs_concave_hull(argentina_ddbs, allow_holes = FALSE, mode = "sf")

        expect_false(identical(output_holes_1, output_holes_2))
      })
    })
    
    it("matches sf::st_concave_hull results", {
      geos_version <- package_version(sf::sf_extSoftVersion()["GEOS"])
      skip_if(geos_version < "3.11.0")

      sf_output   <- sf::st_concave_hull(argentina_sf, ratio = 0.5, allow_holes = FALSE)
      ddbs_output <- ddbs_concave_hull(argentina_sf, ratio = 0.5, allow_holes = FALSE, mode = "sf")
      
      expect_equal(sf_output$geometry, ddbs_output$geometry)
    })
  })
  
  describe("errors", {
    
    describe("ratio parameter validation", {
      
      it("rejects values less than 0", {
        expect_error(ddbs_concave_hull(argentina_ddbs, ratio = -1))
      })
      
      it("rejects values greater than 1", {
        expect_error(ddbs_concave_hull(argentina_ddbs, ratio = 2))
      })
      
      it("rejects vector inputs", {
        expect_error(ddbs_concave_hull(argentina_ddbs, ratio = c(0.1, 0.5)))
      })
      
      it("rejects non-numeric values", {
        expect_error(ddbs_concave_hull(argentina_ddbs, ratio = "0.5"))
        expect_error(ddbs_concave_hull(argentina_ddbs, ratio = TRUE))
      })
      
      it("rejects NULL", {
        expect_error(ddbs_concave_hull(argentina_ddbs, ratio = NULL))
      })
    })
    
    describe("allow_holes parameter validation", {
      
      it("rejects non-logical values", {
        expect_error(ddbs_concave_hull(argentina_ddbs, allow_holes = 3))
        expect_error(ddbs_concave_hull(argentina_ddbs, allow_holes = "TRUE"))
      })
      
      it("rejects NULL", {
        expect_error(ddbs_concave_hull(argentina_ddbs, allow_holes = NULL))
      })
    })
    
    describe("basic argument validation", {
      
      it("requires connection when using table names", {
        expect_error(ddbs_concave_hull("argentina", conn = NULL))
      })
      
      it("validates x argument type", {
        expect_error(ddbs_concave_hull(x = 999))
      })
      
      it("validates conn argument type", {
        expect_error(ddbs_concave_hull(argentina_ddbs, conn = 999))
      })
      
      it("validates overwrite argument type", {
        expect_error(ddbs_concave_hull(argentina_ddbs, overwrite = 999))
      })
      
      it("validates quiet argument type", {
        expect_error(ddbs_concave_hull(argentina_ddbs, quiet = 999))
      })
      
      it("validates table name exists", {
        expect_error(ddbs_concave_hull(x = "999", conn = conn_test))
      })
      
      it("requires name to be single character string", {
        expect_error(ddbs_concave_hull(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
      })
    })
  })
})





# 8. ddbs_geometry_type() ---------------------------------------------------

## - CHECK 1.1: works on all formats with by_feature = TRUE
## - CHECK 1.2: works on all formats with by_feature = FALSE
## - CHECK 1.3: messages work
## - CHECK 1.4: returns the right geometry type
## - CHECK 2.1: specific errors
## - CHECK 2.2: general errors
describe("ddbs_geometry_type()", {
  
  describe("expected behavior", {
    
    it("works on all formats with by_feature = TRUE", {
      output_ddbs <- ddbs_geometry_type(points_ddbs, by_feature = TRUE)
      output_sf   <- ddbs_geometry_type(points_sf, by_feature = TRUE)
      output_conn <- ddbs_geometry_type("points", conn = conn_test, by_feature = TRUE)

      expect_s3_class(output_ddbs, "factor")
      expect_equal(output_ddbs, output_sf)
      expect_equal(output_ddbs, output_conn)
    })
    
    it("works on all formats with by_feature = FALSE", {
      output_ddbs <- ddbs_geometry_type(points_ddbs, by_feature = FALSE)
      output_sf   <- ddbs_geometry_type(points_sf, by_feature = FALSE)
      output_conn <- ddbs_geometry_type("points", conn = conn_test, by_feature = FALSE)

      expect_s3_class(output_ddbs, "factor")
      expect_equal(output_ddbs, output_sf)
      expect_equal(output_ddbs, output_conn)
    })
    
    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_geometry_type(argentina_ddbs))
    })
    
    describe("geometry type detection", {
      
      it("correctly identifies POINT geometries", {
        output <- ddbs_geometry_type(points_ddbs, by_feature = FALSE) |> as.character()
        expect_equal(output, "POINT")
      })
      
      it("correctly identifies POLYGON geometries", {
        output <- ddbs_geometry_type(argentina_ddbs, by_feature = FALSE) |> as.character()
        expect_equal(output, "POLYGON")
      })
      
      it("correctly identifies LINESTRING geometries", {
        output <- ddbs_geometry_type(rivers_ddbs, by_feature = FALSE) |> as.character()
        expect_equal(output, "LINESTRING")
      })
    })
  })
  
  describe("errors", {
    
    describe("by_feature parameter validation", {
      
      it("rejects non-logical values", {
        expect_error(ddbs_geometry_type(argentina_ddbs, by_feature = 3))
        expect_error(ddbs_geometry_type(argentina_ddbs, by_feature = "TRUE"))
      })
      
      it("rejects NULL", {
        expect_error(ddbs_geometry_type(argentina_ddbs, by_feature = NULL))
      })
    })
    
    describe("basic argument validation", {
      
      it("requires connection when using table names", {
        expect_error(ddbs_geometry_type("argentina", conn = NULL))
      })
      
      it("validates x argument type", {
        expect_error(ddbs_geometry_type(x = 999))
      })
      
      it("validates conn argument type", {
        expect_error(ddbs_geometry_type(argentina_ddbs, conn = 999))
      })
      
      it("validates table name exists", {
        expect_error(ddbs_geometry_type(x = "999", conn = conn_test))
      })
    })
  })
})



# 9. ddbs_flip_coordinates() -----------------------------------------------

describe("ddbs_flip_coordinates()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_flip_coordinates(argentina_ddbs)
      output_sf   <- ddbs_flip_coordinates(argentina_sf)
      output_conn <- ddbs_flip_coordinates("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_flip_coordinates(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_flip_coordinates(argentina_ddbs))
      expect_message(ddbs_flip_coordinates("argentina", conn = conn_test, name = "flip_coords"))
      expect_message(ddbs_flip_coordinates("argentina", conn = conn_test, name = "flip_coords", overwrite = TRUE))
      expect_true(ddbs_flip_coordinates("argentina", conn = conn_test, name = "flip_coords2"))

      expect_no_message(ddbs_flip_coordinates(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_flip_coordinates("argentina", conn = conn_test, name = "flip_coords", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_flip_coordinates(argentina_ddbs)
      output_tbl  <- ddbs_read_table(conn_test, "flip_coords")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("double flip returns original geometry", {
      flipped_twice <- ddbs_flip_coordinates(argentina_ddbs) |>
        ddbs_flip_coordinates()

      expect_equal(
        ddbs_collect(flipped_twice)$geometry,
        ddbs_collect(argentina_ddbs)$geometry
      )
    })
  })

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_flip_coordinates("argentina", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_flip_coordinates(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_flip_coordinates(argentina_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_flip_coordinates(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_flip_coordinates(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_flip_coordinates(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_flip_coordinates(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})




# 10. ddbs_drop_geometry() --------------------------------------------------

describe("ddbs_drop_geometry()", {

  describe("expected behavior", {

    it("removes the geometry column", {
      output    <- ddbs_drop_geometry(argentina_ddbs)
      col_names <- dplyr::collect(output) |> names()
      expect_false("geometry" %in% col_names)
    })

    it("result is not a duckspatial_df", {
      output <- ddbs_drop_geometry(argentina_ddbs)
      expect_false(inherits(output, "duckspatial_df"))
    })

    it("preserves non-geometry columns", {
      output        <- ddbs_drop_geometry(argentina_ddbs)
      expected_cols <- argentina_sf |> sf::st_drop_geometry() |> names()
      output_cols   <- dplyr::collect(output) |> names()
      expect_true(all(expected_cols %in% output_cols))
    })
  })
})




# 11. ddbs_make_line() -------------------------------------------------------

describe("ddbs_make_line()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_make_line(points_ddbs)
      output_sf   <- ddbs_make_line(points_sf)
      output_conn <- ddbs_make_line("points", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns LINESTRING geometry", {
      output    <- ddbs_make_line(points_ddbs, mode = "sf")
      geom_type <- sf::st_geometry_type(output) |> as.character()
      expect_equal(geom_type, "LINESTRING")
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_make_line(points_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_make_line(points_ddbs))
      expect_message(ddbs_make_line("points", conn = conn_test, name = "make_line"))
      expect_message(ddbs_make_line("points", conn = conn_test, name = "make_line", overwrite = TRUE))
      expect_true(ddbs_make_line("points", conn = conn_test, name = "make_line2"))

      expect_no_message(ddbs_make_line(points_ddbs, quiet = TRUE))
      expect_no_message(ddbs_make_line("points", conn = conn_test, name = "make_line", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_make_line(points_ddbs)
      output_tbl  <- ddbs_read_table(conn_test, "make_line")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    describe("by parameter", {

      it("creates one line per group", {
        pts_grp <- dplyr::mutate(points_sf[1:10, ], grp = rep(c("A", "B"), each = 5))
        output  <- ddbs_make_line(pts_grp, by = "grp", mode = "sf")

        expect_equal(nrow(output), 2L)
      })

      it("preserves group column in output", {
        pts_grp <- dplyr::mutate(points_sf[1:10, ], grp = rep(c("A", "B"), each = 5))
        output  <- ddbs_make_line(pts_grp, by = "grp", mode = "sf")

        expect_true("grp" %in% names(output))
      })
    })
  })

  describe("errors", {

    it("requires POINT geometry", {
      expect_error(ddbs_make_line(argentina_ddbs))
    })

    it("requires connection when using table names", {
      expect_error(ddbs_make_line("points", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_make_line(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_make_line(points_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_make_line(points_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_make_line(points_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_make_line(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_make_line(points_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})




# 12. ddbs_maximum_inscribed_circle() ----------------------------------------

describe("ddbs_maximum_inscribed_circle()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_maximum_inscribed_circle(argentina_ddbs)
      output_sf   <- ddbs_maximum_inscribed_circle(argentina_sf)
      output_conn <- ddbs_maximum_inscribed_circle("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns POINT geometry for center", {
      output    <- ddbs_maximum_inscribed_circle(argentina_ddbs, mode = "sf")
      geom_type <- sf::st_geometry_type(output) |> as.character()
      expect_true(all(geom_type == "POINT"))
    })

    it("includes geom_radius column", {
      output <- ddbs_maximum_inscribed_circle(argentina_ddbs, mode = "sf")
      expect_true("geom_radius" %in% names(output))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_maximum_inscribed_circle(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_maximum_inscribed_circle(argentina_ddbs))
      expect_message(ddbs_maximum_inscribed_circle("argentina", conn = conn_test, name = "max_inscribed"))
      expect_message(ddbs_maximum_inscribed_circle("argentina", conn = conn_test, name = "max_inscribed", overwrite = TRUE))
      expect_true(ddbs_maximum_inscribed_circle("argentina", conn = conn_test, name = "max_inscribed2"))

      expect_no_message(ddbs_maximum_inscribed_circle(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_maximum_inscribed_circle("argentina", conn = conn_test, name = "max_inscribed", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_maximum_inscribed_circle(argentina_ddbs)
      output_tbl  <- ddbs_read_table(conn_test, "max_inscribed")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    describe("geom parameter", {

      it("accepts 'center' (default)", {
        output <- ddbs_maximum_inscribed_circle(argentina_ddbs, geom = "center")
        expect_s3_class(output, "duckspatial_df")
      })

      it("accepts 'nearest'", {
        output <- ddbs_maximum_inscribed_circle(argentina_ddbs, geom = "nearest")
        expect_s3_class(output, "duckspatial_df")
      })
    })

    describe("tolerance parameter", {

      it("works with custom tolerance", {
        output <- ddbs_maximum_inscribed_circle(argentina_ddbs, tolerance = 0.01)
        expect_s3_class(output, "duckspatial_df")
      })
    })
  })

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_maximum_inscribed_circle("argentina", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_maximum_inscribed_circle(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_maximum_inscribed_circle(argentina_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_maximum_inscribed_circle(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_maximum_inscribed_circle(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_maximum_inscribed_circle(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_maximum_inscribed_circle(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})




# 13. ddbs_remove_repeated_points() ------------------------------------------

describe("ddbs_remove_repeated_points()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_remove_repeated_points(argentina_ddbs)
      output_sf   <- ddbs_remove_repeated_points(argentina_sf)
      output_conn <- ddbs_remove_repeated_points("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_remove_repeated_points(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_remove_repeated_points(argentina_ddbs))
      expect_message(ddbs_remove_repeated_points("argentina", conn = conn_test, name = "remove_repeated"))
      expect_message(ddbs_remove_repeated_points("argentina", conn = conn_test, name = "remove_repeated", overwrite = TRUE))
      expect_true(ddbs_remove_repeated_points("argentina", conn = conn_test, name = "remove_repeated2"))

      expect_no_message(ddbs_remove_repeated_points(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_remove_repeated_points("argentina", conn = conn_test, name = "remove_repeated", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_remove_repeated_points(argentina_ddbs)
      output_tbl  <- ddbs_read_table(conn_test, "remove_repeated")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })

    it("removes repeated consecutive vertices", {
      poly_repeated <- sf::st_as_sf(sf::st_sfc(
        sf::st_polygon(list(matrix(
          c(0, 0,  1, 0,  1, 0,  1, 1,  0, 1,  0, 0),
          ncol = 2, byrow = TRUE
        ))),
        crs = 4326
      ))

      output       <- ddbs_remove_repeated_points(poly_repeated, mode = "sf")
      n_pts_before <- nrow(sf::st_coordinates(poly_repeated))
      n_pts_after  <- nrow(sf::st_coordinates(output))

      expect_lt(n_pts_after, n_pts_before)
    })

    describe("tolerance parameter", {

      it("works with default value (0)", {
        output <- ddbs_remove_repeated_points(argentina_ddbs, tolerance = 0)
        expect_s3_class(output, "duckspatial_df")
      })

      it("works with custom tolerance", {
        output <- ddbs_remove_repeated_points(argentina_ddbs, tolerance = 0.01)
        expect_s3_class(output, "duckspatial_df")
      })
    })
  })

  describe("errors", {

    describe("tolerance parameter validation", {

      it("rejects negative values", {
        expect_error(ddbs_remove_repeated_points(argentina_ddbs, tolerance = -1))
      })

      it("rejects non-numeric values", {
        expect_error(ddbs_remove_repeated_points(argentina_ddbs, tolerance = "0.5"))
      })
    })

    describe("basic argument validation", {

      it("requires connection when using table names", {
        expect_error(ddbs_remove_repeated_points("argentina", conn = NULL))
      })

      it("validates x argument type", {
        expect_error(ddbs_remove_repeated_points(x = 999))
      })

      it("validates conn argument type", {
        expect_error(ddbs_remove_repeated_points(argentina_ddbs, conn = 999))
      })

      it("validates overwrite argument type", {
        expect_error(ddbs_remove_repeated_points(argentina_ddbs, overwrite = 999))
      })

      it("validates quiet argument type", {
        expect_error(ddbs_remove_repeated_points(argentina_ddbs, quiet = 999))
      })

      it("validates table name exists", {
        expect_error(ddbs_remove_repeated_points(x = "999", conn = conn_test))
      })

      it("requires name to be single character string", {
        expect_error(ddbs_remove_repeated_points(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
      })
    })
  })
})




# N. ddbs_vertices() -------------------------------------------------------

describe("ddbs_vertices()", {

  describe("expected behavior", {

    it("works on all formats", {
      output_ddbs <- ddbs_vertices(argentina_ddbs)
      output_sf   <- ddbs_vertices(argentina_sf)
      output_conn <- ddbs_vertices("argentina", conn = conn_test)

      expect_s3_class(output_ddbs, "duckspatial_df")
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_sf))
      expect_equal(ddbs_collect(output_ddbs), ddbs_collect(output_conn))
    })

    it("returns MULTIPOINT geometries", {
      output <- ddbs_vertices(argentina_ddbs, mode = "sf")
      expect_true(all(sf::st_geometry_type(output) == "MULTIPOINT"))
    })

    it("returns different output formats (duckspatial_df, sf)", {
      output_sf_fmt <- ddbs_vertices(argentina_ddbs, mode = "sf")
      expect_s3_class(output_sf_fmt, "sf")
    })

    it("shows and suppresses messages correctly", {
      expect_no_message(ddbs_vertices(argentina_ddbs))
      expect_message(ddbs_vertices("argentina", conn = conn_test, name = "vertices"))
      expect_message(ddbs_vertices("argentina", conn = conn_test, name = "vertices", overwrite = TRUE))
      expect_true(ddbs_vertices("argentina", conn = conn_test, name = "vertices2"))

      expect_no_message(ddbs_vertices(argentina_ddbs, quiet = TRUE))
      expect_no_message(ddbs_vertices("argentina", conn = conn_test, name = "vertices", overwrite = TRUE, quiet = TRUE))
    })

    it("writes tables to the database", {
      output_ddbs <- ddbs_vertices(argentina_ddbs)
      output_tbl  <- ddbs_read_table(conn_test, "vertices")

      expect_equal(
        ddbs_collect(output_ddbs)$geometry,
        output_tbl$geometry
      )
    })
  })

  describe("errors", {

    it("requires connection when using table names", {
      expect_error(ddbs_vertices("argentina", conn = NULL))
    })

    it("validates x argument type", {
      expect_error(ddbs_vertices(x = 999))
    })

    it("validates conn argument type", {
      expect_error(ddbs_vertices(argentina_ddbs, conn = 999))
    })

    it("validates overwrite argument type", {
      expect_error(ddbs_vertices(argentina_ddbs, overwrite = 999))
    })

    it("validates quiet argument type", {
      expect_error(ddbs_vertices(argentina_ddbs, quiet = 999))
    })

    it("validates table name exists", {
      expect_error(ddbs_vertices(x = "999", conn = conn_test))
    })

    it("requires name to be single character string", {
      expect_error(ddbs_vertices(argentina_ddbs, conn = conn_test, name = c('banana', 'banana')))
    })
  })
})


## stop connection
ddbs_stop_conn(conn_test)
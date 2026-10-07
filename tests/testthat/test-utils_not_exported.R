
# 0. Set up --------------------------------------------------------------

## skip tests on CRAN because they take too much time
skip_if(Sys.getenv("TEST_ONE") != "")
testthat::skip_on_cran()
testthat::skip_if_not_installed("duckdb")

## create duckdb connection
conn_test <- duckspatial::ddbs_create_conn()

## 4 squares in a projected CRS, with a reserved-word column and a
## geometry column whose name contains a space
sq <- function(x0, y0) sf::st_polygon(list(matrix(c(x0, y0, x0 + 10, y0, x0 + 10, y0 + 10, x0, y0 + 10, x0, y0), ncol = 2, byrow = TRUE)))
squares_sf <- sf::st_sf(
  id    = 1:4,
  group = c("a", "a", "b", "b"),
  geometry = sf::st_sfc(sq(0, 0), sq(10, 0), sq(0, 10), sq(10, 10), crs = 3857)
)
spaced_sf <- squares_sf
names(spaced_sf)[names(spaced_sf) == "geometry"] <- "my geom"
sf::st_geometry(spaced_sf) <- "my geom"


# 1. sql_ident() -----------------------------------------------------------

describe("sql_ident()", {

  it("wraps identifiers in double quotes and escapes embedded quotes", {
    expect_equal(sql_ident("geometry"), '"geometry"')
    expect_equal(sql_ident("my geom"), '"my geom"')
    expect_equal(sql_ident('we"ird'), '"we""ird"')
    expect_equal(sql_ident(c("a", "group")), c('"a"', '"group"'))
  })

})


# 2. Identifier quoting in SQL (#168) --------------------------------------

describe("column names with spaces or reserved words (#168)", {

  it("work as geometry column in unary, measure and affine functions", {
    expect_equal(attr(ddbs_collect(ddbs_buffer(spaced_sf, 1)), "sf_column"), "my geom")
    expect_equal(as.numeric(ddbs_area(spaced_sf, mode = "sf")), rep(100, 4))
    expect_s3_class(ddbs_collect(ddbs_centroid(spaced_sf)), "sf")
    expect_s3_class(ddbs_collect(ddbs_rotate(spaced_sf, 10)), "sf")
  })

  it("work as geometry column in binary functions, predicates and joins", {
    expect_s3_class(ddbs_collect(ddbs_join(spaced_sf, spaced_sf)), "sf")
    expect_s3_class(ddbs_collect(ddbs_intersection(spaced_sf, spaced_sf)), "sf")
    expect_no_error(dplyr::collect(ddbs_intersects(spaced_sf, spaced_sf)))
    expect_equal(nrow(ddbs_filter(spaced_sf, spaced_sf[1, ], mode = "sf")), nrow(sf::st_filter(spaced_sf, spaced_sf[1, ])))
  })

  it("escape embedded double quotes in the geometry column", {
    weird_sf <- squares_sf
    names(weird_sf)[names(weird_sf) == "geometry"] <- 'we"ird'
    sf::st_geometry(weird_sf) <- 'we"ird'
    expect_s3_class(ddbs_collect(ddbs_centroid(weird_sf)), "sf")
  })

  it("work as `by` and `new_column`", {
    expect_equal(nrow(ddbs_collect(ddbs_union_agg(squares_sf, by = "group"))), 2)
    squares_sf[["my group"]] <- squares_sf$group
    expect_equal(nrow(ddbs_collect(ddbs_union_agg(squares_sf, by = "my group"))), 2)
    expect_true("my area" %in% names(ddbs_collect(ddbs_area(squares_sf, new_column = "my area"))))
  })

  it("work in ddbs_interpolate_aw() columns", {
    source_sf <- squares_sf
    source_sf[["my value"]] <- c(1, 2, 3, 4)
    target_sf <- squares_sf[, "id"]
    names(target_sf)[1] <- "target id"
    res <- ddbs_interpolate_aw(
      target_sf, source_sf, tid = "target id", sid = "id",
      extensive = "my value", mode = "sf"
    )
    expect_equal(res[["my value"]][order(res[["target id"]])], c(1, 2, 3, 4))
  })

  it("work as schema name in ddbs_create_schema()", {
    expect_no_error(ddbs_create_schema(conn_test, "my schema", quiet = TRUE))
    schemas <- DBI::dbGetQuery(conn_test, "SELECT schema_name FROM information_schema.schemata")
    expect_true("my schema" %in% schemas$schema_name)
  })

  it("quotes table names in sql_table()", {
    expect_equal(sql_table("my table"), '"my table"')
    expect_equal(sql_table("s1.out"), '"s1"."out"')
    expect_equal(sql_table(c("my schema", "my table")), '"my schema"."my table"')
    expect_equal(sql_table('"already quoted"'), '"already quoted"')
  })

  it("keep dotted schema.table output names working", {
    ddbs_create_schema(conn_test, "s1", quiet = TRUE)
    ddbs_area(squares_sf, conn = conn_test, name = "s1.out", quiet = TRUE)
    expect_equal(DBI::dbGetQuery(conn_test, "SELECT count(*) AS n FROM s1.out")$n, 4)
  })

})


# 3. Table names with spaces or reserved words (#168) ----------------------

describe("table names with spaces or reserved words (#168)", {

  ddbs_write_table(conn_test, squares_sf, "squares", overwrite = TRUE, quiet = TRUE)

  it("work as output names", {
    ddbs_write_table(conn_test, squares_sf, "my table", quiet = TRUE)
    ddbs_write_table(conn_test, squares_sf, "my table", overwrite = TRUE, quiet = TRUE)
    ddbs_write_table(conn_test, squares_sf, "order", quiet = TRUE)
    ddbs_area(squares_sf, conn = conn_test, name = "my area", quiet = TRUE)
    ddbs_union_agg(squares_sf, by = "group", conn = conn_test, name = "my union", quiet = TRUE)
    ddbs_rotate(squares_sf, 10, conn = conn_test, name = "my rot", quiet = TRUE)
    ddbs_transform(squares_sf, 4326, conn = conn_test, name = "my tr", quiet = TRUE)
    n <- function(t) DBI::dbGetQuery(conn_test, paste0("SELECT count(*) AS n FROM ", t))$n
    expect_equal(n('"my table"'), 4)
    expect_equal(n('"order"'), 4)
    expect_equal(n('"my area"'), 4)
    expect_equal(n('"my union"'), 2)
    expect_equal(n('"my rot"'), 4)
    expect_equal(n('"my tr"'), 4)
    expect_true("my table" %in% ddbs_list_tables(conn_test)$table_name)
  })

  it("work as output names in a quoted schema", {
    DBI::dbExecute(conn_test, 'CREATE SCHEMA IF NOT EXISTS "my schema"')
    for (i in 1:2) ddbs_area(squares_sf, conn = conn_test, name = "my schema.my area", overwrite = TRUE, quiet = TRUE)
    expect_equal(DBI::dbGetQuery(conn_test, 'SELECT count(*) AS n FROM "my schema"."my area"')$n, 4)
  })

  it("work as registered view names", {
    for (i in 1:2) ddbs_register_table(conn_test, squares_sf, "my view", overwrite = TRUE, quiet = TRUE)
    expect_equal(nrow(ddbs_read_table(conn_test, "my view")), 4)
  })

  it("work as input names", {
    DBI::dbExecute(conn_test, 'CREATE OR REPLACE TABLE "group" AS SELECT * FROM squares')
    expect_equal(nrow(ddbs_read_table(conn_test, "group")), 4)
    expect_no_error(utils::capture.output(ddbs_glimpse(conn_test, "group")))
    expect_equal(ddbs_crs(conn_test, "group"), sf::st_crs(3857))
    expect_equal(as.numeric(ddbs_area("group", conn = conn_test, mode = "sf")), rep(100, 4))
    expect_equal(nrow(ddbs_collect(ddbs_join("group", "squares", conn = conn_test))), nrow(sf::st_join(squares_sf, squares_sf)))
    expect_equal(nrow(ddbs_collect(as_duckspatial_df("group", conn = conn_test))), 4)
  })

  it("do not leave temporary views behind for character input", {
    views <- function() DBI::dbGetQuery(conn_test, "SELECT count(*) AS n FROM duckdb_views() WHERE NOT internal")$n
    before <- views()
    ddbs_area("group", conn = conn_test, mode = "sf")
    ddbs_collect(ddbs_buffer("group", 1, conn = conn_test))
    expect_equal(views(), before)
  })

})


## stop connection
ddbs_stop_conn(conn_test)

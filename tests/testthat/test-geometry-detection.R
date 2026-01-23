# Helper check for native CRS availability if needed
ddbs_has_native_crs <- function(conn) {
    tryCatch({
        DBI::dbExecute(conn, "CREATE TEMP TABLE t_test_crs (g GEOMETRY(EPSG:4326));")
        TRUE
    }, error = function(e) FALSE)
}

test_that("detect_geometry_column works correctly", {
    conn <- ddbs_temp_conn()
    
    # helper to create table with specific geometry type
    create_table <- function(name, type = "GEOMETRY", col_name = "geom") {
        DBI::dbExecute(conn, glue::glue("CREATE TABLE {name} (id INTEGER, {col_name} {type});"))
    }
    
    # 1. Standard GEOMETRY type
    create_table("t_geom", "GEOMETRY", "geometry")
    tbl_geom <- dplyr::tbl(conn, "t_geom")
    expect_equal(detect_geometry_column(tbl_geom, "t_geom"), "geometry")
    expect_equal(detect_geometry_column(tbl_geom), "geometry") # without source_table
    
    # 2. Native CRS GEOMETRY(EPSG:...)
    if (ddbs_has_native_crs(conn)) { # Assuming we might want to guard if DuckDB version is old, but we assume new here
        create_table("t_epsg", "GEOMETRY(EPSG:4326)", "my_geom")
        tbl_epsg <- dplyr::tbl(conn, "t_epsg")
        expect_equal(detect_geometry_column(tbl_epsg, "t_epsg"), "my_geom")
    }
    
    # 3. Fallback when no GEOMETRY type found
    create_table("t_blob", "BLOB", "wkb")
    tbl_blob <- dplyr::tbl(conn, "t_blob")
    expect_equal(detect_geometry_column(tbl_blob, "t_blob"), "geom") # defaults to "geom"
    
    # 4. Quoted identifiers (table with space)
    create_table('"table space"', "GEOMETRY", "geom_space")
    tbl_space <- dplyr::tbl(conn, dbplyr::ident("table space"))
    # The source_table argument should be the RAW table name, detect_geometry_column handles quoting
    expect_equal(detect_geometry_column(tbl_space, "table space"), "geom_space")
    # Also test auto-extraction if remote_name works (it should for ident)
    expect_equal(detect_geometry_column(tbl_space), "geom_space")
    
    # 5. Lazy query (complex SQL)
    # SELECT * FROM t_geom WHERE id = 1
    tbl_lazy <- tbl_geom |> dplyr::filter(id == 1)
    # Should detect from subquery describe
    expect_equal(detect_geometry_column(tbl_lazy), "geometry")
    
    # 6. NULL/Empty (Fallbacks)
    expect_equal(detect_geometry_column(dplyr::tbl(conn, "t_blob")), "geom")
})

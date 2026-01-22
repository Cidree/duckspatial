test_that("duckspatial_df dplyr verbs work correctly", {
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc_ds <- as_duckspatial_df(nc_sf)
  
  # 1. filter()
  filtered <- nc_ds |> dplyr::filter(BIR74 > 1000)
  expect_s3_class(filtered, "duckspatial_df")
  expect_equal(attr(filtered, "sf_column"), "geometry")
  
  # 2. select() - sticky geometry
  selected <- nc_ds |> dplyr::select(BIR74)
  expect_s3_class(selected, "duckspatial_df")
  expect_true("geometry" %in% colnames(selected))
  expect_true("BIR74" %in% colnames(selected))
  
  # 3. rename()
  renamed <- nc_ds |> dplyr::rename(my_geom = geometry)
  expect_equal(attr(renamed, "sf_column"), "my_geom")
  expect_true("my_geom" %in% colnames(renamed))
  
  # 4. rename_with()
  renamed_w <- nc_ds |> dplyr::rename_with(toupper)
  expect_equal(attr(renamed_w, "sf_column"), "GEOMETRY")
  expect_true("GEOMETRY" %in% colnames(renamed_w))
  
  # 5. pull() - spatial
  pilled_geom <- nc_ds |> dplyr::pull(geometry)
  expect_s3_class(pilled_geom, "sfc")
  
  # 6. mutate(geom = NULL) - smart reconstruction drops class
  mutated <- nc_ds |> dplyr::mutate(geometry = NULL)
  expect_false(inherits(mutated, "duckspatial_df"))
  
  # 7. transmute() - sticky geometry
  transmuted <- nc_ds |> dplyr::transmute(BIR74)
  expect_s3_class(transmuted, "duckspatial_df")
  expect_true("geometry" %in% colnames(transmuted))
})

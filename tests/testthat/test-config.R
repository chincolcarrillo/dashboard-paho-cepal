source("r/00_config.R", encoding = "UTF-8")

test_that("la taxonomía hc_cat2 contiene las siete categorías normativas", {
  expect_length(hc_cat2_levels, 7)
  expect_false(anyDuplicated(hc_cat2_levels) > 0)
  expect_setequal(
    hc_cat2_levels,
    c(
      "Células humanas, tejidos y productos médicos de terapia avanzada",
      "Diagnósticos in vitro",
      "Dispositivos médicos",
      "Hemoderivados, antisueros y productos inmunobiológicos",
      "Ingredientes farmacéuticos activos",
      "Medicamentos",
      "Vacunas (humanas)"
    )
  )
})

test_that("Europa y Asia Central son regiones diferentes", {
  expect_true(all(c("Europe", "Central Asia") %in% region_levels))
  expect_false("Europe & Central Asia" %in% region_levels)
  expect_true(unclassified_region %in% region_levels)
  expect_equal(unclassified_region, "No clasificada")
  expect_false(anyDuplicated(region_levels) > 0)
})

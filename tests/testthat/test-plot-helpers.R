source("r/02_functions_plots.R", encoding = "UTF-8")

test_that("prepare_country_snapshot devuelve una fila por país para el año solicitado", {
  trade_balance <- tibble::tribble(
    ~year, ~ref_area_code, ~ref_area_name, ~ref_area_type, ~exports_1000usd, ~imports_1000usd, ~balance_1000usd,
    2023L, "MEX", "Mexico", "country", 100, 50, 50,
    2024L, "MEX", "Mexico", "country", 200, 80, 120,
    2024L, "BRA", "Brazil", "country", 20, 300, -280,
    2024L, "LAC", "Latin America & Caribbean", "region", 220, 380, -160
  )

  snapshot <- prepare_country_snapshot(trade_balance, 2024L)

  expect_setequal(snapshot$ref_area_code, c("MEX", "BRA"))
  expect_equal(unique(snapshot$year), 2024L)
  expect_equal(anyDuplicated(snapshot$ref_area_code), 0)
  expect_equal(snapshot$exports_1000usd[snapshot$ref_area_code == "MEX"], 200)
})

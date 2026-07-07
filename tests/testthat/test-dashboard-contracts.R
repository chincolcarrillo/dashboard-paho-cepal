source("r/00_config.R", encoding = "UTF-8")

read_dashboard_fixture <- function(file_name) {
  file_path <- file.path(dashboard_data_dir, file_name)
  skip_if_not(file.exists(file_path), paste("Falta", file_path))
  readRDS(file_path)
}

test_that("las exportaciones regionales respetan categoría, clave y shares", {
  data <- read_dashboard_fixture("exports_region_hc_cat2.rds")

  expect_setequal(unique(data$hc_cat2), hc_cat2_levels)
  expect_equal(
    anyDuplicated(data[c("year", "exp_region", "hc_cat2")]),
    0
  )

  share_totals <- data |>
    dplyr::group_by(year, hc_cat2) |>
    dplyr::summarise(total = sum(share_exports_value), .groups = "drop")
  expect_equal(share_totals$total, rep(1, nrow(share_totals)), tolerance = 1e-8)
})

test_that("el balance comercial cumple su identidad", {
  data <- read_dashboard_fixture("trade_balance_lac.rds")

  expect_equal(
    data$balance_1000usd,
    data$exports_1000usd - data$imports_1000usd,
    tolerance = 1e-8
  )
  expect_equal(
    anyDuplicated(data[c("year", "ref_area_code", "ref_area_type", "hc_cat2")]),
    0
  )
})

test_that("las regiones procesadas pertenecen al contrato", {
  exports <- read_dashboard_fixture("exports_region_hc_cat2.rds")
  partners <- read_dashboard_fixture("partner_region_lac_2024.rds")

  expect_false(anyNA(exports$exp_region))
  expect_false(anyNA(partners$partner_region))
  expect_setequal(setdiff(unique(exports$exp_region), region_levels), character())
  expect_setequal(setdiff(unique(partners$partner_region), region_levels), character())
})

test_that("la base común de productos exportados respeta categorías y clave", {
  data <- read_dashboard_fixture("product_exports_lac_2024_by_country.rds")

  expect_setequal(setdiff(unique(data$hc_cat2), hc_cat2_levels), character())
  expect_equal(
    anyDuplicated(data[c("year", "hc_cat2", "product", "exporter")]),
    0
  )
  expect_false(anyNA(data$exports_1000usd))
})

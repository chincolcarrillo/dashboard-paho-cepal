#  ----------- INDICADORES DE COMERCIO PARA DASHBOARD -----------

# Este script contiene funciones para calcular indicadores derivados a partir
# del universo completo de BACI. La idea es mantener estos calculos separados
# de las bases PAHO, porque varios indicadores de especialización/complejidad
# requieren comparar productos de salud contra la canasta exportadora completa.

# RCA_cp = (X_cp / X_c) / (X_wp / X_w)
#   X_cp son las exportaciones del country c del producto p
#   X_c total exportado por c
#   X_wp exports world del producto p
#   X_w total exports world

calculate_product_country_rca <- function(
  baci_data,
  indicator_year,
  countries_exp = NULL
) {
  required_cols <- c("year", "exporter", "product", "value_1000usd")
  missing_cols <- setdiff(required_cols, names(baci_data))
  if (length(missing_cols) > 0) {
    rlang::abort(glue::glue(
      "'baci_data' no contiene columnas requeridas: {paste(missing_cols, collapse = ', ')}."
    ))
  }

  product_country_exports <- baci_data |> # X_cp
    dplyr::filter(.data$year == .env$indicator_year) |> # Se filtra el anio de interes
    dplyr::mutate(
      year = as.integer(year),
      exporter = as.character(exporter),
      product = as.character(product)
    ) |>
    dplyr::group_by(year, exporter, product) |>
    dplyr::summarise(
      exports_1000usd = sum(value_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::collect()

  country_totals <- product_country_exports |> # X_c
    dplyr::group_by(year, exporter) |>
    dplyr::summarise(
      country_total_exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    )

  world_product_totals <- product_country_exports |> # X_wp
    dplyr::group_by(year, product) |>
    dplyr::summarise(
      world_product_exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    )

  world_totals <- product_country_exports |> # X_w
    dplyr::group_by(year) |>
    dplyr::summarise(
      world_total_exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    )

  indicators <- product_country_exports |>
    dplyr::left_join(country_totals, by = c("year", "exporter")) |>
    dplyr::left_join(world_product_totals, by = c("year", "product")) |>
    dplyr::left_join(world_totals, by = "year") |>
    dplyr::mutate(
      country_product_share = dplyr::if_else(
        country_total_exports_1000usd > 0,
        exports_1000usd / country_total_exports_1000usd,
        NA_real_
      ),
      world_product_share = dplyr::if_else(
        world_total_exports_1000usd > 0,
        world_product_exports_1000usd / world_total_exports_1000usd,
        NA_real_
      ),
      rca_balassa = dplyr::if_else(
        world_product_share > 0,
        country_product_share / world_product_share,
        NA_real_
      )
    )

  if (!is.null(countries_exp)) {
    indicators <- indicators |> # agrega columnas de contexto
      dplyr::left_join(
        countries_exp |>
          dplyr::mutate(exporter = as.character(country_code)),
        by = "exporter"
      )
  }

  indicators |>
    dplyr::arrange(year, exporter, product)
}

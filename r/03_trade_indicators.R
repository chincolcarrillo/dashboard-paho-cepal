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

  indicator_years <- unique(as.integer(indicator_year))
  if (length(indicator_years) == 0 || anyNA(indicator_years)) {
    rlang::abort("'indicator_year' debe contener al menos un año válido.")
  }

  product_country_exports <- baci_data |> # X_cp
    dplyr::filter(.data$year %in% .env$indicator_years) |> # Uno o mas anios de interes
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

# Identificar si un par producto-país cumple los criterios usados en la tabla
# del dashboard. Mantener esta lógica centralizada evita diferencias entre la
# capa de presentación y los análisis internos.
classify_dashboard_product_country_pairs <- function(
  data,
  rca_threshold = dashboard_product_rca_threshold,
  exports_threshold_1000usd = dashboard_product_exports_threshold_1000usd
) {
  required_cols <- c("exports_1000usd", "rca_balassa")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(glue::glue(
      "La base no contiene columnas requeridas: {paste(missing_cols, collapse = ', ')}."
    ))
  }

  data |>
    dplyr::mutate(
      eligible_by_rca = dplyr::coalesce(
        .data$rca_balassa > .env$rca_threshold,
        FALSE
      ),
      eligible_by_exports = dplyr::coalesce(
        .data$exports_1000usd > .env$exports_threshold_1000usd,
        FALSE
      ),
      dashboard_table_eligible = .data$eligible_by_rca | .data$eligible_by_exports,
      dashboard_table_criterion = dplyr::case_when(
        .data$eligible_by_rca & .data$eligible_by_exports ~ "RCA y volumen de exportación",
        .data$eligible_by_rca ~ "RCA",
        .data$eligible_by_exports ~ "Volumen de exportación",
        TRUE ~ "No cumple los umbrales"
      )
    )
}

# Seleccionar pares producto-país que aparecen en la tabla del año de corte,
# pero que no habrían aparecido con los mismos criterios en el año de referencia.
identify_emerging_product_country_pairs <- function(
  current_products,
  reference_indicators,
  cutoff_year,
  reference_year,
  rca_threshold = dashboard_product_rca_threshold,
  exports_threshold_1000usd = dashboard_product_exports_threshold_1000usd
) {
  current_required <- c(
    "year", "hc_cat2", "product", "exporter", "exp_country_name",
    "exports_1000usd", "rca_balassa"
  )
  reference_required <- c(
    "year", "product", "exporter", "exports_1000usd", "rca_balassa"
  )

  missing_current <- setdiff(current_required, names(current_products))
  missing_reference <- setdiff(reference_required, names(reference_indicators))
  if (length(missing_current) > 0) {
    rlang::abort(glue::glue(
      "'current_products' no contiene columnas requeridas: {paste(missing_current, collapse = ', ')}."
    ))
  }
  if (length(missing_reference) > 0) {
    rlang::abort(glue::glue(
      "'reference_indicators' no contiene columnas requeridas: {paste(missing_reference, collapse = ', ')}."
    ))
  }

  if (!cutoff_year %in% unique(current_products$year)) {
    rlang::abort(glue::glue(
      "El año de corte {cutoff_year} no está disponible en 'current_products'."
    ))
  }
  if (!reference_year %in% unique(reference_indicators$year)) {
    rlang::abort(glue::glue(
      "El año de referencia {reference_year} no está disponible en 'reference_indicators'."
    ))
  }

  current_eligible <- current_products |>
    dplyr::filter(.data$year == .env$cutoff_year) |>
    classify_dashboard_product_country_pairs(
      rca_threshold = rca_threshold,
      exports_threshold_1000usd = exports_threshold_1000usd
    ) |>
    dplyr::filter(.data$dashboard_table_eligible) |>
    dplyr::rename(
      cutoff_year = year,
      exports_cutoff_1000usd = exports_1000usd,
      rca_cutoff = rca_balassa,
      eligible_by_rca_cutoff = eligible_by_rca,
      eligible_by_exports_cutoff = eligible_by_exports,
      eligibility_criterion_cutoff = dashboard_table_criterion
    ) |>
    dplyr::select(-dashboard_table_eligible)

  reference_status <- reference_indicators |>
    dplyr::filter(.data$year == .env$reference_year) |>
    classify_dashboard_product_country_pairs(
      rca_threshold = rca_threshold,
      exports_threshold_1000usd = exports_threshold_1000usd
    ) |>
    dplyr::transmute(
      exporter,
      product,
      exports_reference_1000usd = exports_1000usd,
      rca_reference = rca_balassa,
      eligible_by_rca_reference = eligible_by_rca,
      eligible_by_exports_reference = eligible_by_exports,
      dashboard_table_eligible_reference = dashboard_table_eligible
    )

  current_eligible |>
    dplyr::left_join(reference_status, by = c("exporter", "product")) |>
    dplyr::mutate(
      reference_year = as.integer(.env$reference_year),
      exports_reference_1000usd = dplyr::coalesce(
        .data$exports_reference_1000usd,
        0
      ),
      eligible_by_rca_reference = dplyr::coalesce(
        .data$eligible_by_rca_reference,
        FALSE
      ),
      eligible_by_exports_reference = dplyr::coalesce(
        .data$eligible_by_exports_reference,
        FALSE
      ),
      dashboard_table_eligible_reference = dplyr::coalesce(
        .data$dashboard_table_eligible_reference,
        FALSE
      )
    ) |>
    dplyr::filter(!.data$dashboard_table_eligible_reference) |>
    dplyr::mutate(
      exports_change_1000usd =
        .data$exports_cutoff_1000usd - .data$exports_reference_1000usd,
      exports_ratio_vs_reference = dplyr::if_else(
        .data$exports_reference_1000usd > 0,
        .data$exports_cutoff_1000usd / .data$exports_reference_1000usd,
        NA_real_
      ),
      rca_change = .data$rca_cutoff - .data$rca_reference
    ) |>
    dplyr::select(
      cutoff_year,
      hc_cat2,
      product,
      description,
      description_short,
      exporter,
      exp_country_name,
      exports_cutoff_1000usd,
      rca_cutoff,
      reference_year,
      exports_change_1000usd,
      exports_ratio_vs_reference,
      rca_change
    ) |>
    dplyr::arrange(
      .data$hc_cat2,
      dplyr::desc(.data$exports_cutoff_1000usd),
      dplyr::desc(.data$rca_cutoff),
      .data$exp_country_name,
      .data$product
    )
}

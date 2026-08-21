#  ----------- PROCESAMIENTO CEPII-BACI 07 PARA PRODUCTOS DEL AREA SALUD -----------

# 0. SET-UP ----

# cargar paquetes
if (!requireNamespace("pacman", quietly = TRUE)) {
  stop("Instale el paquete 'pacman' antes de ejecutar este script.", call. = FALSE)
}
library(pacman)
p_load(tidyverse,
       glue,
       janitor,
       readxl,
       writexl,
       fs,
       duckdb,
       arrow)

source("r/00_config.R", encoding = "UTF-8")
source("r/00_ingest_baci.R", encoding = "UTF-8")
source("r/03_trade_indicators.R", encoding = "UTF-8")

# evitar notacion cientifica
options(scipen = 999)

# 1. CARGAR DATA BACI ----

# ZIP disponible en "https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS07_V202601.zip"
prepare_baci_parquet()

baci <- open_dataset("data/raw/parquet")


# 2. IDENTIFICAR PAISES Y REGIONES -----

# Codigos BACI
paises <-  read_csv("data/raw/country_codes_V202601.csv")

# World Bank Country and Lending Groups
regiones <- read_excel("data/country-class.xlsx") |>
  clean_names() 

paises <- paises |>
  left_join(regiones, by = c("country_iso3" = "code")) |>
  mutate(country_code = as.integer(country_code),
         region_longname = case_when(region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "Oriente Medio, Norte de África, Afganistán y Pakistán",
                                     region == "Latin America & Caribbean" ~ "América Latina y el Caribe",
                                     region == "East Asia & Pacific" ~ "Asia oriental y el Pacífico",
                                     region == "Central Asia" ~ "Asia central",
                                     region == "Europe" ~ "Europa",
                                     region == "North America" ~ "América del Norte",
                                     region == "South Asia" ~ "Asia meridional",
                                     region == "Sub-Saharan Africa" ~ "África al sur del Sahara",
                                     TRUE ~ unclassified_region) ,
         region = case_when(region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENA",
                            region == "Latin America & Caribbean" ~ "LAC",
                            region == "East Asia & Pacific" ~ "Asia oriental y el Pacífico",
                            region == "Central Asia" ~ "Asia central",
                            region == "Europe" ~ "Europa",
                            region == "North America" ~ "América del Norte",
                            region == "South Asia" ~ "Asia meridional",
                            region == "Sub-Saharan Africa" ~ "África al sur del Sahara",
                            TRUE ~ unclassified_region)) |>
  mutate(
    region = if_else(
      country_code == 490L, # Proxy de Taiwan
      "Asia oriental y el Pacífico", 
      region),
    region = if_else(
      country_code %in% c(533L, 531L, 796L, 92L, 534L), # Aruba, Curazao, Turks and Caicos, Br Virgin, Saint Maarten
      unclassified_region,
      region),
    region_longname = if_else(
      country_code == 490L, # Proxy de Taiwan
      "Asia oriental y el Pacífico",
      region_longname),
    region_longname = if_else(  # Todas las que no estan clasificadas en region, quedan como no clasificadas en longname
      region == unclassified_region,
      unclassified_region,
      region_longname
    ),
    region_longname = replace_na(region_longname, unclassified_region)
  ) |>
  select(country_code, country_name, region, region_longname)

# paises_no_clasificados <- paises |> filter(region == unclassified_region)
# Paises sin region WB quedan etiquetados explicitamente para evitar NA en el dashboard.

paises <- paises |> # arreglar nombres en ingles
  mutate(country_name = case_when(country_name == "Antigua and Barbuda" ~ "Antigua y Barbuda",
                                  country_name == "Bolivia (Plurinational State of)" ~ "Bolivia",
                                  country_name == "Brazil" ~ "Brasil",
                                  country_name == "Belize" ~ "Belice",
                                  country_name == "Dominican Rep." ~ "República Dominicana",
                                  country_name == "Grenada" ~ "Granada",
                                  country_name == "Guyana" ~ "Guayana",
                                  country_name == "Haiti" ~ "Haití",
                                  country_name == "Mexico" ~ "México",
                                  country_name == "Panama" ~ "Panamá",
                                  country_name == "Peru" ~ "Perú",
                                  country_name == "Saint Kitts and Nevis" ~ "San Cristóbal y Nieves",
                                  country_name == "Saint Lucia" ~ "Santa Lucía",
                                  country_name == "Saint Vincent and the Grenadines" ~ "San Vicente y las Granadinas",
                                  country_name == "Suriname" ~ "Surinam",
                                  country_name == "Trinidad and Tobago" ~ "Trinidad y Tobago",
                                  TRUE ~ country_name))

# Dejar columnas listas para unir con base BACI
paises_imp <- paises |> rename_with(~ paste0("imp_", .), -country_code)
paises_exp <- paises |> rename_with(~ paste0("exp_", .), -country_code)


# 3. SELECCIONAR PRODUCTOS AREA SALUD -----

# Categorias acordadas con MEPP (experta PAHO) de acuerdo a HS07
productos <-  read_excel("data/rev_paho_2026.xlsx", 
                         sheet = "exportable")

productos <- productos |>
  mutate(
    code = as.character(code),
    hc_cat2 = as.character(hc_cat2)
  ) |>
  filter(hc == 1, !is.na(hc_cat2))

unexpected_categories <- productos |>
  distinct(hc_cat2) |>
  filter(!hc_cat2 %in% hc_cat2_levels) |>
  pull(hc_cat2)

if (length(unexpected_categories) > 0) {
  rlang::abort(glue::glue(
    "Cats hc_cat2 inesperadas: {paste(unexpected_categories, collapse = ', ')}"
  ))
}

comercio_hc_world <- baci |> 
  mutate(product = as.character(product)) |>
  inner_join(productos, by = c("product" = "code")) |>
  left_join(paises_imp, by = c("importer" = "country_code")) |>
  left_join(paises_exp, by = c("exporter" = "country_code")) |>
  collect()

# 4. CREAR DFs PARA DASHBOARD -----
# >5millones de observaciones, mejor crear data sets parciales y mas livianos para el dashboard

replace_missing_trade_labels <- function(data) {
  data |>
    mutate(
      exp_region = replace_na(as.character(exp_region), unclassified_region),
      imp_region = replace_na(as.character(imp_region), unclassified_region),
      exp_region_longname = replace_na(
        as.character(exp_region_longname),
        unclassified_region
      ),
      imp_region_longname = replace_na(
        as.character(imp_region_longname),
        unclassified_region
      ),
      exporter = replace_na(as.character(exporter), unclassified_region),
      importer = replace_na(as.character(importer), unclassified_region),
      exp_country_name = replace_na(
        as.character(exp_country_name),
        unclassified_region
      ),
      imp_country_name = replace_na(
        as.character(imp_country_name),
        unclassified_region
      )
    )
}

## Base minima ----
comercio_hc_min <- comercio_hc_world |>
  select(-quantity_tons, -description, -description_short) |>
  replace_missing_trade_labels() |>
  mutate(
    year = as.integer(year),
    # Manejo explicito de datos perdidos en variables categoricas clave
    # Evita que group_by() produzca categorias NA difíciles de interpretar en el dashboard
    hc_cat2 = as.character(hc_cat2)
  )

if (anyNA(comercio_hc_min$hc_cat2)) {
  rlang::abort("Existen productos de salud sin clasificación hc_cat2.")
}

## Fx para guardar .rds ----
output_dir <- "data/dashboard/"
save_dashboard_rds <- function(object, file_name, output_dir = "data/dashboard/") {
  path <- file.path(output_dir, file_name)
  saveRDS(object, path)
  invisible(path)
}

## Fx para imprimir validaciones simples ----
validate_dashboard_base <- function(
  data,
  base_name,
  value_var = NULL,
  key = NULL,
  share_group = NULL,
  share_var = NULL,
  tolerance = 1e-8
) {
  if (nrow(data) == 0) {
    rlang::abort(glue::glue("La base '{base_name}' no contiene observaciones."))
  }

  if (anyDuplicated(names(data)) > 0) {
    rlang::abort(glue::glue("La base '{base_name}' contiene columnas duplicadas."))
  }

  if (!is.null(key)) {
    missing_key <- setdiff(key, names(data))
    if (length(missing_key) > 0) {
      rlang::abort(glue::glue(
        "La base '{base_name}' no contiene la clave completa: {paste(missing_key, collapse = ', ')}"
      ))
    }
    if (anyDuplicated(data[key]) > 0) {
      rlang::abort(glue::glue("La clave de '{base_name}' no es única."))
    }
  }

  if ("hc_cat2" %in% names(data)) {
    invalid_categories <- setdiff(unique(data$hc_cat2), hc_cat2_levels)
    if (length(invalid_categories) > 0) {
      rlang::abort(glue::glue(
        "La base '{base_name}' contiene hc_cat2 no válidas: {paste(invalid_categories, collapse = ', ')}"
      ))
    }
  }

  region_columns <- intersect(
    c("exp_region", "imp_region", "partner_region"),
    names(data)
  )
  region_columns_with_na <- purrr::keep(region_columns, ~ anyNA(data[[.x]]))
  if (length(region_columns_with_na) > 0) {
    rlang::abort(glue::glue(
      "La base '{base_name}' contiene NA en columnas regionales: {paste(region_columns_with_na, collapse = ', ')}"
    ))
  }

  invalid_regions <- purrr::map(region_columns, ~ setdiff(unique(data[[.x]]), region_levels)) |>
    unlist() |>
    unique()
  if (length(invalid_regions) > 0) {
    rlang::abort(glue::glue(
      "La base '{base_name}' contiene regiones no válidas: {paste(invalid_regions, collapse = ', ')}"
    ))
  }

  balance_columns <- c("exports_1000usd", "imports_1000usd", "balance_1000usd")
  if (all(balance_columns %in% names(data))) {
    balance_error <- abs(
      data$balance_1000usd -
        (data$exports_1000usd - data$imports_1000usd)
    )
    if (any(balance_error > tolerance, na.rm = TRUE)) {
      rlang::abort(glue::glue("El balance comercial no cuadra en '{base_name}'."))
    }
  }

  share_columns <- intersect(
    c("share_exports_value", "share_flow_value"),
    names(data)
  )

  invalid_shares <- purrr::some(
    share_columns,
    ~ any(data[[.x]] < 0 | data[[.x]] > 1, na.rm = TRUE)
  )
  if (invalid_shares) {
    rlang::abort(glue::glue("La base '{base_name}' contiene participaciones fuera de [0, 1]."))
  }

  if (!is.null(share_group) && !is.null(share_var)) {
    share_totals <- data |>
      group_by(across(all_of(share_group))) |>
      summarise(share_total = sum(.data[[share_var]], na.rm = TRUE), .groups = "drop")
    if (any(abs(share_totals$share_total - 1) > tolerance)) {
      rlang::abort(glue::glue(
        "Las participaciones de '{share_var}' no suman uno en '{base_name}'."
      ))
    }
  }

  message("\n", strrep("=", 78))
  message(glue("Validación: {base_name}"))
  message(strrep("-", 78))
  message(glue("Filas: {format(nrow(data), big.mark = '.')}"))
  
  if ("year" %in% names(data)) {
    years_available <- data |>
      distinct(year) |>
      arrange(year) |>
      pull(year)
    
    message(glue(
      "Años incluidos: {paste(years_available, collapse = ', ')}"
    ))
  }
  
  if ("hc_cat2" %in% names(data)) {
    categories_available <- data |>
      distinct(hc_cat2) |>
      arrange(hc_cat2) |>
      pull(hc_cat2)
    
    message(glue(
      "Categorías hc_cat2 disponibles: {paste(categories_available, collapse = ', ')}"
    ))
  }
  
  if (!is.null(value_var) && value_var %in% names(data)) {
    total_value <- sum(data[[value_var]], na.rm = TRUE)
    
    message(glue(
      "Suma total de {value_var}: {format(round(total_value, 2), big.mark = '.')}"
    ))
  }
  
  message(strrep("=", 78), "\n")
  
  invisible(data)
}

## 4.1. Participacion regional en exp mundiales ----

# Objetivo:
#   Visualizar la participación de LAC en las exportaciones mundiales de
#   productos farma o tec sanitarias, seleccionando por categoria
#   de producto y comparando tendencias con otras regiones

exports_region_hc_cat2 <- comercio_hc_min |>
  group_by(year, exp_region, hc_cat2) |>
  summarise(
    exports_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(year, hc_cat2) |>
  mutate(
    world_exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
    share_exports_value = if_else(
      world_exports_1000usd > 0,
      exports_1000usd / world_exports_1000usd,
      NA_real_
    )
  ) |>
  ungroup() |>
  arrange(year, hc_cat2, desc(exports_1000usd))

validate_dashboard_base(
  exports_region_hc_cat2,
  base_name = "exports_region_hc_cat2",
  value_var = "exports_1000usd",
  key = c("year", "exp_region", "hc_cat2"),
  share_group = c("year", "hc_cat2"),
  share_var = "share_exports_value"
)

save_dashboard_rds(
  exports_region_hc_cat2,
  "exports_region_hc_cat2.rds"
)

## 4.1.1. Panorama regional: tendencias de exportaciones Mundo/LAC ----

# Objetivo:
#   Alimentar la hoja de panorama regional con series de exportaciones para:
#   - todos los productos PAHO;
#   - medicamentos y otras tecnologías sanitarias, distinguiendo dispositivos médicos;
#   - insumos, identificados como ingredientes farmacéuticos activos.

overview_exports_by_scope_hc <- bind_rows(
  exports_region_hc_cat2 |>
    filter(exp_region == "LAC") |>
    group_by(year, hc_cat2) |>
    summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(region_scope = "LAC"),
  exports_region_hc_cat2 |>
    group_by(year, hc_cat2) |>
    summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(region_scope = "Mundo")
) |>
  group_by(year, region_scope, hc_cat2) |>
  summarise(
    exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    product_group = case_when(
      hc_cat2 %in% medicines_other_health_categories ~ "Medicamentos y otras tecnologías sanitarias",
      hc_cat2 %in% medical_device_categories ~ "Dispositivos médicos",
      hc_cat2 == ifa_category ~ "Ingredientes farmacéuticos activos",
      TRUE ~ "Otros productos"
    )
  )

overview_exports_trends <- bind_rows(
  overview_exports_by_scope_hc |>
    group_by(year, region_scope) |>
    summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      overview_tab = "Todos los productos",
      product_group = "Todos los productos"
    ),
  overview_exports_by_scope_hc |>
    filter(hc_cat2 %in% c(medicines_other_health_categories, medical_device_categories)) |>
    group_by(year, region_scope, product_group) |>
    summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(overview_tab = "Tecnologías sanitarias"),
  overview_exports_by_scope_hc |>
    filter(hc_cat2 == ifa_category) |>
    group_by(year, region_scope, product_group) |>
    summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(overview_tab = "Insumos")
) |>
  mutate(
    exports_musd = exports_1000usd / 1000,
    line_label = paste(region_scope, product_group, sep = " - ")
  ) |>
  arrange(overview_tab, product_group, region_scope, year)

validate_dashboard_base(
  overview_exports_trends,
  base_name = "overview_exports_trends",
  value_var = "exports_1000usd",
  key = c("year", "overview_tab", "region_scope", "product_group")
)

save_dashboard_rds(
  overview_exports_trends,
  "overview_exports_trends.rds"
)

## 4.2. Exportaciones, importaciones y balance comercial en LAC ----

# Objetivo:
#   Alimentar un grafico combinado con:
#     - barras para exportaciones;
#     - barras para importaciones;
#     - linea para balance comercial neto.

# Balance comercial:
#   balance_1000usd = exports_1000usd - imports_1000usd

### 4.2.1 Exportaciones por pais LAC ----
exports_lac_country <- comercio_hc_min |>
  filter(exp_region == "LAC") |>
  mutate(
    ref_area_code = exporter,
    ref_area_name = exp_country_name,
    ref_area_type = "country",
    flow_type = "exports_1000usd"
  ) |>
  group_by(year, ref_area_code, ref_area_name, ref_area_type, hc_cat2, flow_type) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  )

### 4.2.2 Importaciones por pais LAC ----
imports_lac_country <- comercio_hc_min |>
  filter(imp_region == "LAC") |>
  mutate(
    ref_area_code = importer,
    ref_area_name = imp_country_name,
    ref_area_type = "country",
    flow_type = "imports_1000usd"
  ) |>
  group_by(year, ref_area_code, ref_area_name, ref_area_type, hc_cat2, flow_type) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  )

### 4.2.3 Exportaciones del agregado regional LAC ----
exports_lac_region <- comercio_hc_min |>
  filter(exp_region == "LAC") |>
  mutate(
    ref_area_code = "LAC",
    ref_area_name = "América Latina y el Caribe",
    ref_area_type = "region",
    flow_type = "exports_1000usd"
  ) |>
  group_by(year, ref_area_code, ref_area_name, ref_area_type, hc_cat2, flow_type) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  )

### 4.2.4 Importaciones del agregado regional LAC ----
imports_lac_region <- comercio_hc_min |>
  filter(imp_region == "LAC") |>
  mutate(
    ref_area_code = "LAC",
    ref_area_name = "América Latina y el Caribe",
    ref_area_type = "region",
    flow_type = "imports_1000usd"
  ) |>
  group_by(year, ref_area_code, ref_area_name, ref_area_type, hc_cat2, flow_type) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  )

### 4.2.5 Combinar flujos y pasar a formato wide ----
trade_balance_lac <- bind_rows(
  exports_lac_country,
  imports_lac_country,
  exports_lac_region,
  imports_lac_region
) |>
  pivot_wider(
    names_from = flow_type,
    values_from = value_1000usd,
    values_fill = list(value_1000usd = 0)
  ) |>
  mutate(
    exports_1000usd = replace_na(exports_1000usd, 0),
    imports_1000usd = replace_na(imports_1000usd, 0),
    balance_1000usd = exports_1000usd - imports_1000usd
  ) |>
  arrange(ref_area_type, ref_area_name, year, hc_cat2)

validate_dashboard_base(
  trade_balance_lac,
  base_name = "trade_balance_lac",
  value_var = "exports_1000usd",
  key = c("year", "ref_area_code", "ref_area_type", "hc_cat2")
)

save_dashboard_rds(
  trade_balance_lac,
  "trade_balance_lac.rds"
)

## 4.2.6. Panorama regional: comercio por país y categoría, último año ----

# Objetivo:
#   Alimentar gráficos de barras apiladas para importaciones y exportaciones
#   por país LAC, coloreadas por categoría hc_cat2.

overview_country_category_year <- max(trade_balance_lac$year, na.rm = TRUE)

overview_lac_country_category_trade <- trade_balance_lac |>
  filter(
    ref_area_type == "country",
    year == overview_country_category_year
  ) |>
  select(
    year,
    ref_area_code,
    ref_area_name,
    hc_cat2,
    exports_1000usd,
    imports_1000usd
  ) |>
  pivot_longer(
    cols = c(exports_1000usd, imports_1000usd),
    names_to = "flow_type",
    values_to = "value_1000usd"
  ) |>
  mutate(
    flow_type = recode(
      flow_type,
      exports_1000usd = "Exportaciones",
      imports_1000usd = "Importaciones"
    ),
    value_musd = value_1000usd / 1000
  ) |>
  arrange(flow_type, ref_area_name, hc_cat2)

validate_dashboard_base(
  overview_lac_country_category_trade,
  base_name = "overview_lac_country_category_trade",
  value_var = "value_1000usd",
  key = c("year", "ref_area_code", "hc_cat2", "flow_type")
)

save_dashboard_rds(
  overview_lac_country_category_trade,
  "overview_lac_country_category_trade.rds"
)

## 4.3. Origen/destino regional del comercio de LAC en 2024 ----

# Objetivo:
#   Crear una base para dos barras:
#     - Exports: distribucion regional de los destinos de exports de LAC.
#     - Imports: distribucion regional de los orígenes de imports de LAC.

# El usuario podra seleccionar:
#   - pais LAC o agregado regional LAC;
#   - categoria hc_cat2.

### 4.3.1 Exportaciones por pais LAC, con destino regional ----
partner_exports_country_2024 <- comercio_hc_min |>
  filter(
    year == target_year_partner,
    exp_region == "LAC"
  ) |>
  mutate(
    ref_area_code = exporter,
    ref_area_name = exp_country_name,
    ref_area_type = "country",
    flow_type = "Exports",
    partner_region = imp_region,
    partner_region_longname = imp_region_longname
  ) |>
  group_by(
    year,
    ref_area_code,
    ref_area_name,
    ref_area_type,
    hc_cat2,
    flow_type,
    partner_region,
    partner_region_longname
  ) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  )

### 4.3.2 Importaciones por pais LAC, con origen regional ----
partner_imports_country_2024 <- comercio_hc_min |>
  filter(
    year == target_year_partner,
    imp_region == "LAC"
  ) |>
  mutate(
    ref_area_code = importer,
    ref_area_name = imp_country_name,
    ref_area_type = "country",
    flow_type = "Imports",
    partner_region = exp_region,
    partner_region_longname = exp_region_longname
  ) |>
  group_by(
    year,
    ref_area_code,
    ref_area_name,
    ref_area_type,
    hc_cat2,
    flow_type,
    partner_region,
    partner_region_longname
  ) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  )

### 4.3.3 Exportaciones del agregado LAC, con destino regional ----
partner_exports_region_2024 <- comercio_hc_min |>
  filter(
    year == target_year_partner,
    exp_region == "LAC"
  ) |>
  mutate(
    ref_area_code = "LAC",
    ref_area_name = "América Latina y el Caribe",
    ref_area_type = "region",
    flow_type = "Exports",
    partner_region = imp_region,
    partner_region_longname = imp_region_longname
  ) |>
  group_by(
    year,
    ref_area_code,
    ref_area_name,
    ref_area_type,
    hc_cat2,
    flow_type,
    partner_region,
    partner_region_longname
  ) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  )

### 4.3.4 Importaciones del agregado LAC, con origen regional ----
partner_imports_region_2024 <- comercio_hc_min |>
  filter(
    year == target_year_partner,
    imp_region == "LAC"
  ) |>
  mutate(
    ref_area_code = "LAC",
    ref_area_name = "América Latina y el Caribe",
    ref_area_type = "region",
    flow_type = "Imports",
    partner_region = exp_region,
    partner_region_longname = exp_region_longname
  ) |>
  group_by(
    year,
    ref_area_code,
    ref_area_name,
    ref_area_type,
    hc_cat2,
    flow_type,
    partner_region,
    partner_region_longname
  ) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  )

### 4.3.5 Combinar y calcular porcentajes dentro de cada barra ----
partner_region_lac_2024 <- bind_rows(
  partner_exports_country_2024,
  partner_imports_country_2024,
  partner_exports_region_2024,
  partner_imports_region_2024
) |>
  group_by(
    year,
    ref_area_code,
    ref_area_name,
    ref_area_type,
    hc_cat2,
    flow_type
  ) |>
  mutate(
    total_flow_1000usd = sum(value_1000usd, na.rm = TRUE),
    share_flow_value = if_else(
      total_flow_1000usd > 0,
      value_1000usd / total_flow_1000usd,
      NA_real_
    )
  ) |>
  ungroup() |>
  arrange(ref_area_type, ref_area_name, hc_cat2, flow_type, desc(value_1000usd))

validate_dashboard_base(
  partner_region_lac_2024,
  base_name = "partner_region_lac_2024",
  value_var = "value_1000usd",
  key = c(
    "year", "ref_area_code", "ref_area_type", "hc_cat2",
    "flow_type", "partner_region"
  ),
  share_group = c(
    "year", "ref_area_code", "ref_area_type", "hc_cat2", "flow_type"
  ),
  share_var = "share_flow_value"
)

save_dashboard_rds(
  partner_region_lac_2024,
  "partner_region_lac_2024.rds"
)


## 4.4. Sankey de comercio intrarregional LAC ----

# Objetivo:
#   Construir una base para diagramas Sankey donde exportador e importador pertenecen a LAC.
#   El grosor del flujo se define por value_1000usd.

sankey_intra_lac <- comercio_hc_min |>
  filter(
    exp_region == "LAC",
    imp_region == "LAC",
    year %in% sankey_years
  ) |>
  mutate(
    source = exporter,
    source_name = exp_country_name,
    target = importer,
    target_name = imp_country_name
  ) |>
  group_by(
    year,
    hc_cat2,
    source,
    source_name,
    target,
    target_name
  ) |>
  summarise(
    value_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year, hc_cat2, source_name, target_name)

validate_dashboard_base(
  sankey_intra_lac,
  base_name = "sankey_intra_lac",
  value_var = "value_1000usd",
  key = c("year", "hc_cat2", "source", "target")
)

save_dashboard_rds(
  sankey_intra_lac,
  "sankey_intra_lac.rds"
)

## 4.5. Base auxiliar: productos exportados por país de origen, último año ----

# Objetivo:
#   Alimentar tablas de productos HS6 exportados por país LAC de origen para
#   todas las categorías hc_cat2 del dashboard.
#
# Unidad:
#   - year x hc_cat2 x product x país exportador
#   - exports_1000usd: miles de USD
#
# Nota:
#   Se usa comercio_hc_world y no comercio_hc_min porque comercio_hc_min
#   elimina description y description_short.
#   El RCA Balassa se calcula aparte con el universo completo de productos BACI
#   y luego se une a esta base PAHO por year x exporter x product.

product_exports_year <- max(comercio_hc_world$year, na.rm = TRUE)

product_country_indicators_2024 <- calculate_product_country_rca(
  baci_data = baci,
  indicator_year = product_exports_year,
  countries_exp = paises_exp
) |>
  mutate(
    exp_region = replace_na(as.character(exp_region), unclassified_region),
    exp_region_longname = replace_na(as.character(exp_region_longname), unclassified_region),
    exp_country_name = replace_na(as.character(exp_country_name), unclassified_region)
  )

validate_dashboard_base(
  product_country_indicators_2024,
  base_name = "product_country_indicators_2024",
  value_var = "exports_1000usd",
  key = c("year", "exporter", "product")
)

save_dashboard_rds(
  product_country_indicators_2024,
  "product_country_indicators_2024.rds"
)

product_exports_lac_2024_by_country <- comercio_hc_world |>
  replace_missing_trade_labels() |>
  mutate(
    year = as.integer(year),
    product = as.character(product),
    hc_cat2 = as.character(hc_cat2),
    exp_region = as.character(exp_region),
    exporter = as.character(exporter)
  ) |>
  filter(
    year == product_exports_year,
    exp_region == "LAC"
  ) |>
  group_by(
    year,
    hc_cat2,
    product,
    description,
    description_short,
    exporter,
    exp_country_name
  ) |>
  summarise(
    exports_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(year, hc_cat2, product, description, description_short) |>
  mutate(
    product_exports_1000usd = sum(exports_1000usd, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    exports_musd = exports_1000usd / 1000,
    product_exports_musd = product_exports_1000usd / 1000
  ) |>
  left_join(
    product_country_indicators_2024 |>
      select(
        year,
        exporter,
        product,
        country_total_exports_1000usd,
        world_product_exports_1000usd,
        world_total_exports_1000usd,
        country_product_share,
        world_product_share,
        rca_balassa
      ),
    by = c("year", "exporter", "product")
  ) |>
  arrange(hc_cat2, desc(product_exports_1000usd), desc(exports_1000usd))

validate_dashboard_base(
  product_exports_lac_2024_by_country,
  base_name = "product_exports_lac_2024_by_country",
  value_var = "exports_1000usd",
  key = c("year", "hc_cat2", "product", "exporter")
)

save_dashboard_rds(
  product_exports_lac_2024_by_country,
  "product_exports_lac_2024_by_country.rds"
)

## 4.6. Base auxiliar: exportaciones LAC de dispositivos médicos, 2024 ----

# Objetivo:
#   Investigar qué productos HS6 explican los resultados observados para
#   el conjunto de categorías de dispositivos médicos en las exportaciones
#   de LAC durante 2024.
#
# Unidad:
#   - product: código HS07 a 6 dígitos
#   - value_1000usd: miles de USD
#
# Nota:
#   Se usa comercio_hc_world y no comercio_hc_min porque comercio_hc_min
#   elimina description y description_short.

medical_devices_lac_exports_2024_product <- comercio_hc_world |>
  replace_missing_trade_labels() |>
  mutate(
    year = as.integer(year),
    product = as.character(product),
    hc_cat2 = as.character(hc_cat2),
    exp_region = as.character(exp_region),
    exporter = as.character(exporter),
    importer = as.character(importer)
  ) |>
  filter(
    year == 2024,
    exp_region == "LAC",
    hc_cat2 %in% medical_device_categories
  ) |>
  group_by(
    year,
    product,
    description,
    description_short,
    hc_cat2,
    exporter,
    exp_country_name,
    importer,
    imp_country_name,
    imp_region
  ) |>
  summarise(
    exports_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(product, description, description_short, hc_cat2) |>
  mutate(
    product_exports_1000usd = sum(exports_1000usd, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    exports_musd = exports_1000usd / 1000,
    product_exports_musd = product_exports_1000usd / 1000
  ) |>
  arrange(desc(product_exports_1000usd), desc(exports_1000usd))

validate_dashboard_base(
  medical_devices_lac_exports_2024_product,
  base_name = "medical_devices_lac_exports_2024_product",
  value_var = "exports_1000usd"
)

save_dashboard_rds(
  medical_devices_lac_exports_2024_product,
  "medical_devices_lac_exports_2024_product.rds"
)

# Revisar top 20
revisar_prods_meddev <- medical_devices_lac_exports_2024_product |>
  distinct(
    product,
    description,
    description_short,
    product_exports_1000usd,
    product_exports_musd
  ) |>
  arrange(desc(product_exports_1000usd)) |>
  slice_head(n = 20)

# Revisar top exportadores
revisar_exp_meddev <- medical_devices_lac_exports_2024_product |>
  group_by(product, description_short, exp_country_name) |>
  summarise(
    exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
    exports_musd = exports_1000usd / 1000,
    .groups = "drop"
  ) |>
  arrange(product, desc(exports_1000usd))

## 4.7. Base auxiliar: importaciones LAC de medicamentos, 2024 ----

# Objetivo:
#   Investigar qué productos HS6 explican los resultados observados para
#   "Medicamentos" en las importaciones de LAC durante 2024.
#
# Unidad:
#   - product: código HS07 a 6 dígitos
#   - value_1000usd: miles de USD
#
# Nota:
#   Se usa comercio_hc_world y no comercio_hc_min porque comercio_hc_min
#   elimina description y description_short.

medicines_lac_imports_2024_product <- comercio_hc_world |>
  replace_missing_trade_labels() |>
  mutate(
    year = as.integer(year),
    product = as.character(product),
    hc_cat2 = as.character(hc_cat2),
    exp_region = as.character(exp_region),
    imp_region = as.character(imp_region),
    exporter = as.character(exporter),
    importer = as.character(importer)
  ) |>
  filter(
    year == 2024,
    imp_region == "LAC",
    hc_cat2 == "Medicamentos"
  ) |>
  group_by(
    year,
    product,
    description,
    description_short,
    hc_cat2,
    exporter,
    exp_country_name,
    exp_region,
    importer,
    imp_country_name
  ) |>
  summarise(
    imports_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(product, description, description_short, hc_cat2) |>
  mutate(
    product_imports_1000usd = sum(imports_1000usd, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    imports_musd = imports_1000usd / 1000,
    product_imports_musd = product_imports_1000usd / 1000
  ) |>
  arrange(desc(product_imports_1000usd), desc(imports_1000usd))

validate_dashboard_base(
  medicines_lac_imports_2024_product,
  base_name = "medicines_lac_imports_2024_product",
  value_var = "imports_1000usd"
)

save_dashboard_rds(
  medicines_lac_imports_2024_product,
  "medicines_lac_imports_2024_product.rds"
)

rev_prods <- medicines_lac_imports_2024_product |>
  distinct(
    product,
    description,
    description_short,
    product_imports_1000usd,
    product_imports_musd
  ) |>
  arrange(desc(product_imports_1000usd)) |>
  slice_head(n = 20)

rev_paises <- medicines_lac_imports_2024_product |>
  group_by(product, description_short, imp_country_name) |>
  summarise(
    imports_1000usd = sum(imports_1000usd, na.rm = TRUE),
    imports_musd = imports_1000usd / 1000,
    .groups = "drop"
  ) |>
  arrange(product, desc(imports_1000usd))


## 4.8. Base auxiliar: exportaciones LAC de IFAs, 2024 ----

# Objetivo:
#   Investigar qué productos HS6 explican los resultados observados para
#   "Ingredientes farmacéuticos activos" en las exportaciones de LAC durante 2024.
#
# Unidad:
#   - product: código HS07 a 6 dígitos
#   - value_1000usd: miles de USD
#
# Nota:
#   Se usa comercio_hc_world y no comercio_hc_min porque comercio_hc_min
#   elimina description y description_short.

ifas_lac_exports_2024_product <- comercio_hc_world |>
  replace_missing_trade_labels() |>
  mutate(
    year = as.integer(year),
    product = as.character(product),
    hc_cat2 = as.character(hc_cat2),
    exp_region = as.character(exp_region),
    exporter = as.character(exporter),
    importer = as.character(importer)
  ) |>
  filter(
    year == 2024,
    exp_region == "LAC",
    hc_cat2 == ifa_category
  ) |>
  group_by(
    year,
    product,
    description,
    description_short,
    hc_cat2,
    exporter,
    exp_country_name,
    importer,
    imp_country_name,
    imp_region
  ) |>
  summarise(
    exports_1000usd = sum(value_1000usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(product, description, description_short, hc_cat2) |>
  mutate(
    product_exports_1000usd = sum(exports_1000usd, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    exports_musd = exports_1000usd / 1000,
    product_exports_musd = product_exports_1000usd / 1000
  ) |>
  arrange(desc(product_exports_1000usd), desc(exports_1000usd))

validate_dashboard_base(
  ifas_lac_exports_2024_product,
  base_name = "ifas_lac_exports_2024_product",
  value_var = "exports_1000usd"
)

save_dashboard_rds(
  ifas_lac_exports_2024_product,
  "ifas_lac_exports_2024_product.rds"
)

# Revisar top 20
revisar_prods_ifas <- ifas_lac_exports_2024_product |>
  distinct(
    product,
    description,
    description_short,
    product_exports_1000usd,
    product_exports_musd
  ) |>
  arrange(desc(product_exports_1000usd)) |>
  slice_head(n = 20)

# Revisar top exportadores
revisar_exp_ifas <- ifas_lac_exports_2024_product |>
  group_by(product, description_short, exp_country_name) |>
  summarise(
    exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
    exports_musd = exports_1000usd / 1000,
    .groups = "drop"
  ) |>
  arrange(product, desc(exports_1000usd))


# 5. BASES LIVIANAS SOBRE RCA ----



# 6. RESUMEN DE ARCHIVOS CREADOS ----

created_files <- tibble(
  file = list.files(
    output_dir,
    pattern = "\\.rds$",
    full.names = TRUE
  )
) |>
  mutate(
    file_name = basename(file),
    size_mb = round(file.info(file)$size / 1024^2, 3)
  ) |>
  arrange(file_name)

message("\nArchivos .rds disponibles en la carpeta de salida:")
print(created_files)

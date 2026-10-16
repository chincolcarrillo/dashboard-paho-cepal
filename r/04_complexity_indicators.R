#  ----------- INDICADORES DE COMPLEJIDAD PARA DASHBOARD -----------

# 0. SET-UP ----

# cargar paquetes
if (!requireNamespace("pacman", quietly = TRUE)) {
  stop("Instale el paquete 'pacman' antes de ejecutar este script.", call. = FALSE)
}
library(pacman)
p_load(tidyverse,
       readxl,
       glue,
       janitor)

source("r/00_config.R", encoding = "UTF-8")

# 1. CARGAR DATOS ----
pci <- read_csv(
  "data/Data-PCI.csv",
  col_types = cols(`HS6 ID` = col_character(), .default = col_guess())
)
  
productos <-  read_excel("data/rev_paho_2026.xlsx", 
                         sheet = "exportable")

product_balance_path <- file.path(
  internal_data_dir,
  "product_trade_balance_lac_2024.rds"
)

if (!file.exists(product_balance_path)) {
  rlang::abort(glue::glue(
    "No se encontró '{product_balance_path}'. Ejecute primero r/01_clean_baci.R."
  ))
}

product_trade_balance <- readRDS(product_balance_path)

# 2. SELECCIONAR PRODUCTOS AREA SALUD -----
pci <- pci |>
  select(code=`HS6 ID`, 
         pci_2024=`2024`, pci_2023=`2023`, pci_2022=`2022`) |>
  mutate(code = as.character(code))

productos <- productos |>
  mutate(
    code = as.character(code),
    hc_cat2 = as.character(hc_cat2)
  ) |>
  filter(hc == 1, !is.na(hc_cat2))

pci_tec_sanitaria <- pci |>
  right_join(productos, by = "code") |>
  left_join(
    product_trade_balance |>
      select(
        year,
        product,
        hc_cat2,
        exports_1000usd,
        imports_1000usd,
        balance_1000usd
      ),
    by = c("code" = "product", "hc_cat2" = "hc_cat2")
  ) |>
  mutate(
    product_group = case_when(
      hc_cat2 == ifa_category ~ "IFAs",
      hc_cat2 %in% medicines_other_health_categories ~
        "Medicamentos, vacunas y otros",
      hc_cat2 %in% medical_device_categories ~ "Dispositivos médicos",
      TRUE ~ NA_character_
    ),
    product_group = factor(
      product_group,
      levels = c(
        "Medicamentos, vacunas y otros",
        "Dispositivos médicos",
        "IFAs"
      )
    )
  ) |>
  arrange(product_group, hc_cat2, code)

dir.create(internal_data_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(
  pci_tec_sanitaria,
  file.path(internal_data_dir, "pci_tec_sanitaria.rds")
)




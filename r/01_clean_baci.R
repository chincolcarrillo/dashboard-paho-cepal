#  ----------- PROCESAMIENTO CEPII-BACI 17 PARA PRODUCTOS DEL AREA SALUD -----------

# 0. SET-UP ----

# cargar paquetes
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,
       janitor,
       readxl,
       writexl,
       fs,
       duckdb,
       arrow)

# 1. CARGAR DATA BACI ----

# ZIP disponible en "https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS17_V202601.zip"

csv_files <- dir_ls("data/raw", regexp = "BACI_HS17_Y.*\\.csv$")
con <- dbConnect(duckdb(), "data/raw/baci.duckdb")

for (f in csv_files) {
  year <- stringr::str_extract(path_file(f), "\\d{4}")
  out <- glue::glue("data/raw/parquet/year={year}/data.parquet")
  dir_create(path_dir(out))
  
  dbExecute(con, glue::glue("
    COPY (
      SELECT
        t::INTEGER AS year,
        k::VARCHAR AS product,
        i::INTEGER AS exporter,
        j::INTEGER AS importer,
        v::DOUBLE AS value_1000usd,
        q::DOUBLE AS quantity_tons
      FROM read_csv_auto('{f}', HEADER = TRUE)
    )
    TO '{out}' (FORMAT PARQUET);
  "))
}

dbDisconnect(con)

baci <- open_dataset("data/raw/parquet")


# 02. IDENTIFICAR PAISES Y REGIONES -----
paises <-  read_csv("data/raw/country_codes_V202601.csv")
regiones <-  read_excel("data/world-bank-country-class.xlsx") |> # World Bank Country and Lending Groups
  clean_names() 
paises <- paises |>
  left_join(regiones, by = c("country_iso3" = "code")) |>
  mutate(country_code = as.integer(country_code),
         region_longname = region,
         region = case_when(region == "Middle East, North Africa, Afghanistan & Pakistan" ~ "MENA",
                            region == "Latin America & Caribbean" ~ "LAC",
                            region == "East Asia & Pacific" ~ "East Asia & Pacific",
                            region == "Europe & Central Asia" ~ " Europe & Central Asia",
                            region == "North America" ~ "North America",
                            region == "South Asia" ~ "South Asia",
                            region == "Sub-Saharan Africa" ~ "Sub-Saharan Africa")) |>
  select(country_code, country_name, region, region_longname)

# paises_sin_region <- paises |> filter(is.na(region))
# 27 paises sin WB region
# incluye “Asia, not elsewhere specified” (code 490), proxy de Taiwan
paises[131, 3:4] <- "East Asia & Pacific"

# Dejar columnas listas para unir con base BACI
paises_imp <- paises |> rename_with(~ paste0("imp_", .), -country_code)
paises_exp <- paises |> rename_with(~ paste0("exp_", .), -country_code)


# 03. SELECCIONAR PRODUCTOS AREA SALUD -----
productos <-  read_excel("data/rev_paho_2026.xlsx", sheet = "exportable") # Categorias acordadas con MEPP (experta PAHO)
concordancias <- read_excel("data/WITS_Concordance_H5_to_H3.xlsx") |> # Tabla de concordancias de la World Integrated Trade Solution (WITS)
  clean_names()
productos <- productos |>
  left_join(concordancias, by = c("code" = "hs_2007_product_code"))
prods_sin_HS2017 <- productos |> filter(is.na(hs_2017_product_code))

concordancias <- concordancias |>
  mutate(hs_2017_product_code = as.character(hs_2017_product_code))

productos_iguales <- product_codes_HS17_V202601 |>
  inner_join(concordancias, by = c("code" = "hs_2017_product_code"))

productos_diff <- productos |>
  left_join(product_codes_HS17_V202601, by = "code")
prods_sin_BACI <- productos_diff |> filter(is.na(description.y))

productos <- productos |> 
  mutate(code = as.character(code))

product_codes_HS17_V202601 <- read_csv("data/raw/product_codes_HS17_V202601.csv")


comercio_hc_world <- baci |> 
  mutate(product = as.integer(product)) |>
  inner_join(productos, by = c("product" = "code")) |>
  left_join(paises_imp, by = c("importer" = "country_code")) |>
  left_join(paises_exp, by = c("exporter" = "country_code")) |>
  collect()


# 03. RECODIFICAR -----
comercio_hc_world2 <- comercio_hc_world |>
  mutate(
    imp_lac = case_when(imp_sub_region_name == "Latin America and the Caribbean" ~ "LAC",
                        imp_sub_region_name == "Northern America" ~ "Norteamérica",
                        imp_region_name == "Europe" ~ "Europa",
                        imp_region_name == "Oceania" ~ "Oceanía",
                        imp_region_name == "Africa" ~ "África",
                        imp_region_name == "Asia" ~ "Asia"),
    exp_lac = case_when(exp_sub_region_name == "Latin America and the Caribbean" ~ "LAC",
                        exp_sub_region_name == "Northern America" ~ "Norteamérica",
                        exp_region_name == "Europe" ~ "Europa",
                        exp_region_name == "Oceania" ~ "Oceanía",
                        exp_region_name == "Africa" ~ "África",
                        exp_region_name == "Asia" ~ "Asia"),
  )

comercio_hc_lac <- comercio_hc_world |>
  filter(imp_sub_region_name == "Latin America and the Caribbean" | exp_sub_region_name == "Latin America and the Caribbean")

# 04. GUARDAR -----
saveRDS(comercio_hc_lac, file = "data/comercio_hc_lac.rds")

#rm(paises, paises_exp, paises_imp, productos, con, baci, csv_files, f, out, year)

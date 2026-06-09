#  ----------- CONVERT PRODUCT CODES HS2007 to HS2017 -----------

# 0. SET-UP ----

# cargar paquetes
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,
       janitor,
       readxl, 
       writexl)

# Fx auxiliar: normalizacion de HS (no he tenido problemas, pero por si acaso)
normalizar_hs <- function(x) {
  x |>
    as.character() |>  # como texto
    stringr::str_trim() |>
    stringr::str_remove("\\.0$") |> # elimina ".0" si viene de numeric/double
    stringr::str_replace_all("[^0-9]", "") |> # elimina todo lo que no sean numeros (por si viene con puntos)
    stringr::str_pad(width = 6, side = "left", pad = "0") |> # si pierde ceros iniciales, rellena hasta llegar a 6 digitos
    dplyr::na_if("0000NA") |> # si hay NAs
    dplyr::na_if("000000")    # si hay vacios
}

# 01. CARGAR DATOS ----
## 01.1. Descripcion productos en HS2017, en base de BACI ----
prods_baci <- read_csv("data/raw/product_codes_HS17_V202601.csv") |>
  clean_names() |>
  mutate(
    code = normalizar_hs(code)
  )

## 01.2. Categorias acordadas con MEPP (experta PAHO) de acuerdo a HS07 ----
prods_rev_paho <-  read_excel("data/rev_paho_2026.xlsx", 
                         sheet = "exportable") |>
  clean_names() |>
  mutate(
    code = normalizar_hs(code)
  ) |>
  select(code_hs07=code, 
         description_hs07=description,
         description_short_hs07=description_short,
         starts_with("hc"))

## 01.3. Tablas de conversion y correlacion de UN Stats ----
# https://unstats.un.org/unsd/classifications/Econ
conversiones <- read_excel("data/HS2017toHS2007ConversionAndCorrelationTables.xlsx") |> 
  clean_names() |>
  mutate(
    code_hs07 = normalizar_hs(to_hs_2007),
    conversion_hs17 = normalizar_hs(from_hs_2017),
  ) |>
  select(-to_hs_2007, -from_hs_2017)

correlaciones <- read_excel("data/HS2017toHS2007ConversionAndCorrelationTables.xlsx",
                           sheet = "Correlation HS17-HS07",
                           skip = 1) |> 
  clean_names() |>
  mutate(
    hs_2007 = normalizar_hs(hs_2007),
    hs_2017 = normalizar_hs(hs_2017)
    ) |>
  left_join(prods_baci, by = c("hs_2017" = "code")) |> # pegarle descripciones
  select(code_hs07=hs_2007, 
         relacion_tipo=relationship, 
         correlacion_hs17=hs_2017, 
         description_hs17=description)


# 02. CONSTRUIR BASE PARA REVISION PAHO ----

base_revision <- prods_rev_paho |>
  # Mantiene todos los productos hc == 1 definidos por MEPP en HS2007
  left_join(conversiones, by = "code_hs07") |>
  # Agrega todos los codigos HS2017 correlacionados y su tipo de relacion
  left_join(correlaciones, by = "code_hs07") |>
  mutate(
    conversion_tipo = case_when(
      relacion_tipo == "1:1" ~ "Directa",
      relacion_tipo == "n:1" ~ "Directa",
      relacion_tipo %in% c("1:n", "n:n") & code_hs07 == conversion_hs17 ~ "Código retenido",
      relacion_tipo %in% c("1:n", "n:n") & str_detect(code_hs07, "(90|99)$") ~ "Others retenido",
      is.na(conversion_hs17) ~ "Sin conversión",
      TRUE ~ "Otra conversión"
    ),
    estatus_asignacion = case_when(
      is.na(conversion_hs17) ~ "Revisar (sin conversión)",
      relacion_tipo %in% c("1:n", "n:n")  ~ "Revisar",
      TRUE ~ "Incluir producto"
    )
  ) |>
  select(
    code_hs07,
    description_hs07,
    description_short_hs07,
    conversion_hs17,
    conversion_tipo,
    correlacion_hs17,
    relacion_tipo,
    description_hs17,
    hc,
    hc_cat1,
    hc_cat2,
    estatus_asignacion
  ) 

table(base_revision$estatus_asignacion)
# habria que validar 143 productos  

# interesante: 
# Vacunas se separa en varias categorias
# Se estaria sumando una categoria de kit diagnostico (malaria)
# Algunos principios activos se condensan en una sola categoria (alcaloides)


# 03. CONSTRUIR BASE PARA REVISION PAHO ----

write_xlsx(base_revision, "data/para_revision_HS2017.xlsx")

#  ----------- CONVERT PRODUCT CODES HS2007 to HS2017 -----------

# 0. SET-UP ----

# cargar paquetes
if (!requireNamespace("pacman", quietly = TRUE)) {
  stop("Instale el paquete 'pacman' antes de ejecutar este script.", call. = FALSE)
}
library(pacman)
p_load(tidyverse,
       janitor,
       readxl, 
       writexl,
       install = FALSE)

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


# 03. EXPORTAR BASE PARA REVISION PAHO ----

write_xlsx(base_revision, "data/para_revision_HS2017.xlsx")


# 04. CREAR BASE EXTRA PARA SANKEY ----

archivo_hs <- "data/HSCodeandDescription.xlsx"

# Hojas disponibles en el archivo
hs_versions <- c("HS07", "HS12", "HS17", "HS22")

# 2. Función para leer y estandarizar cada hoja ---------------------------------

read_hs_sheet <- function(sheet_name) {
  
  read_excel(
    path = archivo_hs,
    sheet = sheet_name,
    col_types = "text"
  ) |>
    clean_names() |>
    transmute(
      version = str_to_lower(sheet_name),
      
      # Se conserva como texto para evitar perder ceros iniciales.
      code = str_trim(as.character(code)),
      description = str_squish(as.character(description)),
      parent_code = str_trim(as.character(parent_code)),
      
      # Estas variables deberían ser estables entre versiones.
      level = as.integer(level),
      is_basic_level = as.integer(is_basic_level)
    ) |>
    filter(!is.na(code), code != "") |>
    distinct(version, code, .keep_all = TRUE)
}

# 3. Leer todas las versiones en formato largo ----------------------------------

hs_long <- map_dfr(hs_versions, read_hs_sheet)

# 4. Crear base ancha para consulta rápida --------------------------------------

hs_lookup <- hs_long |>
  mutate(
    available = 1L
  ) |>
  select(
    code,
    version,
    level,
    description,
    parent_code,
    is_basic_level,
    available
  ) |>
  pivot_wider(
    names_from = version,
    values_from = c(description, available, parent_code, level, is_basic_level),
    names_glue = "{.value}_{version}"
  ) |>
  mutate(
    across(
      starts_with("available_"),
      ~ replace_na(.x, 0L)
    ),
    parent_code = coalesce(
      parent_code_hs07,
      parent_code_hs12,
      parent_code_hs17,
      parent_code_hs22
    ),
    level = coalesce(
      level_hs07,
      level_hs12,
      level_hs17,
      level_hs22
    ),
    is_basic_level = coalesce(
      is_basic_level_hs07,
      is_basic_level_hs12,
      is_basic_level_hs17,
      is_basic_level_hs22
    )
  ) |>
  rowwise() |>
  mutate(
    descriptions_difieren = {
      descs <- c_across(
        c(
          description_hs07,
          description_hs12,
          description_hs17,
          description_hs22
        )
      )
      
      descs <- descs[!is.na(descs)]
      descs <- unique(descs)
      
      length(descs) > 1
    }
  ) |>
  ungroup() |>
  filter(is_basic_level == 1) |>
  select(
    code,
    parent_code,
    level,
    available_hs07,
    available_hs12,
    available_hs17,
    available_hs22,
    description_hs07,
    description_hs12,
    description_hs17,
    description_hs22,
    descriptions_difieren,
    everything(),
    -starts_with("parent_code_hs"),
    -starts_with("level_hs"),
    -starts_with("is_basic_level")
  ) |>
  arrange(code)

granpa_codes <- hs_long |>
  filter(level == 2 & version == "hs22") |>
  select(
    code,
    parent_description=description
  )

parent_codes <- hs_long |>
  filter(level == 4 & version == "hs22") |>
  select(
    code,
    description,
    parent_code
  ) |>
  left_join(granpa_codes, by = c("parent_code" = "code")) |>
  filter(parent_code %in% c("29",
                            "30",
                            "34",
                            "38",
                            "39",
                            "40",
                            "61",
                            "84"))


hs_lookup <- hs_lookup |>
  inner_join(parent_codes, join_by(parent_code == code))

write_xlsx(hs_lookup, "data/para_revision_versiones_HS.xlsx")
write_xlsx(parent_codes, "data/parent_codes.xlsx")


# REV: -91:99 SI: 90, 87, 84, 61, 40, 39, 38, 34, 30, 29

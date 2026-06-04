#  ----------- CONVERT PRODUCT CODES HS2007 to HS2017 -----------

# 0. SET-UP ----

# cargar paquetes
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,
       janitor,
       readxl)

# 01. CARGAR DATOS ----
# Descripcion productos en HS2017, en base de BACI
prods_baci <- read_csv("data/raw/product_codes_HS17_V202601.csv")

# Categorias acordadas con MEPP (experta PAHO) de acuerdo a HS07
productos <-  read_excel("data/rev_paho_2026.xlsx", 
                         sheet = "exportable") 
productos <- productos |>
  mutate(code = as.character(code))

# Tabla de conversion de UN Stats https://unstats.un.org/unsd/classifications/Econ
conversiones <- read_excel("data/HS2017toHS2007ConversionAndCorrelationTables.xlsx") |> 
  clean_names()

# Tabla de correlaciones, incluye tipo de relacion (1:1, 1:n, n:1, n:n)
correlaciones <- read_excel("data/HS2017toHS2007ConversionAndCorrelationTables.xlsx",
                           sheet = "Correlation HS17-HS07",
                           skip = 1) |> 
  clean_names()

# 02. REVISAR CORRELACIONES ----

# Incluir descripciones por producto
correlaciones <- correlaciones |>
  left_join(prods_baci, by = c("hs_2017" = "code")) |>
  select(hs_2007, relationship, code_hs17=hs_2017, desc_hs17=description)

paho_hs2017 <- productos |>
  left_join(correlaciones, by = c("code" = "hs_2007")) |>
  mutate(
    paho_match = !is.na(code_hs17),
    assignment_confidence = case_when(
      relationship %in% c("1:1", "n:1") ~ "alta",
      relationship %in% c("1:n", "n:n") ~ "revisar", # para estos casos UNSD selecciona el “mejor” codigo anterior (“other”, código completo, participación de comercio >75% o ajuste manual)
      TRUE ~ "sin información"
    ),
    assignment_status = case_when(
      paho_match & assignment_confidence == "alta" ~ "incluir",
      paho_match & assignment_confidence == "revisar" ~ "validar",
      TRUE ~ "excluir"
    )
  )

table(paho_hs2017$assignment_status)
# habria que validar 41 productos  

# interesante: 
# Vacunas se separa en varias categorias
# Se estaria sumando una categoria de kit diagnostico (malaria)
# Algunos principios activos se condensan en una sola categoria (alcaloides)




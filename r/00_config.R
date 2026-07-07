# Configuración compartida del pipeline y del dashboard.

hc_cat2_levels <- c(
  "Células humanas, tejidos y productos médicos de terapia avanzada",
  "Diagnósticos in vitro",
  "Dispositivos médicos",
  "Hemoderivados, antisueros y productos inmunobiológicos",
  "Ingredientes farmacéuticos activos",
  "Medicamentos",
  "Vacunas (humanas)"
)

region_levels <- c(
  "LAC",
  "North America",
  "Europe",
  "Central Asia",
  "East Asia & Pacific",
  "South Asia",
  "MENA",
  "Sub-Saharan Africa",
  "No clasificada"
)

region_long_labels <- c(
  "LAC" = "Latin America & Caribbean",
  "North America" = "North America",
  "Europe" = "Europe",
  "Central Asia" = "Central Asia",
  "East Asia & Pacific" = "East Asia & Pacific",
  "South Asia" = "South Asia",
  "MENA" = "Middle East, North Africa, Afghanistan & Pakistan",
  "Sub-Saharan Africa" = "Sub-Saharan Africa",
  "No clasificada" = "No clasificada"
)

region_colors <- c(
  "Latin America & Caribbean" = "#1B9E77",
  "North America" = "#7570B3",
  "Europe" = "#D95F02",
  "Central Asia" = "#E6AB02",
  "East Asia & Pacific" = "#E7298A",
  "South Asia" = "#66A61E",
  "Middle East, North Africa, Afghanistan & Pakistan" = "#A6761D",
  "Sub-Saharan Africa" = "#1F78B4",
  "No clasificada" = "#999999"
)

unclassified_region <- "No clasificada"

dashboard_data_dir <- "data/dashboard"
sankey_years <- c(2018L, 2021L, 2024L)
target_year_partner <- 2024L

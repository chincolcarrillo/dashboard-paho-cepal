# Configuración compartida del pipeline y del dashboard.

hc_cat2_levels <- c(
  "Ingredientes farmacéuticos activos",
  "Células, tejidos y otras sustancias de origen humano o animal para uso terapéutico",
  "Medicamentos",
  "Sangre y productos derivados, inmunoglobulinas y antisueros",
  "Vacunas (humanas)",
  "Cuidado de heridas y dispositivos de protección",
  "Dispositivos cardiovasculares",
  "Dispositivos de diagnóstico in vitro y de laboratorio",
  "Dispositivos de diagnóstico por imagen",
  "Dispositivos de esterilización y desinfección",
  "Dispositivos odontológicos",
  "Dispositivos para rehabilitación y asistencia",
  "Dispositivos quirúrgicos e invasivos",
  "Equipos de diagnóstico y monitoreo",
  "Equipos de laboratorio",
  "Equipos terapéuticos",
  "Insumos médicos y suministros para la atención de pacientes",
  "Mobiliario médico y elementos de soporte"
)

ifa_category <- "Ingredientes farmacéuticos activos"

medicines_other_health_categories <- c(
  "Células, tejidos y otras sustancias de origen humano o animal para uso terapéutico",
  "Medicamentos",
  "Sangre y productos derivados, inmunoglobulinas y antisueros",
  "Vacunas (humanas)"
)

medical_device_categories <- setdiff(
  hc_cat2_levels,
  c(ifa_category, medicines_other_health_categories)
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

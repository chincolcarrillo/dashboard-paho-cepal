# Configuración compartida del pipeline y del dashboard.

hc_cat2_levels <- c(
  "Ingredientes farmacéuticos activos",
  "Medicamentos",
  "Vacunas (humanas)",
  "Células, tejidos y otras sustancias de origen humano o animal para uso terapéutico",
  "Sangre y productos derivados, inmunoglobulinas y antisueros",
  "Dispositivos de diagnóstico in vitro y de laboratorio",
  "Cuidado de heridas y dispositivos de protección",
  "Dispositivos cardiovasculares",
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
  "Medicamentos",
  "Vacunas (humanas)",
  "Células, tejidos y otras sustancias de origen humano o animal para uso terapéutico",
  "Sangre y productos derivados, inmunoglobulinas y antisueros"
)

medical_device_categories <- setdiff(
  hc_cat2_levels,
  c(ifa_category, medicines_other_health_categories)
)

region_levels <- c(
  "LAC",
  "América del Norte",
  "Europa",
  "Asia central",
  "Asia oriental y el Pacífico",
  "Asia meridional",
  "MENA",
  "África al sur del Sahara",
  "No clasificada"
)

region_long_labels <- c(
  "LAC" = "América Latina y el Caribe",
  "América del Norte" = "América del Norte",
  "Europa" = "Europa",
  "Asia central" = "Asia central",
  "Asia oriental y el Pacífico" = "Asia oriental y el Pacífico",
  "Asia meridional" = "Asia meridional",
  "MENA" = "Oriente Medio, Norte de África, Afganistán y Pakistán",
  "África al sur del Sahara" = "África al sur del Sahara",
  "No clasificada" = "No clasificada"
)

region_colors <- c(
  "América Latina y el Caribe" = "#1B9E77",
  "América del Norte" = "#7570B3",
  "Europa" = "#D95F02",
  "Asia central" = "#E6AB02",
  "Asia oriental y el Pacífico" = "#E7298A",
  "Asia meridional" = "#66A61E",
  "Oriente Medio, Norte de África, Afganistán y Pakistán" = "#A6761D",
  "África al sur del Sahara" = "#1F78B4",
  "No clasificada" = "#999999"
)

unclassified_region <- "No clasificada"

dashboard_data_dir <- "data/dashboard"
sankey_years <- c(2018L, 2021L, 2024L)
target_year_partner <- 2024L

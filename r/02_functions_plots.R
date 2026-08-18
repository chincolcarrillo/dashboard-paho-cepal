#  ----------- FUNCIONES AUXILIARES Y DE VISUALIZACION PARA DASHBOARD -----------

# 0. SET-UP ----

# cargar paquetes
if (!requireNamespace("pacman", quietly = TRUE)) {
  stop("Instale el paquete 'pacman' antes de renderizar el dashboard.", call. = FALSE)
}
library(pacman)
p_load(tidyverse,
       plotly,
       scales,
       glue,
       htmlwidgets,
       networkD3,
       reactable,
       htmltools,
       forcats,
       stringr,
       rlang)

source("r/00_config.R", encoding = "UTF-8")

# 1. PARAMS GLOBALES----

# Carpeta donde se almacenan las bases livianas
dashboard_data_dir <- "data/dashboard"

# Nombres esperados de archivos livianos.
dashboard_files <- list(
  exports_region_hc_cat2 = "exports_region_hc_cat2.rds",
  trade_balance_lac = "trade_balance_lac.rds",
  partner_region_lac_2024 = "partner_region_lac_2024.rds",
  sankey_intra_lac = "sankey_intra_lac.rds"
)

overview_dashboard_files <- list(
  overview_exports_trends = "overview_exports_trends.rds",
  overview_lac_country_category_trade = "overview_lac_country_category_trade.rds"
)

# Orden de regiones para mantener consistencia entre graficos.
region_order <- region_levels

# Etiquetas largas de region. Se usan cuando las bases solo traen codigos cortos.
region_labels <- region_long_labels

# Paleta de colores para regiones
region_palette <- region_colors

flow_labels <- c(
  "exports" = "Exportaciones",
  "imports" = "Importaciones",
  "Exports" = "Exportaciones",
  "Imports" = "Importaciones",
  "export" = "Exportaciones",
  "import" = "Importaciones"
)

hc_cat2_palette <- c(
  "Ingredientes farmacéuticos activos" = "#FDB462",
  "Células, tejidos y otras sustancias de origen humano o animal para uso terapéutico" = "#8DD3C7",
  "Medicamentos" = "#B3DE69",
  "Sangre y productos derivados, inmunoglobulinas y antisueros" = "#80B1D3",
  "Vacunas (humanas)" = "#FCCDE5",
  "Cuidado de heridas y dispositivos de protección" = "#FB8072",
  "Dispositivos cardiovasculares" = "#E15759",
  "Dispositivos de diagnóstico in vitro y de laboratorio" = "#FF9DA7",
  "Dispositivos de diagnóstico por imagen" = "#F28E2B",
  "Dispositivos de esterilización y desinfección" = "#FFBE7D",
  "Dispositivos odontológicos" = "#B07AA1",
  "Dispositivos para rehabilitación y asistencia" = "#D4A6C8",
  "Dispositivos quirúrgicos e invasivos" = "#9C755F",
  "Equipos de diagnóstico y monitoreo" = "#BAB0AC",
  "Equipos de laboratorio" = "#59A14F",
  "Equipos terapéuticos" = "#8CD17D",
  "Insumos médicos y suministros para la atención de pacientes" = "#EDC948",
  "Mobiliario médico y elementos de soporte" = "#76B7B2"
)

overview_line_palette <- c(
  "Mundo - Todos los productos" = "#4C78A8",
  "LAC - Todos los productos" = "#4C78A8",
  "Mundo - Medicamentos y otras tecnologías sanitarias" = "#8DD3C7",
  "LAC - Medicamentos y otras tecnologías sanitarias" = "#8DD3C7",
  "Mundo - Dispositivos médicos" = "#FB8072",
  "LAC - Dispositivos médicos" = "#FB8072",
  "Mundo - Ingredientes farmacéuticos activos" = "#FDB462",
  "LAC - Ingredientes farmacéuticos activos" = "#FDB462"
)

# 2. FXS AUXILIARES DE CARGA ----

# cargar un archivo .rds desde data/dashboard/
load_dashboard_data <- function(file_name, data_dir = dashboard_data_dir) {
  file_path <- file.path(data_dir, file_name)

  if (!file.exists(file_path)) {
    cli_msg <- glue::glue(
      "No se encontró el archivo '{file_name}' en '{data_dir}'.\n",
      "Ruta evaluada: {normalizePath(file_path, winslash = '/', mustWork = FALSE)}\n",
      "Revise el directorio de trabajo de Quarto o use una ruta relativa equivalente, ",
      "por ejemplo data_dir = '../data/dashboard'."
    )
    stop(cli_msg, call. = FALSE)
  }

  readRDS(file_path)
}

## (NO SE SI LO NECESITE) ----
# Cargar todas las bases livianas del dashboard 
load_all_dashboard_data <- function(data_dir = dashboard_data_dir) {
  purrr::imap(
    dashboard_files,
    ~ load_dashboard_data(file_name = .x, data_dir = data_dir)
  )
}

# Validar columnas requeridas
#' Argumentos:
#'   data: data frame a revisar.
#'   required_cols: vector de columnas requeridas.
#'   data_name: nombre descriptivo de la base para mensajes de error.
#' Devuelve: TRUE invisiblemente si la validación es exitosa.
check_required_columns <- function(data, required_cols, data_name = "data") {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      glue::glue(
        "La base '{data_name}' no contiene las siguientes columnas requeridas: ",
        "{paste(missing_cols, collapse = ', ')}."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# 3. FXS AUXILIARES DE FORMATO ----

# Nota: las variables de valor de flujo estan expresadas en miles de USD
# Por tanto:
#   1 unidad = US$ 1.000
#   millones de USD = x / 1.000
#   mil millones de USD = x / 1.000.000
# En las etiquetas se usa espaniol: "millones" y "mil millones".

# Formatear valores expresados en miles de USD
format_usd_thousands <- function(x, accuracy = 0.1) {
  scales::label_number(
    prefix = "US$ ",
    suffix = " mil",
    accuracy = accuracy,
    big.mark = ".",
    decimal.mark = ","
  )(x)
}

# Formatear valores expresados en millones de USD desde una variable en miles
format_usd_millions <- function(x, accuracy = 0.1) {
  scales::label_number(
    prefix = "US$ ",
    suffix = " millones",
    accuracy = accuracy,
    big.mark = ".",
    decimal.mark = ","
  )(x / 1000)
}

# Formatear valores expresados en mil millones de USD desde una variable en miles
format_usd_billions <- function(x, accuracy = 0.1) {
  scales::label_number(
    prefix = "US$ ",
    suffix = " mil millones",
    accuracy = accuracy,
    big.mark = ".",
    decimal.mark = ","
  )(x / 1000000)
}

# Formatear porcentajes
# Asumiendo que x es proporcion entre 0 y 1
format_percent_label <- function(x, accuracy = 0.1) {
  scales::label_percent(
    accuracy = accuracy,
    big.mark = ".",
    decimal.mark = ","
  )(x)
}

# crear etiquetas largas de region cuando la base trae nombres cortos
#' Argumentos:
#'   x: vector con nombres o códigos de región.
#' Devuelve: vector de etiquetas largas.
recode_region_names <- function(x) {
  x_chr <- as.character(x)
  dplyr::recode(x_chr, !!!region_labels, .default = x_chr)
}

# Ordenar regiones segun criterio global (orden visual consistente)
#' Argumentos:
#'   x: vector de regiones cortas o largas.
#' Devuelve: factor ordenado.
order_regions <- function(x) {
  x_long <- recode_region_names(x)
  region_order_long <- recode_region_names(region_order)

  factor(
    x_long,
    levels = c(region_order_long, setdiff(unique(x_long), region_order_long))
  )
}

#' Crear tooltip HTML simple para plotly (unir lineas ya formateadas)
#' Argumentos:
#'   ...: textos o vectores de texto de igual longitud.
#' Devuelve: vector de strings con saltos HTML.
make_tooltip <- function(...) {
  inputs <- list(...)
  purrr::pmap_chr(inputs, ~ paste(c(...), collapse = "<br>"))
}

#' Aplicar paleta regional solo para regiones conocidas
#' (evitar errores cuando aparecen regiones no contempladas)
scale_fill_region <- function(...) {
  ggplot2::scale_fill_manual(values = region_palette, drop = FALSE, ...)
}

#' Completar paleta regional con etiquetas observadas en datos heredados
#'
#' Algunas bases ya renderizadas pueden conservar etiquetas anteriores, como
#' "Unclassified" o "Europe & Central Asia". Esta función evita que una escala
#' manual cerrada bloquee el render mientras se reconstruyen los `.rds`.
complete_region_palette <- function(region_names) {
  observed_regions <- unique(as.character(region_names))
  missing_regions <- setdiff(observed_regions, names(region_palette))

  if (length(missing_regions) == 0) {
    return(region_palette)
  }

  fallback_values <- stats::setNames(
    rep("#999999", length(missing_regions)),
    missing_regions
  )

  c(region_palette, fallback_values)
}

# 4. FXS PARA OPCIONES DE SELECTORES ----

#' Obtener categorías de productos disponibles
#'
#' Devuelve: vector ordenado de categorias hc_cat2.
get_hc_cat2_choices <- function(data) {
  check_required_columns(data, "hc_cat2", "data")

  data |>
    dplyr::distinct(hc_cat2) |>
    dplyr::filter(!is.na(hc_cat2)) |>
    dplyr::arrange(hc_cat2) |>
    dplyr::pull(hc_cat2)
}

#' Obtener areas de referencia disponibles
#'
#' Devuelve: vector nombrado, donde nombres = ref_area_name y valores = ref_area_code.
get_area_choices <- function(data) {
  check_required_columns(data, c("ref_area_code", "ref_area_name"), "data")

  choices <- data |>
    dplyr::distinct(ref_area_code, ref_area_name) |>
    dplyr::filter(!is.na(ref_area_code), !is.na(ref_area_name)) |>
    dplyr::arrange(dplyr::desc(ref_area_code == "LAC"), ref_area_name)

  stats::setNames(choices$ref_area_code, choices$ref_area_name)
}

#' Obtener anios disponibles
#'
#' Devuelve: vector ordenado de anios.
get_year_choices <- function(data) {
  check_required_columns(data, "year", "data")

  data |>
    dplyr::distinct(year) |>
    dplyr::filter(!is.na(year)) |>
    dplyr::arrange(year) |>
    dplyr::pull(year)
}

# 5. GRAFICO: Stacked area chart de exportaciones mundiales ----

#' Objetivo: construir un stacked area chart para comparar la participación de LAC
#' y otras regiones en las exportaciones mundiales por categoria de producto.
#' Argumentos principales:
#'   data: base exports_region_hc_cat2.
#'   selected_hc_cat2: categoría de producto. Si es NULL, usa todas las categorías
#'     agregándolas por año y región.
#'   value_var: variable de participación. Por defecto share_exports_value.
#'   highlight_region: region a destacar conceptualmente en titulo/subtitulo.
#'   interactive: si TRUE devuelve plotly; si FALSE devuelve ggplot.
#' Devuelve: objeto plotly o ggplot listo para Quarto.
plot_exports_region_area <- function(
  data,
  selected_hc_cat2 = NULL,
  value_var = "share_exports_value",
  highlight_region = "LAC",
  interactive = TRUE
) {
  required_cols <- c(
    "year", "exp_region", "hc_cat2",
    "exports_1000usd", "world_exports_1000usd"
  )
  check_required_columns(data, required_cols, "exports_region_hc_cat2")

  if (value_var != "share_exports_value") {
    warning(
      "plot_exports_region_area() está diseñada para graficar participación. ",
      "Se usará 'share_exports_value' calculada como exports_1000usd / world_exports_1000usd.",
      call. = FALSE
    )
    value_var <- "share_exports_value"
  }

  selected_label <- if (is.null(selected_hc_cat2)) {
    "Todas las categorías"
  } else {
    selected_hc_cat2
  }

  plot_data <- data

  if (!is.null(selected_hc_cat2)) {
    plot_data <- plot_data |>
      dplyr::filter(.data$hc_cat2 == .env$selected_hc_cat2)
  }

  plot_data <- plot_data |>
    dplyr::group_by(year, exp_region) |>
    dplyr::summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      world_exports_1000usd = sum(world_exports_1000usd, na.rm = TRUE),
      share_exports_value = exports_1000usd / world_exports_1000usd,
      hc_cat2 = selected_label,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      exp_region_longname = order_regions(exp_region),
      tooltip = make_tooltip(
        glue::glue("<b>Año:</b> {year}"),
        glue::glue("<b>Región exportadora:</b> {exp_region_longname}"),
        glue::glue("<b>Categoría:</b> {hc_cat2}"),
        glue::glue("<b>Exportaciones:</b> {format_usd_millions(exports_1000usd)}"),
        glue::glue("<b>Participación mundial:</b> {format_percent_label(.data[[value_var]])}")
      )
    )

  p <- plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = year,
        y = .data[[value_var]],
        fill = exp_region_longname,
        text = tooltip
      )
    ) +
    ggplot2::geom_area(alpha = 0.9, color = "white", linewidth = 0.15) +
    scale_fill_region(name = "Región exportadora") +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.03))
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      title = "Participación regional en exportaciones mundiales",
      subtitle = glue::glue(
        "Categoría: {selected_label}. Región de interés: {recode_region_names(highlight_region)}"
      ),
      x = NULL,
      y = "Participación en exportaciones mundiales",
      caption = "Fuente: elaboración propia con base en BACI/CEPII y clasificación PAHO. Valores monetarios expresados en miles de USD en las bases procesadas."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(interactive)) {
    return(plotly::ggplotly(p, tooltip = "text") |>
             plotly::layout(legend = list(orientation = "h", x = 0, y = -0.2)))
  }

  p
}

# 6. GRAFICO: Balance comercial ------

#' Objetivo: visualizar exportaciones positivas, importaciones negativas y balance
#' comercial neto como linea de tiempo
#' Argumentos principales:
#'   data: base trade_balance_lac.
#'   selected_area: código de país o agregado regional, por defecto "LAC".
#'   selected_hc_cat2: categoría de producto. Si es NULL, usa todas las categorías
#'     agregándolas por año y área.
#'   interactive: si TRUE devuelve plotly; si FALSE devuelve ggplot.
#' Devuelve: objeto plotly o ggplot listo para Quarto.

plot_trade_balance <- function(
  data,
  selected_area = "LAC",
  selected_hc_cat2 = NULL,
  interactive = TRUE
) {
  required_cols <- c(
    "year", "ref_area_code", "ref_area_name", "ref_area_type", "hc_cat2",
    "exports_1000usd", "imports_1000usd", "balance_1000usd"
  )
  check_required_columns(data, required_cols, "trade_balance_lac")

  selected_label <- if (is.null(selected_hc_cat2)) {
    "Todas las categorías"
  } else {
    selected_hc_cat2
  }

  base_data <- data |>
    dplyr::filter(.data$ref_area_code == .env$selected_area)

  if (!is.null(selected_hc_cat2)) {
    base_data <- base_data |>
      dplyr::filter(.data$hc_cat2 == .env$selected_hc_cat2)
  }

  base_data <- base_data |>
    dplyr::group_by(year, ref_area_code, ref_area_name, ref_area_type) |>
    dplyr::summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      imports_1000usd = sum(imports_1000usd, na.rm = TRUE),
      balance_1000usd = sum(balance_1000usd, na.rm = TRUE),
      hc_cat2 = selected_label,
      .groups = "drop"
    )

  if (nrow(base_data) == 0) {
    stop(
      glue::glue(
        "No hay datos para selected_area = '{selected_area}' y selected_hc_cat2 = '{selected_label}'."
      ),
      call. = FALSE
    )
  }

  bars_data <- base_data |>
    dplyr::transmute(
      year,
      ref_area_code,
      ref_area_name,
      hc_cat2,
      Exports = exports_1000usd,
      Imports = -imports_1000usd
    ) |>
    tidyr::pivot_longer(
      cols = c("Exports", "Imports"),
      names_to = "flow_type",
      values_to = "value_plot_1000usd"
    ) |>
    dplyr::mutate(
      value_abs_1000usd = abs(value_plot_1000usd),
      tooltip = make_tooltip(
        glue::glue("<b>Año:</b> {year}"),
        glue::glue("<b>Área:</b> {ref_area_name}"),
        glue::glue("<b>Categoría:</b> {hc_cat2}"),
        glue::glue("<b>Flujo:</b> {flow_type}"),
        glue::glue("<b>Valor:</b> {format_usd_millions(value_abs_1000usd)}")
      )
    )

  line_data <- base_data |>
    dplyr::mutate(
      tooltip = make_tooltip(
        glue::glue("<b>Año:</b> {year}"),
        glue::glue("<b>Área:</b> {ref_area_name}"),
        glue::glue("<b>Categoría:</b> {hc_cat2}"),
        glue::glue("<b>Balance comercial:</b> {format_usd_millions(balance_1000usd)}")
      )
    )

  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = bars_data,
      ggplot2::aes(
        x = year,
        y = value_plot_1000usd / 1000,
        fill = flow_type,
        text = tooltip
      ),
      position = "identity",
      alpha = 0.85,
      width = 0.75
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "grey35") +
    ggplot2::geom_line(
      data = line_data,
      ggplot2::aes(
        x = year,
        y = balance_1000usd / 1000,
        text = tooltip,
        group = 1
      ),
      linewidth = 1.1,
      color = "grey15"
    ) +
    ggplot2::geom_point(
      data = line_data,
      ggplot2::aes(
        x = year,
        y = balance_1000usd / 1000,
        text = tooltip
      ),
      size = 2.2,
      color = "grey15"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Exports" = "#1b9e77", "Imports" = "#d95f02"),
      name = "Flujo"
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        prefix = "US$ ", suffix = " M",
        big.mark = ".", decimal.mark = ","
      )
    ) +
    ggplot2::labs(
      title = glue::glue("Exportaciones, importaciones y balance comercial: {unique(base_data$ref_area_name)}"),
      x = NULL,
      y = "Millones de USD",
      caption = "Nota: las importaciones se grafican con signo negativo sólo para visualización. El balance corresponde a exportaciones menos importaciones."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(interactive)) {
    return(plotly::ggplotly(p, tooltip = "text") |>
             plotly::layout(legend = list(orientation = "h", x = 0, y = -0.2)))
  }

  p
}

# 7. GRAFICO: Barras apiladas al 100% de region origen/destino ---------

#' Objetivo: construir barras apiladas al 100% para observar el destino de las
#' exportaciones y el origen de las importaciones de LAC o un pais seleccionado.
#' Argumentos principales:
#'   data: base partner_region_lac_2024.
#'   selected_area: código de país o agregado regional, por defecto "LAC".
#'   selected_hc_cat2: categoría de producto. Si es NULL, usa todas las categorías
#'     agregándolas por flujo y región contraparte.
#'   interactive: si TRUE devuelve plotly; si FALSE devuelve ggplot.
#' Devuelve: objeto plotly o ggplot listo para Quarto.
#'
#' Nota:
#'   En exportaciones, partner_region corresponde al destino.
#'   En importaciones, partner_region corresponde al origen.
plot_partner_region_100pct <- function(
  data,
  selected_area = "LAC",
  selected_hc_cat2 = NULL,
  interactive = TRUE
) {
  required_cols <- c(
    "year", "ref_area_code", "ref_area_name", "ref_area_type", "hc_cat2",
    "flow_type", "partner_region", "value_1000usd", "total_flow_1000usd"
  )
  check_required_columns(data, required_cols, "partner_region_lac_2024")

  selected_label <- if (is.null(selected_hc_cat2)) {
    "Todas las categorías"
  } else {
    selected_hc_cat2
  }

  plot_data <- data |>
    dplyr::filter(.data$ref_area_code == .env$selected_area)

  if (!is.null(selected_hc_cat2)) {
    plot_data <- plot_data |>
      dplyr::filter(.data$hc_cat2 == .env$selected_hc_cat2)
  }

  if (!"partner_region_longname" %in% names(plot_data)) {
    plot_data <- plot_data |>
      dplyr::mutate(partner_region_longname = recode_region_names(partner_region))
  }

  plot_data <- plot_data |>
    dplyr::mutate(
      partner_region_longname = as.character(partner_region_longname),
      flow_type = dplyr::recode(as.character(flow_type), !!!flow_labels, .default = as.character(flow_type))
    ) |>
    dplyr::mutate(hc_cat2 = selected_label) |>
    dplyr::group_by(
      year, ref_area_code, ref_area_name, ref_area_type,
      hc_cat2, flow_type, partner_region, partner_region_longname
    ) |>
    dplyr::summarise(
      value_1000usd = sum(value_1000usd, na.rm = TRUE),
      total_flow_1000usd = sum(total_flow_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(year, ref_area_code, flow_type) |>
    dplyr::mutate(
      share_flow_value = value_1000usd / sum(value_1000usd, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      partner_region_longname = order_regions(partner_region_longname),
      counterparty_label = dplyr::if_else(
        flow_type == "Exportaciones",
        "Destino regional",
        "Origen regional"
      ),
      tooltip = make_tooltip(
        glue::glue("<b>Año:</b> {year}"),
        glue::glue("<b>Área:</b> {ref_area_name}"),
        glue::glue("<b>Categoría:</b> {hc_cat2}"),
        glue::glue("<b>Flujo:</b> {flow_type}"),
        glue::glue("<b>{counterparty_label}:</b> {partner_region_longname}"),
        glue::glue("<b>Participación:</b> {format_percent_label(share_flow_value)}"),
        glue::glue("<b>Valor:</b> {format_usd_millions(value_1000usd)}")
      )
    )

  if (nrow(plot_data) == 0) {
    stop(
      glue::glue(
        "No hay datos para selected_area = '{selected_area}' y selected_hc_cat2 = '{selected_label}'."
      ),
      call. = FALSE
    )
  }

  plot_palette <- complete_region_palette(plot_data$partner_region_longname)

  p <- plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = share_flow_value,
        y = flow_type,
        fill = partner_region_longname,
        text = tooltip
      )
    ) +
    ggplot2::geom_col(width = 0.68, color = "white", linewidth = 0.25) +
    scale_fill_region(name = "Región contraparte") +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      title = glue::glue("Composición regional del comercio: {unique(plot_data$ref_area_name)}"),
      x = "Participación del flujo total",
      y = NULL,
      caption = "Nota: en exportaciones la región contraparte corresponde al destino; en importaciones corresponde al origen."
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(interactive)) {
    plot_widget <- plotly::plot_ly()
    region_names <- levels(plot_data$partner_region_longname)
    region_names <- region_names[region_names %in% as.character(plot_data$partner_region_longname)]

    for (region_name in region_names) {
      region_data <- plot_data |>
        dplyr::filter(as.character(.data$partner_region_longname) == .env$region_name)

      plot_widget <- plot_widget |>
        plotly::add_trace(
          data = region_data,
          x = ~share_flow_value,
          y = ~flow_type,
          type = "bar",
          orientation = "h",
          name = region_name,
          marker = list(color = unname(plot_palette[[region_name]])),
          customdata = ~tooltip,
          textposition = "none",
          hovertemplate = "%{customdata}<extra></extra>"
        )
    }

    return(plot_widget |>
             plotly::layout(
               barmode = "stack",
               title = list(
                 text = glue::glue(
                   "Composición regional del comercio: {unique(plot_data$ref_area_name)}"
                 ),
                 x = 0.5,
                 xanchor = "center"
               ),
               xaxis = list(
                 title = "Participación del flujo total",
                 range = c(0, 1),
                 tickformat = ".0%"
               ),
               yaxis = list(
                 title = "",
                 categoryorder = "array",
                 categoryarray = c("Exportaciones", "Importaciones")
               ),
               margin = list(l = 115, r = 35, t = 85, b = 120),
               legend = list(
                 orientation = "h",
                 x = 0.5,
                 xanchor = "center",
                 y = -0.25
               )
             ))
  }

  p
}

# 8. GRAFICO: Sankey comercio intrarregional LAC ------

#' Objetivo: construir tablas de nodos y links compatibles con networkD3.
#' Argumentos principales:
#'   data: base sankey_intra_lac.
#'   selected_year: año a graficar.
#'   selected_hc_cat2: categoría de producto. Si es NULL, usa todas las categorías
#'     agregándolas por par exportador-importador.
#'   min_value_1000usd: umbral mínimo de valor en miles de USD.
#'   top_n_flows: número máximo de flujos principales a conservar.
#'   keep_self_flows: si TRUE conserva flujos donde source == target.
#' Devuelve: lista con nodes y links.
#'
#' Nota:
#'   networkD3 usa índices de nodos que comienzan en cero.
prepare_sankey_data <- function(
  data,
  selected_year = 2024,
  selected_hc_cat2 = NULL,
  min_value_1000usd = 0,
  top_n_flows = NULL,
  keep_self_flows = FALSE
) {
  required_cols <- c(
    "year", "hc_cat2", "source", "source_name", "target", "target_name", "value_1000usd"
  )
  check_required_columns(data, required_cols, "sankey_intra_lac")

  selected_label <- if (is.null(selected_hc_cat2)) {
    "Todas las categorías"
  } else {
    selected_hc_cat2
  }

  links_raw <- data |>
    dplyr::filter(.data$year == .env$selected_year)

  if (!is.null(selected_hc_cat2)) {
    links_raw <- links_raw |>
      dplyr::filter(.data$hc_cat2 == .env$selected_hc_cat2)
  }

  links_raw <- links_raw |>
    dplyr::filter(.env$keep_self_flows | .data$source != .data$target) |>
    dplyr::group_by(source, source_name, target, target_name) |>
    dplyr::summarise(
      value_1000usd = sum(value_1000usd, na.rm = TRUE),
      quantity_tons = if ("quantity_tons" %in% names(data)) {
        sum(.data$quantity_tons, na.rm = TRUE)
      } else {
        NA_real_
      },
      year = selected_year,
      hc_cat2 = selected_label,
      .groups = "drop"
    ) |>
    dplyr::filter(.data$value_1000usd >= .env$min_value_1000usd) |>
    dplyr::arrange(dplyr::desc(value_1000usd))

  if (!is.null(top_n_flows)) {
    links_raw <- links_raw |>
      dplyr::slice_head(n = top_n_flows)
  }

  if (nrow(links_raw) == 0) {
    stop(
      glue::glue(
        "No hay flujos para selected_year = {selected_year}, ",
        "selected_hc_cat2 = '{selected_label}', ",
        "min_value_1000usd = {min_value_1000usd}."
      ),
      call. = FALSE
    )
  }

  nodes <- tibble::tibble(
    name = unique(c(links_raw$source_name, links_raw$target_name))
  ) |>
    dplyr::arrange(name) |>
    dplyr::mutate(node_id = dplyr::row_number() - 1L)

  links <- links_raw |>
    dplyr::left_join(nodes, by = c("source_name" = "name")) |>
    dplyr::rename(source_id = node_id) |>
    dplyr::left_join(nodes, by = c("target_name" = "name")) |>
    dplyr::rename(target_id = node_id) |>
    dplyr::mutate(
      value = value_1000usd,
      tooltip = glue::glue(
        "{source_name} → {target_name}<br>",
        "Valor: {format_usd_millions(value_1000usd)}<br>",
        "Año: {year}<br>",
        "Categoría: {hc_cat2}"
      )
    ) |>
    dplyr::select(
      source_id, target_id, value,
      source_name, target_name, year, hc_cat2,
      value_1000usd, quantity_tons, tooltip
    )

  list(
    nodes = nodes |>
      dplyr::select(name),
    links = links
  )
}

#' Graficar Sankey intrarregional LAC
#'
#' Objetivo: mostrar flujos de comercio intrarregional entre países de LAC.
#' Base esperada: sankey_intra_lac.rds.
#' Argumentos principales:
#'   data: base sankey_intra_lac.
#'   selected_year: año a graficar.
#'   selected_hc_cat2: categoría de producto.
#'   min_value_1000usd: umbral mínimo en miles de USD.
#'   top_n_flows: principales flujos a mantener.
#'   height, width: dimensiones del htmlwidget.
#' Devuelve: objeto htmlwidget de networkD3.
#'
#' Recomendación:
#'   Para dashboards, conviene limitar top_n_flows, por ahora: 40
plot_sankey_intra_lac <- function(
  data,
  selected_year = 2024,
  selected_hc_cat2 = NULL,
  min_value_1000usd = 0,
  top_n_flows = 40,
  height = 500,
  width = NULL
) {
  required_cols <- c(
    "year", "hc_cat2", "source", "source_name", "target", "target_name", "value_1000usd"
  )
  check_required_columns(data, required_cols, "sankey_intra_lac")

  selected_label <- if (is.null(selected_hc_cat2)) {
    "Todas las categorías"
  } else {
    selected_hc_cat2
  }

  plot_data <- data |>
    dplyr::filter(
      .data$year == .env$selected_year,
      .data$source != .data$target,
      .data$value_1000usd >= .env$min_value_1000usd
    )

  if (!is.null(selected_hc_cat2)) {
    plot_data <- plot_data |>
      dplyr::filter(.data$hc_cat2 == .env$selected_hc_cat2)
  }

  plot_data <- plot_data |>
    dplyr::mutate(value_musd = to_musd(value_1000usd)) |>
    dplyr::arrange(dplyr::desc(value_musd))

  if (!is.null(top_n_flows)) {
    plot_data <- plot_data |>
      dplyr::slice_head(n = top_n_flows)
  }

  if (nrow(plot_data) == 0) {
    rlang::abort(glue::glue(
      "No hay flujos intrarregionales para el Sankey en {selected_year}."
    ))
  }

  links_raw <- plot_data |>
    dplyr::transmute(
      source_node = paste0("Exp: ", source_name),
      target_node = paste0("Imp: ", target_name),
      value = value_musd,
      tooltip = glue::glue(
        "Exp: {source_name}<br>",
        "Imp: {target_name}<br>",
        "Valor: {scales::label_number(accuracy = 0.1, big.mark = '.', decimal.mark = ',')(value_musd)} millones USD"
      )
    )

  source_nodes <- links_raw |>
    dplyr::group_by(source_node) |>
    dplyr::summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_value), source_node) |>
    dplyr::transmute(
      name = source_node,
      node_total_value = total_value
    )

  target_nodes <- links_raw |>
    dplyr::group_by(target_node) |>
    dplyr::summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(total_value), target_node) |>
    dplyr::transmute(
      name = target_node,
      node_total_value = total_value
    )

  nodes <- dplyr::bind_rows(source_nodes, target_nodes) |>
    dplyr::mutate(
      node_type = dplyr::if_else(
        stringr::str_starts(name, "Exp: "),
        "source",
        "target"
      )
    ) |>
    dplyr::group_by(node_type) |>
    dplyr::arrange(dplyr::desc(node_total_value), name, .by_group = TRUE) |>
    dplyr::mutate(
      node_rank = dplyr::row_number(),
      node_count = dplyr::n(),
      node_gap = dplyr::if_else(
        node_count > 1,
        pmin(0.022, 0.10 / (node_count - 1)),
        0
      ),
      available_y = 0.92 - node_gap * (node_count - 1),
      node_share = node_total_value / sum(node_total_value, na.rm = TRUE),
      node_height = pmax(node_share * available_y * 0.72, 0.018),
      x = dplyr::if_else(node_type == "source", 0.01, 0.99),
      # Plotly interpreta y como el borde superior del nodo. Usar posiciones
      # acumuladas evita que los nodos grandes se monten sobre el siguiente.
      y = 0.04 + dplyr::lag(cumsum(node_height + node_gap), default = 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(node_id = dplyr::row_number() - 1L)

  links <- links_raw |>
    dplyr::mutate(
      source = match(source_node, nodes$name) - 1L,
      target = match(target_node, nodes$name) - 1L
    ) |>
    dplyr::select(source, target, value, tooltip)

  plotly::plot_ly(
    type = "sankey",
    arrangement = "fixed",
    width = width,
    height = height,
    domain = list(y = c(0, 0.88)),
    node = list(
      label = nodes$name,
      x = nodes$x,
      y = nodes$y,
      pad = 8,
      thickness = 22,
      line = list(color = "rgba(80,80,80,0.35)", width = 0.5),
      hovertemplate = "%{label}<extra></extra>"
    ),
    link = list(
      source = links$source,
      target = links$target,
      value = links$value,
      customdata = links$tooltip,
      hovertemplate = "%{customdata}<extra></extra>"
    )
  ) |>
    plotly::layout(
      title = list(
        text = glue::glue(
          "Principales flujos intrarregionales LAC",
          "<br><sup>{selected_label}, {selected_year}. Valores en millones USD</sup>"
        ),
        x = 0.5,
        xanchor = "center"
      ),
      margin = list(l = 20, r = 20, t = 80, b = 20)
    )
}

#' Graficar Sankeys intrarregionales por año
#'
#' Objetivo: generar una lista nombrada de Sankeys, útil para comparar años lado
#' a lado en Quarto.
#' Base esperada: sankey_intra_lac.rds.
#' Argumentos principales:
#'   data: base sankey_intra_lac.
#'   years: vector de años a comparar, por defecto 2018, 2021 y 2024.
#'   selected_hc_cat2: categoría de producto.
#'   min_value_1000usd: umbral mínimo en miles de USD.
#'   top_n_flows: principales flujos a mantener.
#' Devuelve: lista nombrada de objetos htmlwidget.
plot_sankey_intra_lac_by_year <- function(
  data,
  years = c(2018, 2021, 2024),
  selected_hc_cat2 = NULL,
  min_value_1000usd = 0,
  top_n_flows = 40
) {
  sankeys <- purrr::map(
    years,
    ~ plot_sankey_intra_lac(
      data = data,
      selected_year = .x,
      selected_hc_cat2 = selected_hc_cat2,
      min_value_1000usd = min_value_1000usd,
      top_n_flows = top_n_flows
    )
  )

  names(sankeys) <- as.character(years)
  sankeys
}

# 9. HELPERS PARA PÁGINAS ESTÁTICAS POR CATEGORÍA ----

resolve_dashboard_data_dir <- function(data_dir = dashboard_data_dir) {
  candidates <- unique(c(data_dir, "data/dashboard", "../data/dashboard"))
  existing <- candidates[dir.exists(candidates)]

  if (length(existing) == 0) {
    rlang::abort(glue::glue(
      "No se encontró la carpeta de datos. Rutas probadas: {paste(candidates, collapse = ', ')}"
    ))
  }

  existing[[1]]
}

validate_category_data <- function(data_list, category) {
  if (!category %in% hc_cat2_levels) {
    rlang::abort(glue::glue("La categoría '{category}' no pertenece a hc_cat2."))
  }

  purrr::iwalk(data_list, function(data, data_name) {
    check_required_columns(data, "hc_cat2", data_name)
  })

  required_category_data <- c(
    "exports_region_hc_cat2",
    "trade_balance_lac"
  )
  purrr::walk(required_category_data, function(data_name) {
    if (!category %in% data_list[[data_name]]$hc_cat2) {
      rlang::abort(glue::glue(
        "La categoría '{category}' no está disponible en '{data_name}'."
      ))
    }
  })

  invisible(TRUE)
}

load_category_dashboard_data <- function(
  category,
  data_dir = dashboard_data_dir
) {
  resolved_dir <- resolve_dashboard_data_dir(data_dir)
  data <- load_all_dashboard_data(resolved_dir)
  validate_category_data(data, category)

  purrr::map(data, ~ dplyr::filter(.x, .data$hc_cat2 == .env$category))
}

load_overview_dashboard_data <- function(data_dir = dashboard_data_dir) {
  resolved_dir <- resolve_dashboard_data_dir(data_dir)

  data <- purrr::imap(
    overview_dashboard_files,
    ~ load_dashboard_data(file_name = .x, data_dir = resolved_dir)
  )

  check_required_columns(
    data$overview_exports_trends,
    c("year", "overview_tab", "region_scope", "product_group", "line_label", "exports_1000usd"),
    "overview_exports_trends"
  )

  check_required_columns(
    data$overview_lac_country_category_trade,
    c("year", "ref_area_code", "ref_area_name", "hc_cat2", "flow_type", "value_1000usd"),
    "overview_lac_country_category_trade"
  )

  data
}

to_musd <- function(x) {
  x / 1000
}

theme_trade <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

dashboard_card <- function(label, value, note = NULL) {
  htmltools::div(
    class = "dashboard-card",
    htmltools::tags$span(label, class = "dashboard-card-label"),
    htmltools::tags$h3(value),
    htmltools::tags$p(note)
  )
}

format_landing_kpi_value <- function(value, type = c("money", "percent")) {
  type <- rlang::arg_match(type)

  if (length(value) == 0 || is.na(value) || !is.finite(value)) {
    return("n/d")
  }

  if (type == "money") {
    return(format_usd_billions(value, accuracy = 0.1))
  }

  format_percent_label(value, accuracy = 0.1)
}

calculate_cagr <- function(value_start, value_end, years) {
  if (
    length(value_start) == 0 ||
      length(value_end) == 0 ||
      is.na(value_start) ||
      is.na(value_end) ||
      value_start <= 0 ||
      years <= 0
  ) {
    return(NA_real_)
  }

  (value_end / value_start)^(1 / years) - 1
}

prepare_landing_category_kpis <- function(
  exports_region,
  trade_balance,
  category_links,
  category_groups = NULL,
  cagr_window = 5
) {
  check_required_columns(
    exports_region,
    c("year", "exp_region", "hc_cat2", "exports_1000usd"),
    "exports_region_hc_cat2"
  )
  check_required_columns(
    trade_balance,
    c(
      "year", "ref_area_code", "ref_area_type", "hc_cat2",
      "exports_1000usd", "imports_1000usd"
    ),
    "trade_balance_lac"
  )

  world_exports <- exports_region |>
    dplyr::group_by(year, hc_cat2) |>
    dplyr::summarise(
      world_exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    )

  latest_year <- max(world_exports$year, na.rm = TRUE)
  first_year <- min(world_exports$year, na.rm = TRUE)
  cagr_start_year <- latest_year - cagr_window

  world_latest <- world_exports |>
    dplyr::filter(.data$year == .env$latest_year) |>
    dplyr::select(hc_cat2, market_total_global_1000usd = world_exports_1000usd)

  world_first <- world_exports |>
    dplyr::filter(.data$year == .env$first_year) |>
    dplyr::select(hc_cat2, first_world_exports_1000usd = world_exports_1000usd)

  world_cagr_start <- world_exports |>
    dplyr::filter(.data$year == .env$cagr_start_year) |>
    dplyr::select(hc_cat2, cagr_5y_world_exports_1000usd = world_exports_1000usd)

  lac_latest <- trade_balance |>
    dplyr::filter(
      .data$ref_area_code == "LAC",
      .data$ref_area_type == "region",
      .data$year == .env$latest_year
    ) |>
    dplyr::group_by(hc_cat2) |>
    dplyr::summarise(
      lac_exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      lac_imports_1000usd = sum(imports_1000usd, na.rm = TRUE),
      .groups = "drop"
    )

  if (is.null(category_groups)) {
    category_groups <- tibble::tibble(
      hc_cat2 = hc_cat2_levels,
      landing_kpi_group = dplyr::case_when(
        .data$hc_cat2 %in% medical_device_categories ~ "devices",
        .data$hc_cat2 == ifa_category ~ "inputs",
        TRUE ~ "other_health"
      ),
      landing_kpi_color = dplyr::case_when(
        .data$landing_kpi_group == "devices" ~ "#fb8072",
        .data$landing_kpi_group == "inputs" ~ "#fdb462",
        TRUE ~ "#8dd3c7"
      )
    )
  } else {
    check_required_columns(
      category_groups,
      c("hc_cat2", "landing_kpi_group", "landing_kpi_color"),
      "category_groups"
    )
  }

  tibble::tibble(hc_cat2 = hc_cat2_levels) |>
    dplyr::left_join(world_latest, by = "hc_cat2") |>
    dplyr::left_join(world_first, by = "hc_cat2") |>
    dplyr::left_join(world_cagr_start, by = "hc_cat2") |>
    dplyr::left_join(lac_latest, by = "hc_cat2") |>
    dplyr::left_join(category_groups, by = "hc_cat2") |>
    dplyr::mutate(
      category_link = unname(category_links[hc_cat2]),
      landing_kpi_group = tidyr::replace_na(.data$landing_kpi_group, "other_health"),
      landing_kpi_color = tidyr::replace_na(.data$landing_kpi_color, "#8dd3c7"),
      first_year = .env$first_year,
      latest_year = .env$latest_year,
      cagr_start_year = .env$cagr_start_year,
      cagr_all_years = purrr::pmap_dbl(
        list(first_world_exports_1000usd, market_total_global_1000usd),
        ~ calculate_cagr(..1, ..2, .env$latest_year - .env$first_year)
      ),
      cagr_5y = purrr::pmap_dbl(
        list(cagr_5y_world_exports_1000usd, market_total_global_1000usd),
        ~ calculate_cagr(..1, ..2, .env$cagr_window)
      )
    )
}

landing_category_kpi_card <- function(row) {
  category_name <- row$hc_cat2[[1]]
  category_link <- row$category_link[[1]]
  if (length(category_link) == 0 || is.na(category_link)) {
    category_link <- "#"
  }
  landing_kpi_color <- row$landing_kpi_color[[1]]
  if (length(landing_kpi_color) == 0 || is.na(landing_kpi_color)) {
    landing_kpi_color <- "#8dd3c7"
  }

  landing_kpi_metric <- function(label, value, class = NULL) {
    htmltools::div(
      class = paste(c("landing-kpi-metric", class), collapse = " "),
      htmltools::tags$span(label, class = "landing-kpi-metric-label"),
      htmltools::tags$strong(value, class = "landing-kpi-metric-value")
    )
  }

  htmltools::div(
    class = "landing-kpi-card",
    style = htmltools::css(border_left_color = landing_kpi_color),
    htmltools::tags$h3(
      htmltools::tags$a(
        href = category_link,
        category_name
      )
    ),
    htmltools::div(
      class = "landing-kpi-grid",
      htmltools::div(
        class = "landing-kpi-row landing-kpi-row-top",
        landing_kpi_metric(
          "Comercio total global",
          format_landing_kpi_value(row$market_total_global_1000usd, "money")
        ),
        landing_kpi_metric(
          glue::glue("CAGR {row$first_year}-{row$latest_year}"),
          format_landing_kpi_value(row$cagr_all_years, "percent")
        ),
        landing_kpi_metric(
          "CAGR últimos 5 años",
          format_landing_kpi_value(row$cagr_5y, "percent")
        )
      ),
      htmltools::div(
        class = "landing-kpi-row landing-kpi-row-bottom",
        landing_kpi_metric(
          glue::glue("Importaciones LAC {row$latest_year}"),
          format_landing_kpi_value(row$lac_imports_1000usd, "money"),
          class = "landing-kpi-metric-lac"
        ),
        landing_kpi_metric(
          glue::glue("Exportaciones LAC {row$latest_year}"),
          format_landing_kpi_value(row$lac_exports_1000usd, "money"),
          class = "landing-kpi-metric-lac"
        )
      )
    )
  )
}

render_landing_kpi_cards <- function(kpi_data) {
  health_left <- kpi_data |>
    dplyr::filter(.data$landing_kpi_group == "other_health")

  medical_devices <- kpi_data |>
    dplyr::filter(.data$landing_kpi_group == "devices")

  inputs <- kpi_data |>
    dplyr::filter(.data$landing_kpi_group == "inputs")

  health_left_cards <- htmltools::tagList(
    purrr::map(
      seq_len(nrow(health_left)),
      ~ landing_category_kpi_card(health_left[.x, ])
    )
  )
  medical_devices_cards <- htmltools::tagList(
    purrr::map(
      seq_len(nrow(medical_devices)),
      ~ landing_category_kpi_card(medical_devices[.x, ])
    )
  )
  inputs_cards <- htmltools::tagList(
    purrr::map(
      seq_len(nrow(inputs)),
      ~ landing_category_kpi_card(inputs[.x, ])
    )
  )

  htmltools::tagList(
    htmltools::tags$section(
      class = "landing-kpi-section",
      htmltools::tags$h2("Medicamentos y otras tecnologías sanitarias"),
      htmltools::div(
        class = "landing-kpi-column landing-kpi-column-wide",
        health_left_cards
      )
    ),
    htmltools::tags$section(
      class = "landing-kpi-section",
      htmltools::tags$h2("Dispositivos médicos"),
      htmltools::div(
        class = "landing-kpi-column landing-kpi-column-wide",
        medical_devices_cards
      )
    ),
    htmltools::tags$section(
      class = "landing-kpi-section",
      htmltools::tags$h2("Insumos"),
      htmltools::div(
        class = "landing-kpi-one-column",
        inputs_cards
      )
    )
  )
}

complete_hc_cat2_palette <- function(category_names) {
  observed_categories <- unique(as.character(category_names))
  missing_categories <- setdiff(observed_categories, names(hc_cat2_palette))

  if (length(missing_categories) == 0) {
    return(hc_cat2_palette)
  }

  fallback_values <- scales::hue_pal()(length(missing_categories))
  names(fallback_values) <- missing_categories

  c(hc_cat2_palette, fallback_values)
}

complete_overview_line_palette <- function(line_names) {
  observed_lines <- unique(as.character(line_names))
  missing_lines <- setdiff(observed_lines, names(overview_line_palette))

  if (length(missing_lines) == 0) {
    return(overview_line_palette)
  }

  fallback_values <- scales::hue_pal()(length(missing_lines))
  names(fallback_values) <- missing_lines

  c(overview_line_palette, fallback_values)
}

normalize_overview_product_groups <- function(data) {
  data |>
    dplyr::mutate(
      product_group = dplyr::case_when(
        .data$overview_tab == "Tecnologías sanitarias" &
          .data$product_group %in% c(
            "Otras tecnologías sanitarias",
            "Medicamentos y otras tecnologías sanitarias"
          ) ~ "Medicamentos y otras tecnologías sanitarias",
        TRUE ~ as.character(.data$product_group)
      ),
      line_label = paste(.data$region_scope, .data$product_group, sep = " - ")
    )
}

latest_common_year <- function(...) {
  years <- purrr::map(list(...), ~ unique(.x$year))
  common_years <- purrr::reduce(years, intersect)

  if (length(common_years) == 0) {
    rlang::abort("Las bases no comparten un año de referencia.")
  }

  max(common_years, na.rm = TRUE)
}

prepare_country_snapshot <- function(trade_balance, year) {
  trade_balance |>
    dplyr::filter(
      .data$ref_area_type == "country",
      .data$year == .env$year
    ) |>
    dplyr::group_by(year, ref_area_code, ref_area_name, ref_area_type) |>
    dplyr::summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      imports_1000usd = sum(imports_1000usd, na.rm = TRUE),
      balance_1000usd = sum(balance_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      exports_musd = to_musd(exports_1000usd),
      imports_musd = to_musd(imports_1000usd),
      balance_musd = to_musd(balance_1000usd),
      total_trade_musd = exports_musd + imports_musd
    ) |>
    dplyr::arrange(dplyr::desc(total_trade_musd))
}

validate_country_snapshot <- function(country_snapshot, data_name = "country_snapshot") {
  check_required_columns(
    country_snapshot,
    c("year", "ref_area_code", "ref_area_name"),
    data_name
  )

  years <- unique(country_snapshot$year)
  if (length(years) != 1) {
    rlang::abort(glue::glue(
      "'{data_name}' debe contener un solo año. Años encontrados: {paste(years, collapse = ', ')}."
    ))
  }

  if (anyDuplicated(country_snapshot$ref_area_code) > 0) {
    rlang::abort(glue::glue(
      "'{data_name}' debe contener una sola fila por país."
    ))
  }

  invisible(TRUE)
}

load_category_product_exports <- function(
  category,
  data_dir = dashboard_data_dir,
  file_name = "product_exports_lac_2024_by_country.rds"
) {
  resolved_dir <- resolve_dashboard_data_dir(data_dir)
  file_path <- file.path(resolved_dir, file_name)

  if (!file.exists(file_path)) {
    return(NULL)
  }

  product_exports <- readRDS(file_path)
  check_required_columns(
    product_exports,
    c(
      "year", "hc_cat2", "product", "description", "description_short",
      "exp_country_name", "exports_1000usd"
    ),
    file_name
  )

  product_exports |>
    dplyr::filter(.data$hc_cat2 == .env$category)
}

plot_overview_exports_trends <- function(
  data,
  selected_tab,
  interactive = TRUE
) {
  required_cols <- c(
    "year", "overview_tab", "region_scope", "product_group",
    "line_label", "exports_1000usd"
  )
  check_required_columns(data, required_cols, "overview_exports_trends")

  plot_data <- data |>
    normalize_overview_product_groups() |>
    dplyr::filter(.data$overview_tab == .env$selected_tab) |>
    dplyr::mutate(
      exports_musd = to_musd(exports_1000usd),
      line_label = as.character(line_label),
      tooltip = make_tooltip(
        glue::glue("<b>Año:</b> {year}"),
        glue::glue("<b>Ámbito:</b> {region_scope}"),
        glue::glue("<b>Grupo:</b> {product_group}"),
        glue::glue("<b>Exportaciones:</b> {format_usd_millions(exports_1000usd)}")
      )
    ) |>
    dplyr::arrange(line_label, year)

  if (nrow(plot_data) == 0) {
    rlang::abort(glue::glue("No hay datos para overview_tab = '{selected_tab}'."))
  }

  line_palette <- complete_overview_line_palette(plot_data$line_label)

  if (isTRUE(interactive)) {
    plot_widget <- plotly::plot_ly()
    line_names <- unique(plot_data$line_label)

    for (line_name in line_names) {
      line_data <- plot_data |>
        dplyr::filter(.data$line_label == .env$line_name)

      line_dash <- if (all(unique(line_data$region_scope) == "Mundo")) {
        "dash"
      } else {
        "solid"
      }

      plot_widget <- plot_widget |>
        plotly::add_trace(
          data = line_data,
          x = ~year,
          y = ~exports_musd,
          type = "scatter",
          mode = "lines+markers",
          name = line_name,
          text = ~tooltip,
          hovertemplate = "%{text}<extra></extra>",
          line = list(
            color = unname(line_palette[[line_name]]),
            width = 2.4,
            dash = line_dash
          ),
          marker = list(
            color = unname(line_palette[[line_name]]),
            size = 6
          )
        )
    }

    return(plot_widget |>
             plotly::layout(
               xaxis = list(title = ""),
               yaxis = list(
                 title = "Exportaciones, USD millones",
                 rangemode = "tozero"
               ),
               legend = list(orientation = "h", x = 0, y = -0.2),
               margin = list(l = 75, r = 25, t = 25, b = 80)
             ))
  }

  plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = year,
        y = exports_musd,
        color = line_label,
        linetype = region_scope,
        group = line_label,
        text = tooltip
      )
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_color_manual(values = line_palette) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        prefix = "US$ ",
        suffix = " M",
        big.mark = ".",
        decimal.mark = ","
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Exportaciones",
      color = NULL,
      linetype = NULL
    ) +
    theme_trade()
}

plot_overview_exports_growth_trends <- function(
  data,
  selected_tab,
  lag_years = 1,
  annualized = FALSE,
  interactive = TRUE
) {
  required_cols <- c(
    "year", "overview_tab", "region_scope", "product_group",
    "line_label", "exports_1000usd"
  )
  check_required_columns(data, required_cols, "overview_exports_trends")

  plot_data <- data |>
    normalize_overview_product_groups() |>
    dplyr::filter(.data$overview_tab == .env$selected_tab) |>
    dplyr::mutate(line_label = as.character(line_label)) |>
    dplyr::arrange(line_label, year) |>
    dplyr::group_by(line_label) |>
    dplyr::mutate(
      exports_lag_1000usd = dplyr::lag(exports_1000usd, n = lag_years),
      growth_rate = dplyr::if_else(
        exports_lag_1000usd > 0,
        exports_1000usd / exports_lag_1000usd - 1,
        NA_real_
      ),
      growth_rate = dplyr::if_else(
        annualized & !is.na(growth_rate),
        (1 + growth_rate)^(1 / lag_years) - 1,
        growth_rate
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(growth_rate)) |>
    dplyr::mutate(
      exports_musd = to_musd(exports_1000usd)
    )

  if (nrow(plot_data) == 0) {
    rlang::abort(glue::glue(
      "No hay datos suficientes para calcular crecimiento con rezago de {lag_years} año/s en '{selected_tab}'."
    ))
  }

  line_palette <- complete_overview_line_palette(plot_data$line_label)
  y_axis_title <- if (lag_years == 1) {
    "Crecimiento interanual de exportaciones"
  } else if (isTRUE(annualized)) {
    glue::glue("CAGR de exportaciones a {lag_years} años")
  } else {
    glue::glue("Crecimiento de exportaciones a {lag_years} años")
  }
  growth_label <- if (lag_years == 1) {
    "Crecimiento interanual"
  } else if (isTRUE(annualized)) {
    glue::glue("CAGR a {lag_years} años")
  } else {
    glue::glue("Crecimiento a {lag_years} años")
  }

  plot_data <- plot_data |>
    dplyr::mutate(
      tooltip = make_tooltip(
        glue::glue("<b>Año:</b> {year}"),
        glue::glue("<b>Ámbito:</b> {region_scope}"),
        glue::glue("<b>Grupo:</b> {product_group}"),
        glue::glue("<b>Exportaciones:</b> {format_usd_millions(exports_1000usd)}"),
        glue::glue("<b>{growth_label}:</b> {format_percent_label(growth_rate)}")
      )
    )

  if (isTRUE(interactive)) {
    plot_widget <- plotly::plot_ly()
    line_names <- unique(plot_data$line_label)

    for (line_name in line_names) {
      line_data <- plot_data |>
        dplyr::filter(.data$line_label == .env$line_name)

      line_dash <- if (all(unique(line_data$region_scope) == "Mundo")) {
        "dash"
      } else {
        "solid"
      }

      plot_widget <- plot_widget |>
        plotly::add_trace(
          data = line_data,
          x = ~year,
          y = ~growth_rate,
          type = "scatter",
          mode = "lines+markers",
          name = line_name,
          text = ~tooltip,
          hovertemplate = "%{text}<extra></extra>",
          line = list(
            color = unname(line_palette[[line_name]]),
            width = 2.4,
            dash = line_dash
          ),
          marker = list(
            color = unname(line_palette[[line_name]]),
            size = 6
          )
        )
    }

    return(plot_widget |>
             plotly::layout(
               shapes = list(
                 list(
                   type = "line",
                   x0 = min(plot_data$year, na.rm = TRUE),
                   x1 = max(plot_data$year, na.rm = TRUE),
                   y0 = 0,
                   y1 = 0,
                   line = list(color = "rgba(80,80,80,0.45)", width = 1)
                 )
               ),
               xaxis = list(title = ""),
               yaxis = list(
                 title = y_axis_title,
                 tickformat = ".1%"
               ),
               legend = list(orientation = "h", x = 0, y = -0.2),
               margin = list(l = 75, r = 25, t = 25, b = 80)
             ))
  }

  plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = year,
        y = growth_rate,
        color = line_label,
        linetype = region_scope,
        group = line_label,
        text = tooltip
      )
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35, color = "grey55") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_color_manual(values = line_palette) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 0.1, decimal.mark = ",")
    ) +
    ggplot2::labs(
      x = NULL,
      y = y_axis_title,
      color = NULL,
      linetype = NULL
    ) +
    theme_trade()
}

plot_overview_lac_country_category_trade <- function(
  data,
  flow_type,
  max_countries = NULL,
  interactive = TRUE
) {
  required_cols <- c(
    "year", "ref_area_code", "ref_area_name", "hc_cat2",
    "flow_type", "value_1000usd"
  )
  check_required_columns(data, required_cols, "overview_lac_country_category_trade")

  plot_data <- data |>
    dplyr::filter(.data$flow_type == .env$flow_type) |>
    dplyr::mutate(
      hc_cat2 = factor(as.character(hc_cat2), levels = hc_cat2_levels),
      value_musd = to_musd(value_1000usd)
    )

  if (nrow(plot_data) == 0) {
    rlang::abort(glue::glue("No hay datos para flow_type = '{flow_type}'."))
  }

  country_order <- plot_data |>
    dplyr::group_by(ref_area_name) |>
    dplyr::summarise(
      total_value_1000usd = sum(value_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_value_1000usd), ref_area_name)

  if (!is.null(max_countries)) {
    country_order <- country_order |>
      dplyr::slice_head(n = max_countries)

    plot_data <- plot_data |>
      dplyr::filter(.data$ref_area_name %in% country_order$ref_area_name)
  }

  country_levels <- country_order |>
    dplyr::pull(ref_area_name)

  plot_data <- plot_data |>
    dplyr::mutate(
      ref_area_name = factor(ref_area_name, levels = country_levels),
      tooltip = make_tooltip(
        glue::glue("<b>País:</b> {ref_area_name}"),
        glue::glue("<b>Categoría:</b> {hc_cat2}"),
        glue::glue("<b>Flujo:</b> {flow_type}"),
        glue::glue("<b>Valor:</b> {format_usd_millions(value_1000usd)}")
      )
    ) |>
    dplyr::arrange(ref_area_name, hc_cat2)

  plot_palette <- complete_hc_cat2_palette(plot_data$hc_cat2)

  if (isTRUE(interactive)) {
    plot_widget <- plotly::plot_ly()
    category_names <- hc_cat2_levels[hc_cat2_levels %in% as.character(plot_data$hc_cat2)]

    for (category_name in category_names) {
      category_data <- plot_data |>
        dplyr::filter(as.character(.data$hc_cat2) == .env$category_name)

      plot_widget <- plot_widget |>
        plotly::add_trace(
          data = category_data,
          x = ~value_musd,
          y = ~ref_area_name,
          type = "bar",
          orientation = "h",
          name = category_name,
          marker = list(color = unname(plot_palette[[category_name]])),
          customdata = ~tooltip,
          hovertemplate = "%{customdata}<extra></extra>"
        )
    }

    return(plot_widget |>
             plotly::layout(
               barmode = "stack",
               xaxis = list(title = glue::glue("{flow_type}, USD millones")),
               yaxis = list(
                 title = "",
                 categoryorder = "array",
                 categoryarray = rev(country_levels)
               ),
               legend = list(orientation = "h", x = 0, y = -0.2),
               margin = list(l = 155, r = 25, t = 25, b = 120)
             ))
  }

  plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = value_musd,
        y = forcats::fct_rev(ref_area_name),
        fill = hc_cat2,
        text = tooltip
      )
    ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = plot_palette, drop = FALSE) +
    ggplot2::labs(
      x = glue::glue("{flow_type}, USD millones"),
      y = NULL,
      fill = NULL
    ) +
    theme_trade()
}

plot_overview_lac_country_category_share <- function(
  data,
  flow_type,
  max_countries = NULL,
  interactive = TRUE
) {
  required_cols <- c(
    "year", "ref_area_code", "ref_area_name", "hc_cat2",
    "flow_type", "value_1000usd"
  )
  check_required_columns(data, required_cols, "overview_lac_country_category_trade")

  plot_data <- data |>
    dplyr::filter(.data$flow_type == .env$flow_type) |>
    dplyr::mutate(
      hc_cat2 = factor(as.character(hc_cat2), levels = hc_cat2_levels),
      value_musd = to_musd(value_1000usd)
    )

  if (nrow(plot_data) == 0) {
    rlang::abort(glue::glue("No hay datos para flow_type = '{flow_type}'."))
  }

  country_order <- plot_data |>
    dplyr::group_by(ref_area_name) |>
    dplyr::summarise(
      total_value_1000usd = sum(value_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_value_1000usd), ref_area_name)

  if (!is.null(max_countries)) {
    country_order <- country_order |>
      dplyr::slice_head(n = max_countries)

    plot_data <- plot_data |>
      dplyr::filter(.data$ref_area_name %in% country_order$ref_area_name)
  }

  country_levels <- country_order |>
    dplyr::pull(ref_area_name)

  plot_data <- plot_data |>
    dplyr::group_by(ref_area_name) |>
    dplyr::mutate(
      total_value_1000usd = sum(value_1000usd, na.rm = TRUE),
      share_value = dplyr::if_else(
        total_value_1000usd > 0,
        value_1000usd / total_value_1000usd,
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      ref_area_name = factor(ref_area_name, levels = country_levels),
      tooltip = make_tooltip(
        glue::glue("<b>País:</b> {ref_area_name}"),
        glue::glue("<b>Categoría:</b> {hc_cat2}"),
        glue::glue("<b>Flujo:</b> {flow_type}"),
        glue::glue("<b>Participación:</b> {format_percent_label(share_value)}"),
        glue::glue("<b>Valor:</b> {format_usd_millions(value_1000usd)}")
      )
    ) |>
    dplyr::arrange(ref_area_name, hc_cat2)

  plot_palette <- complete_hc_cat2_palette(plot_data$hc_cat2)

  if (isTRUE(interactive)) {
    plot_widget <- plotly::plot_ly()
    category_names <- hc_cat2_levels[hc_cat2_levels %in% as.character(plot_data$hc_cat2)]

    for (category_name in category_names) {
      category_data <- plot_data |>
        dplyr::filter(as.character(.data$hc_cat2) == .env$category_name)

      plot_widget <- plot_widget |>
        plotly::add_trace(
          data = category_data,
          x = ~share_value,
          y = ~ref_area_name,
          type = "bar",
          orientation = "h",
          name = category_name,
          marker = list(color = unname(plot_palette[[category_name]])),
          customdata = ~tooltip,
          hovertemplate = "%{customdata}<extra></extra>"
        )
    }

    return(plot_widget |>
             plotly::layout(
               barmode = "stack",
               xaxis = list(
                 title = glue::glue("Distribución de {tolower(flow_type)}"),
                 range = c(0, 1),
                 tickformat = ".0%"
               ),
               yaxis = list(
                 title = "",
                 categoryorder = "array",
                 categoryarray = rev(country_levels)
               ),
               legend = list(orientation = "h", x = 0, y = -0.2),
               margin = list(l = 155, r = 25, t = 25, b = 120)
             ))
  }

  plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = share_value,
        y = forcats::fct_rev(ref_area_name),
        fill = hc_cat2,
        text = tooltip
      )
    ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = plot_palette, drop = FALSE) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1)
    ) +
    ggplot2::labs(
      x = glue::glue("Distribución de {tolower(flow_type)}"),
      y = NULL,
      fill = NULL
    ) +
    theme_trade()
}

prepare_overview_country_trade_snapshot <- function(data) {
  required_cols <- c(
    "year", "ref_area_code", "ref_area_name", "flow_type", "value_1000usd"
  )
  check_required_columns(data, required_cols, "overview_lac_country_category_trade")

  snapshot_year <- max(data$year, na.rm = TRUE)

  data |>
    dplyr::filter(.data$year == .env$snapshot_year) |>
    dplyr::group_by(year, ref_area_code, ref_area_name, flow_type) |>
    dplyr::summarise(
      value_1000usd = sum(value_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = flow_type,
      values_from = value_1000usd,
      values_fill = list(value_1000usd = 0)
    ) |>
    dplyr::mutate(
      ref_area_type = "country",
      exports_1000usd = tidyr::replace_na(Exportaciones, 0),
      imports_1000usd = tidyr::replace_na(Importaciones, 0),
      balance_1000usd = exports_1000usd - imports_1000usd,
      exports_musd = to_musd(exports_1000usd),
      imports_musd = to_musd(imports_1000usd),
      balance_musd = to_musd(balance_1000usd),
      total_trade_musd = exports_musd + imports_musd
    ) |>
    dplyr::select(
      year,
      ref_area_code,
      ref_area_name,
      ref_area_type,
      exports_1000usd,
      imports_1000usd,
      balance_1000usd,
      exports_musd,
      imports_musd,
      balance_musd,
      total_trade_musd
    ) |>
    dplyr::arrange(dplyr::desc(total_trade_musd), ref_area_name)
}

plot_overview_lac_country_trade_ranking <- function(
  data,
  max_countries = 15,
  interactive = TRUE
) {
  country_snapshot <- prepare_overview_country_trade_snapshot(data)
  year <- unique(country_snapshot$year)

  plot_lac_country_trade_ranking(
    country_snapshot,
    selected_hc_cat2 = "tecnologías sanitarias e insumos",
    year = year,
    max_countries = max_countries,
    interactive = interactive
  )
}

plot_lac_world_share_line <- function(
  data,
  selected_hc_cat2,
  max_share = 0.12,
  interactive = TRUE
) {
  required_cols <- c("year", "exp_region", "hc_cat2", "share_exports_value")
  check_required_columns(data, required_cols, "exports_region_hc_cat2")

  plot_data <- data |>
    dplyr::filter(
      .data$hc_cat2 == .env$selected_hc_cat2,
      .data$exp_region == "LAC"
    ) |>
    dplyr::arrange(year) |>
    dplyr::mutate(
      tooltip = make_tooltip(
        glue::glue("<b>Año:</b> {year}"),
        glue::glue("<b>Categoría:</b> {hc_cat2}"),
        glue::glue("<b>Participación LAC:</b> {format_percent_label(share_exports_value)}")
      )
    )

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = year,
      y = share_exports_value,
      group = 1,
      text = tooltip
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9, color = "#1B9E77") +
    ggplot2::geom_point(size = 1.5, color = "#1B9E77") +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 0.1, decimal.mark = ","),
      breaks = seq(0, max_share, by = 0.02),
      expand = ggplot2::expansion(mult = c(0, 0.03))
    ) +
    ggplot2::coord_cartesian(ylim = c(0, max_share)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      title = glue::glue("Participación de LAC en las exportaciones mundiales de {selected_hc_cat2}"),
      x = NULL,
      y = "Participación en exportaciones mundiales"
    ) +
    theme_trade()

  if (isTRUE(interactive)) {
    return(plotly::plot_ly(
      data = plot_data,
      x = ~year,
      y = ~share_exports_value,
      type = "scatter",
      mode = "lines+markers",
      text = ~tooltip,
      hovertemplate = "%{text}<extra></extra>",
      line = list(color = "#1B9E77", width = 2.5),
      marker = list(color = "#1B9E77", size = 7)
    ) |>
      plotly::layout(
        title = list(
          text = glue::glue(
            "Participación de LAC en las exportaciones mundiales de {selected_hc_cat2}")
        ),
        xaxis = list(
          title = "",
          tickmode = "array",
          tickvals = scales::pretty_breaks()(plot_data$year)
        ),
        yaxis = list(
          title = "Participación en exportaciones mundiales",
          range = c(0, max_share),
          tickformat = ".1%"
        ),
        margin = list(l = 70, r = 25, t = 85, b = 45)
      ))
  }

  p
}

plot_world_exports_region_structure <- function(
  data,
  selected_hc_cat2,
  interactive = TRUE
) {
  required_cols <- c("year", "exp_region", "hc_cat2", "exports_1000usd")
  check_required_columns(data, required_cols, "exports_region_hc_cat2")

  plot_data <- data |>
    dplyr::filter(.data$hc_cat2 == .env$selected_hc_cat2) |>
    dplyr::group_by(year, exp_region) |>
    dplyr::summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      exports_musd = to_musd(exports_1000usd),
      exp_region_longname = order_regions(exp_region),
      tooltip = make_tooltip(
        glue::glue("<b>Año:</b> {year}"),
        glue::glue("<b>Región exportadora:</b> {exp_region_longname}"),
        glue::glue("<b>Exportaciones:</b> {format_usd_millions(exports_1000usd)}")
      )
    )

  if (nrow(plot_data) == 0) {
    rlang::abort(glue::glue(
      "No hay exportaciones regionales para '{selected_hc_cat2}'."
    ))
  }

  region_order_by_exports <- plot_data |>
    dplyr::group_by(exp_region_longname) |>
    dplyr::summarise(
      total_exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(total_exports_1000usd) |>
    dplyr::pull(exp_region_longname) |>
    as.character()

  plot_data <- plot_data |>
    dplyr::mutate(
      exp_region_longname = factor(
        as.character(exp_region_longname),
        levels = region_order_by_exports
      )
    ) |>
    dplyr::arrange(exp_region_longname, year)

  plot_palette <- complete_region_palette(plot_data$exp_region_longname)
  y_max <- plot_data |>
    dplyr::group_by(year) |>
    dplyr::summarise(total_exports_musd = sum(exports_musd, na.rm = TRUE), .groups = "drop") |>
    dplyr::pull(total_exports_musd) |>
    max(na.rm = TRUE)
  if (!is.finite(y_max) || y_max == 0) {
    y_max <- 1
  }

  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = year,
      y = exports_musd,
      fill = exp_region_longname,
      group = exp_region_longname,
      text = tooltip
      )
    ) +
    ggplot2::geom_area(alpha = 0.9, color = NA, linewidth = 0) +
    ggplot2::scale_fill_manual(
      values = plot_palette,
      breaks = region_order_by_exports,
      drop = FALSE,
      name = "Región exportadora"
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        prefix = "US$ ",
        suffix = " M",
        big.mark = ".",
        decimal.mark = ","
      ),
      expand = ggplot2::expansion(mult = c(0, 0.03))
    ) +
    ggplot2::coord_cartesian(ylim = c(0, y_max * 1.03)) +
    ggplot2::labs(
      title = glue::glue("Exportaciones de {selected_hc_cat2}, por región exportadora"),
      x = NULL,
      y = "Exportaciones"
    ) +
    theme_trade()

  if (isTRUE(interactive)) {
    plot_widget <- plotly::plot_ly()

    for (region_name in region_order_by_exports) {
      region_data <- plot_data |>
        dplyr::filter(as.character(.data$exp_region_longname) == .env$region_name)

      plot_widget <- plot_widget |>
        plotly::add_trace(
          data = region_data,
          x = ~year,
          y = ~exports_musd,
          type = "scatter",
          mode = "lines",
          stackgroup = "exports",
          name = region_name,
          text = ~tooltip,
          hovertemplate = "%{text}<extra></extra>",
          fillcolor = unname(plot_palette[[region_name]]),
          line = list(width = 0)
        )
    }

    return(plot_widget |>
             plotly::layout(
               title = list(
                 text = glue::glue(
                   "Exportaciones de {selected_hc_cat2}, por región exportadora")
               ),
               xaxis = list(title = ""),
               yaxis = list(
                 title = "Exportaciones",
                 rangemode = "tozero"
               ),
               hovermode = "closest",
               legend = list(orientation = "h", x = 0, y = -0.2)
             ))
  }

  p
}

plot_lac_country_trade_ranking <- function(
  country_snapshot,
  selected_hc_cat2,
  year,
  max_countries = 15,
  interactive = TRUE
) {
  required_cols <- c(
    "ref_area_name", "exports_musd", "imports_musd",
    "balance_musd", "total_trade_musd"
  )
  check_required_columns(country_snapshot, required_cols, "country_snapshot")
  validate_country_snapshot(country_snapshot)

  top_countries <- country_snapshot |>
    dplyr::slice_head(n = max_countries) |>
    dplyr::mutate(country = forcats::fct_reorder(ref_area_name, total_trade_musd))

  bars_data <- top_countries |>
    dplyr::select(country, exports_musd, imports_musd) |>
    tidyr::pivot_longer(
      cols = c(exports_musd, imports_musd),
      names_to = "flow",
      values_to = "value_musd"
    ) |>
    dplyr::mutate(
      value_musd = dplyr::if_else(flow == "imports_musd", -value_musd, value_musd),
      flow = dplyr::recode(
        flow,
        exports_musd = "Exportaciones",
        imports_musd = "Importaciones"
      )
    )

  y_limit <- max(
    abs(bars_data$value_musd),
    abs(top_countries$balance_musd),
    na.rm = TRUE
  )
  if (!is.finite(y_limit) || y_limit == 0) {
    y_limit <- 1
  }

  p <- ggplot2::ggplot(
    bars_data,
    ggplot2::aes(x = country, y = value_musd, fill = flow)
  ) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.35) +
    ggplot2::geom_point(
      data = top_countries,
      ggplot2::aes(x = country, y = balance_musd),
      inherit.aes = FALSE,
      size = 2
    ) +
    ggplot2::coord_flip(ylim = c(-y_limit, y_limit)) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        prefix = "US$ ",
        suffix = " M",
        big.mark = ".",
        decimal.mark = ","
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c("Exportaciones" = "#1B9E77", "Importaciones" = "#D95F02"),
      name = NULL
    ) +
    ggplot2::labs(
      title = glue::glue("Principales países LAC por comercio de {selected_hc_cat2}, {year}"),
      subtitle = "Barras = exportaciones e importaciones; punto = balance comercial",
      x = NULL,
      y = "Millones de USD"
    ) +
    theme_trade()

  if (isTRUE(interactive)) {
    return(plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
             plotly::layout(legend = list(orientation = "h", x = 0, y = -0.15)))
  }

  p
}

render_country_results_table <- function(country_snapshot) {
  validate_country_snapshot(country_snapshot)

  country_table <- country_snapshot |>
    dplyr::transmute(
      pais = ref_area_name,
      exportaciones_musd = exports_musd,
      importaciones_musd = imports_musd,
      balance_musd = balance_musd,
      comercio_total_musd = total_trade_musd,
      razon_import_export = dplyr::if_else(
        exports_musd > 0,
        imports_musd / exports_musd,
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(comercio_total_musd))

  reactable::reactable(
    country_table,
    searchable = TRUE,
    filterable = TRUE,
    sortable = TRUE,
    pagination = TRUE,
    defaultPageSize = 12,
    defaultSorted = list(comercio_total_musd = "desc"),
    columns = list(
      pais = reactable::colDef(name = "País", minWidth = 180),
      exportaciones_musd = reactable::colDef(
        name = "Exportaciones",
        align = "right",
        format = reactable::colFormat(separators = TRUE, digits = 1)
      ),
      importaciones_musd = reactable::colDef(
        name = "Importaciones",
        align = "right",
        format = reactable::colFormat(separators = TRUE, digits = 1)
      ),
      balance_musd = reactable::colDef(
        name = "Balance",
        align = "right",
        format = reactable::colFormat(separators = TRUE, digits = 1)
      ),
      comercio_total_musd = reactable::colDef(
        name = "Comercio total",
        align = "right",
        format = reactable::colFormat(separators = TRUE, digits = 1)
      ),
      razon_import_export = reactable::colDef(
        name = "Importaciones / exportaciones",
        align = "right",
        format = reactable::colFormat(separators = TRUE, digits = 1)
      )
    ),
    theme = reactable::reactableTheme(
      borderColor = "#e5e7eb",
      stripedColor = "#f8fafc",
      highlightColor = "#f1f5f9",
      cellPadding = "8px 10px"
    )
  )
}

prepare_intra_lac_export_share <- function(
  trade_balance,
  sankey,
  year,
  selected_hc_cat2
) {
  check_required_columns(
    trade_balance,
    c("year", "ref_area_code", "ref_area_name", "ref_area_type", "exports_1000usd"),
    "trade_balance_lac"
  )
  check_required_columns(
    sankey,
    c("year", "source", "source_name", "target_name", "value_1000usd"),
    "sankey_intra_lac"
  )

  country_exports <- trade_balance |>
    dplyr::filter(
      .data$year == .env$year,
      .data$ref_area_type == "country"
    ) |>
    dplyr::group_by(ref_area_code, ref_area_name) |>
    dplyr::summarise(
      total_exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      .groups = "drop"
    )

  intra_lac_exports <- sankey |>
    dplyr::filter(
      .data$year == .env$year,
      .data$source != .data$target
    ) |>
    dplyr::group_by(source, source_name) |>
    dplyr::summarise(
      intra_lac_exports_1000usd = sum(value_1000usd, na.rm = TRUE),
      .groups = "drop"
    )

  main_destination <- sankey |>
    dplyr::filter(
      .data$year == .env$year,
      .data$source != .data$target
    ) |>
    dplyr::group_by(source, source_name, target_name) |>
    dplyr::summarise(
      destination_exports_1000usd = sum(value_1000usd, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(source, dplyr::desc(destination_exports_1000usd), target_name) |>
    dplyr::group_by(source, source_name) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      source,
      source_name,
      main_lac_destination = target_name,
      main_lac_destination_1000usd = destination_exports_1000usd
    )

  country_exports |>
    dplyr::left_join(
      intra_lac_exports,
      by = c("ref_area_code" = "source", "ref_area_name" = "source_name")
    ) |>
    dplyr::left_join(
      main_destination,
      by = c("ref_area_code" = "source", "ref_area_name" = "source_name")
    ) |>
    dplyr::mutate(
      intra_lac_exports_1000usd = tidyr::replace_na(intra_lac_exports_1000usd, 0),
      main_lac_destination_1000usd = tidyr::replace_na(main_lac_destination_1000usd, 0),
      intra_lac_exports_musd = intra_lac_exports_1000usd / 1000,
      main_lac_destination_musd = main_lac_destination_1000usd / 1000,
      total_exports_musd = total_exports_1000usd / 1000,
      intra_lac_export_share = dplyr::if_else(
        total_exports_1000usd > 0,
        intra_lac_exports_1000usd / total_exports_1000usd,
        NA_real_
      ),
      main_lac_destination_share = dplyr::if_else(
        total_exports_1000usd > 0,
        main_lac_destination_1000usd / total_exports_1000usd,
        NA_real_
      ),
      main_lac_destination = tidyr::replace_na(
        main_lac_destination,
        "Sin exportaciones intrarregionales"
      ),
      selected_hc_cat2 = .env$selected_hc_cat2,
      year = .env$year
    ) |>
    dplyr::arrange(dplyr::desc(intra_lac_export_share), dplyr::desc(total_exports_1000usd))
}

render_intra_lac_export_share_table <- function( # Esta es la de la seccion 6. Comercio intrarregional
  trade_balance,
  sankey,
  year,
  selected_hc_cat2
) {
  if (is.null(sankey) || nrow(sankey) == 0) {
    return(htmltools::div(
      class = "dashboard-card",
      "No hay flujos intrarregionales disponibles para construir esta tabla."
    ))
  }

  table_data <- prepare_intra_lac_export_share(
    trade_balance = trade_balance,
    sankey = sankey,
    year = year,
    selected_hc_cat2 = selected_hc_cat2
  ) |>
    dplyr::transmute(
      pais = ref_area_name,
      proporcion_exportaciones_lac = intra_lac_export_share,
      exportaciones_lac_musd = intra_lac_exports_musd,
      principal_destino_lac = main_lac_destination,
      proporcion_principal_destino_lac = main_lac_destination_share,
      exportaciones_principal_destino_lac_musd = main_lac_destination_musd,
      exportaciones_totales_musd = total_exports_musd
    )

  reactable::reactable(
    table_data,
    searchable = TRUE,
    filterable = TRUE,
    sortable = TRUE,
    pagination = TRUE,
    defaultPageSize = 12,
    defaultSorted = list(proporcion_exportaciones_lac = "desc"),
    columns = list(
      pais = reactable::colDef(name = "País", minWidth = 180),
      proporcion_exportaciones_lac = reactable::colDef(
        name = "% exportado hacia LAC",
        align = "right",
        format = reactable::colFormat(percent = TRUE, digits = 1)
      ),
      principal_destino_lac = reactable::colDef(
        name = "Principal destino dentro de LAC",
        minWidth = 220
      ),
      exportaciones_lac_musd = reactable::colDef(
        name = "Exportaciones a LAC, USD millones",
        align = "right",
        format = reactable::colFormat(digits = 1, separators = TRUE)
      ),
      exportaciones_totales_musd = reactable::colDef(
        name = "Exportaciones totales, USD millones",
        align = "right",
        format = reactable::colFormat(digits = 1, separators = TRUE),
        show = FALSE
      ),
      proporcion_principal_destino_lac = reactable::colDef(
        name = "% hacia principal destino",
        align = "right",
        format = reactable::colFormat(percent = TRUE, digits = 1)
      ),
      exportaciones_principal_destino_lac_musd = reactable::colDef(
        name = "Exportaciones al principal destino, USD millones",
        align = "right",
        format = reactable::colFormat(digits = 1, separators = TRUE),
        show = FALSE
      )
    ),
    theme = reactable::reactableTheme(
      borderColor = "#e5e7eb",
      stripedColor = "#f8fafc",
      highlightColor = "#f1f5f9",
      cellPadding = "8px 10px"
    )
  )
}

render_product_exports_by_country_table <- function(product_exports) {
  if (is.null(product_exports) || nrow(product_exports) == 0) {
    return(htmltools::div(
      class = "dashboard-card",
      "No hay base auxiliar de productos exportados por país para esta categoría."
    ))
  }

  if (!"rca_balassa" %in% names(product_exports)) {
    product_exports <- product_exports |>
      dplyr::mutate(rca_balassa = NA_real_)
  }

  table_data <- product_exports |>
    dplyr::mutate(
      description_full = dplyr::coalesce(
        stringr::str_squish(.data$description),
        stringr::str_squish(.data$description_short),
        "Sin descripción disponible"
      )
    ) |>
    dplyr::group_by(product, description_full, exp_country_name) |>
    dplyr::summarise(
      exports_1000usd = sum(exports_1000usd, na.rm = TRUE),
      exports_musd = exports_1000usd / 1000,
      rca_balassa = mean(rca_balassa, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      rca_balassa = dplyr::if_else(is.nan(rca_balassa), NA_real_, rca_balassa)
    ) |>
    dplyr::mutate(
      producto_hs07 = glue::glue("{product} — {description_full}") |>
        as.character()
    ) |>
    dplyr::arrange(product, dplyr::desc(exports_1000usd)) |>
    dplyr::group_by(producto_hs07) |>
    dplyr::mutate(
      share_product = exports_1000usd / sum(exports_1000usd, na.rm = TRUE)
    ) |>
    dplyr::filter(
      rca_balassa > 1 | exports_musd > 1
    ) |>
    dplyr::ungroup()

  reactable::reactable(
    table_data,
    searchable = TRUE,
    filterable = TRUE,
    sortable = TRUE,
    pagination = TRUE,
    defaultPageSize = 20,
    defaultSorted = list(exports_musd = "desc"),
    highlight = TRUE,
    striped = TRUE,
    bordered = TRUE,
    groupBy = "producto_hs07",
    columns = list(
      producto_hs07 = reactable::colDef(
        name = "Producto HS07",
        minWidth = 360
      ),
      product = reactable::colDef(name = "Código HS07", show = FALSE),
      description_full = reactable::colDef(name = "Descripción", show = FALSE),
      exp_country_name = reactable::colDef(name = "País exportador", minWidth = 180),
      exports_musd = reactable::colDef(
        name = "Exportaciones, USD millones",
        align = "right",
        format = reactable::colFormat(digits = 1, separators = TRUE, prefix = "$")
      ),
      exports_1000usd = reactable::colDef(
        name = "Exportaciones, miles USD",
        align = "right",
        format = reactable::colFormat(digits = 0, separators = TRUE, prefix = "$"),
        show = FALSE
      ),
      share_product = reactable::colDef(
        name = "% de la exportación regional",
        align = "right",
        format = reactable::colFormat(percent = TRUE, digits = 1)
      ),
      rca_balassa = reactable::colDef(
        name = "RCA",
        align = "right",
        format = reactable::colFormat(digits = 2, separators = TRUE)
      )
    ),
    theme = reactable::reactableTheme(
      borderColor = "#e5e7eb",
      stripedColor = "#f8fafc",
      highlightColor = "#f1f5f9",
      cellPadding = "8px 10px"
    )
  )
}

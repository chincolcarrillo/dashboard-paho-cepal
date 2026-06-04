#  ----------- FUNCIONES AUXILIARES Y DE VISUALIZACION PARA DASHBOARD -----------

# 0. SET-UP ----

# cargar paquetes
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse,
       plotly,
       scales,
       glue,
       htmlwidgets,
       networkD3,
       forcats,
       stringr,
       rlang)

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

# Orden de regiones para mantener consistencia entre graficos.
region_order <- c(
  "LAC",
  "North America",
  "Europe & Central Asia",
  "East Asia & Pacific",
  "South Asia",
  "MENA",
  "Sub-Saharan Africa",
  "Unclassified"
)

# Etiquetas largas de region. Se usan cuando las bases solo traen codigos cortos.
region_labels <- c(
  "LAC" = "Latin America & Caribbean",
  "North America" = "North America",
  "Europe & Central Asia" = "Europe & Central Asia",
  "East Asia & Pacific" = "East Asia & Pacific",
  "South Asia" = "South Asia",
  "MENA" = "Middle East & North Africa",
  "Sub-Saharan Africa" = "Sub-Saharan Africa",
  "Unclassified" = "Unclassified"
)

# Paleta de colores para regiones
region_palette <- c(
  "Latin America & Caribbean" = "#1b9e77",
  "North America" = "#7570b3",
  "Europe & Central Asia" = "#d95f02",
  "East Asia & Pacific" = "#e7298a",
  "South Asia" = "#66a61e",
  "Sub-Saharan Africa" = "#e6ab02",
  "Middle East & North Africa" = "#a6761d",
  "Middle East, North Africa, Afghanistan & Pakistan" = "#a6761d",
  "Unclassified" = "#999999"
)

flow_labels <- c(
  "exports" = "Exportaciones",
  "imports" = "Importaciones",
  "Exports" = "Exportaciones",
  "Imports" = "Importaciones",
  "export" = "Exportaciones",
  "import" = "Importaciones"
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
      dplyr::filter(hc_cat2 == selected_hc_cat2)
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
    dplyr::filter(ref_area_code == selected_area)

  if (!is.null(selected_hc_cat2)) {
    base_data <- base_data |>
      dplyr::filter(hc_cat2 == selected_hc_cat2)
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
      subtitle = glue::glue("Categoría: {selected_label}"),
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
    dplyr::filter(ref_area_code == selected_area)

  if (!is.null(selected_hc_cat2)) {
    plot_data <- plot_data |>
      dplyr::filter(hc_cat2 == selected_hc_cat2)
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
        flow_type == "Exports",
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

  p <- plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = flow_type,
        y = share_flow_value,
        fill = partner_region_longname,
        text = tooltip
      )
    ) +
    ggplot2::geom_col(width = 0.68, color = "white", linewidth = 0.25) +
    scale_fill_region(name = "Región contraparte") +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::labs(
      title = glue::glue("Composición regional del comercio: {unique(plot_data$ref_area_name)}"),
      subtitle = glue::glue("Categoría: {selected_label}"),
      x = NULL,
      y = "Participación del flujo total",
      caption = "Nota: en exportaciones la región contraparte corresponde al destino; en importaciones corresponde al origen."
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
    dplyr::filter(year == selected_year)

  if (!is.null(selected_hc_cat2)) {
    links_raw <- links_raw |>
      dplyr::filter(hc_cat2 == selected_hc_cat2)
  }

  links_raw <- links_raw |>
    dplyr::filter(keep_self_flows | source != target) |>
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
    dplyr::filter(value_1000usd >= min_value_1000usd) |>
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
  sankey_data <- prepare_sankey_data(
    data = data,
    selected_year = selected_year,
    selected_hc_cat2 = selected_hc_cat2,
    min_value_1000usd = min_value_1000usd,
    top_n_flows = top_n_flows
  )

  networkD3::sankeyNetwork(
    Links = sankey_data$links,
    Nodes = sankey_data$nodes,
    Source = "source_id",
    Target = "target_id",
    Value = "value",
    NodeID = "name",
    units = "miles de USD",
    fontSize = 12,
    nodeWidth = 24,
    sinksRight = FALSE,
    height = height,
    width = width
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

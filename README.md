# Dashboard PAHO/CEPAL

Dashboard estático construido con R y Quarto sobre comercio internacional de tecnologías sanitarias en América Latina y el Caribe.

Demo: <https://chincolcarrillo.github.io/dashboard-paho-cepal/dashboard/>

## Fuentes

- CEPII-BACI HS 2007 V202601, años 2007–2024.
- Clasificación de productos: `data/rev_paho_2026.xlsx`, hoja `exportable`.
- Clasificación regional: `data/country-class.xlsx`, con los ajustes documentados en `r/01_clean_baci.R`.

## Flujo de producción

El proyecto no instala paquetes durante el procesamiento ni durante el render. Prepare previamente el entorno requerido por los llamados `pacman::p_load()`.

1. Ejecute `r/01_clean_baci.R`. Este llama a `r/00_ingest_baci.R`, convierte únicamente los CSV sin una partición Parquet existente y reconstruye los agregados de `data/dashboard/`.
2. Revise las validaciones: la ejecución se detiene ante categorías inesperadas, clasificaciones ausentes o participaciones inválidas.
3. Revise `r/02_functions_plots.R` si necesita modificar temas, formatos o visualizaciones compartidas.
4. Renderice el proyecto Quarto. Las páginas permanecen estáticas; no se utiliza Shiny.

Los archivos de `data/dashboard/` son los insumos preferidos de las páginas. No es necesario cargar los CSV BACI durante el render.

## Organización del dashboard

Existe una página por cada categoría normativa de `hc_cat2`. Todas incluyen la misma estructura base de análisis y se construyen desde `dashboard/_category-body.qmd`. Los archivos de categoría solo fijan parámetros y contenido específico.

Europa y Asia Central se mantienen como regiones separadas en procesamiento, escalas, colores y leyendas.

## Actualización de categorías

Para modificar la clasificación:

1. Actualice la hoja `exportable` de `data/rev_paho_2026.xlsx`.
2. Verifique que los valores de `hc_cat2` coincidan con `hc_cat2_levels` en `r/00_config.R`.
3. Reconstruya personalmente las bases con `r/01_clean_baci.R`.
4. Renderice personalmente el dashboard.

No agregue etiquetas alternativas directamente en los `.qmd`.

## Validación y entorno reproducible

Después de reconstruir las bases, puede ejecutar personalmente `tests/testthat.R`. Las pruebas comprueban la taxonomía, la separación regional, las claves, los balances y las participaciones.

El repositorio evita un `renv.lock` generado artificialmente. Para fijar las versiones reales del entorno, inicialice `renv` personalmente y cree el lockfile desde la instalación con la que efectivamente procesa y renderiza el proyecto.

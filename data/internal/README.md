# Bases para consulta interna

Esta carpeta contiene artefactos derivados para análisis exploratorios del equipo. No forman parte de `dashboard_files` ni de las páginas públicas enlazadas en la navegación; algunos pueden alimentar páginas Quarto ocultas de revisión interna.

## `emerging_product_country_pairs_15y.rds`

- Se reconstruye al ejecutar `r/01_clean_baci.R`.
- Grano: año de corte × categoría `hc_cat2` × producto HS07 × país exportador ALC.
- Incluye pares que cumplen en el último año disponible `RCA > 1` o exportaciones superiores a US$ 1 millón, pero que quince años antes no cumplían ninguno de los dos criterios.
- Conserva la identificación del par hasta `exp_country_name`, seguida de `exports_cutoff_1000usd`, `rca_cutoff`, `reference_year`, `exports_change_1000usd`, `exports_ratio_vs_reference` y `rca_change`.
- Cuando el par no tenía exportaciones registradas en el año de referencia, el cambio de exportaciones se calcula desde cero y las razones que requieren una base histórica quedan como `NA`.
- Es sólo para consulta interna y no se carga ni publica en el dashboard.

## `product_trade_balance_lac_2024.rds`

- Se reconstruye al ejecutar `r/01_clean_baci.R`.
- Grano: año × categoría `hc_cat2` × producto HS07.
- Contiene exportaciones, importaciones y balance comercial agregado de ALC para 2024.
- Se usa como insumo intermedio de `r/04_complexity_indicators.R`.

## `pci_tec_sanitaria.rds`

- Se reconstruye al ejecutar `r/04_complexity_indicators.R`, después de `r/01_clean_baci.R`.
- Combina la clasificación de productos sanitarios, PCI 2022–2024 y comercio regional de 2024.
- Alimenta exclusivamente la página oculta `dashboard/complejidad-productos.qmd`.

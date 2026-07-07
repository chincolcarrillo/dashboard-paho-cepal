# Contratos de datos del dashboard

Los archivos de esta carpeta son artefactos derivados. Se reconstruyen con `r/01_clean_baci.R` y no deben editarse manualmente.

## Convenciones

- `year`: año calendario.
- `hc_cat2`: uno de los siete valores normativos definidos en `r/00_config.R` y en la hoja `exportable` de `data/rev_paho_2026.xlsx`.
- Variables terminadas en `_1000usd`: miles de dólares corrientes.
- `LAC`: Latin America & Caribbean.
- `Europe` y `Central Asia`: regiones separadas.
- `No clasificada`: etiqueta explícita para países o contrapartes sin región WB asignada.
- `flow_type`: `Exports` o `Imports` en las bases procesadas; las traducciones pertenecen a la capa de presentación.

## Archivos generales

### `exports_region_hc_cat2.rds`

- Grano: año × región exportadora × categoría.
- Clave: `year`, `exp_region`, `hc_cat2`.
- Invariante: `share_exports_value` suma uno por año y categoría.

### `trade_balance_lac.rds`

- Grano: año × área de referencia × tipo de área × categoría.
- Clave: `year`, `ref_area_code`, `ref_area_type`, `hc_cat2`.
- Invariante: `balance_1000usd = exports_1000usd - imports_1000usd`.

### `partner_region_lac_2024.rds`

- Grano: año × área de referencia × tipo de área × categoría × flujo × región socia.
- Clave: `year`, `ref_area_code`, `ref_area_type`, `hc_cat2`, `flow_type`, `partner_region`.
- Invariante: `share_flow_value` suma uno dentro de cada área, categoría y flujo.
- El año se define mediante `partner_year` en `r/00_config.R`; el nombre histórico del archivo se conserva para compatibilidad.

### `sankey_intra_lac.rds`

- Grano: año × categoría × país exportador × país importador.
- Clave: `year`, `hc_cat2`, `source`, `target`.
- Cobertura temporal: `sankey_years` en `r/00_config.R`.

## Bases auxiliares

### `product_exports_lac_2024_by_country.rds`

- Grano: año × categoría × código HS07 × país exportador LAC.
- Clave: `year`, `hc_cat2`, `product`, `exporter`.
- Uso: tabla común de productos exportados por país de origen en cada página de categoría.
- Nota: aunque el nombre histórico conserva `2024`, el script lo construye con el último año disponible en BACI.

Las demás bases auxiliares a nivel HS6 no forman parte del contrato común de las siete páginas. Pueden usarse en módulos especializados, siempre que su categoría y año se validen antes de renderizar.

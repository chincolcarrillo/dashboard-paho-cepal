# Notas metodológicas - Plots y tablas del dashboard

## 1. Fuente y unidades de los datos

- Fuente: BACI/CEPII, con clasificación de categorías de producto propia (`hc_cat2`, en "data/rev_paho_2026.xlsx").
- **Unidad base de todas las variables de valor: miles de USD** (`_1000usd`). Las funciones de formato convierten sobre esa base:
  - `format_usd_thousands()`: mantiene la unidad (miles de USD).
  - `format_usd_millions()` / `to_musd()`: `x / 1.000` -> millones de USD.
  - `format_usd_billions()`: `x / 1.000.000` -> miles de millones de USD.
  - `format_percent_label()`: formatea proporciones (0–1) como porcentaje.
- Formato: separador de miles `.`, decimal `,` (español).
- Los niveles y colores de región (`region_levels`, `region_long_labels`, `region_colors`) y de categoría (`hc_cat2_levels`) se definen en `r/00_config.R`.

## 2. Bases de entrada esperadas

| Base (`.rds`) | Uso principal |
|---|---|
| `exports_region_hc_cat2` | Exportaciones mundiales por año, región exportadora y categoría |
| `trade_balance_lac` | Exportaciones, importaciones y balance por año, área (país o LAC) y categoría |
| `partner_region_lac_2024` | Comercio por región contraparte (destino en exportaciones, origen en importaciones) |
| `sankey_intra_lac` | Flujos bilaterales de exportación entre países de LAC |
| `overview_exports_trends` | Series Mundo/LAC por grupo de producto, para la página de resumen |
| `overview_lac_country_category_trade` | Comercio por país de LAC y categoría, para la página de resumen |
| `product_exports_lac_2024_by_country` | Exportaciones por producto (HS07) y país, con RCA ya calculado |

`check_required_columns()` valida que cada base tenga las columnas necesarias antes de calcular nada, y `load_dashboard_data()` centraliza la lectura desde `data/dashboard/`.

## 3. Indicadores calculados

### 3.1 Participación en exportaciones mundiales (*market share*)
`share_exports_value = exports_1000usd / world_exports_1000usd`, agregado por año y región exportadora (y opcionalmente categoría). 
Se usa en el stacked area chart regional (`plot_exports_region_area`) y en la línea de participación de LAC (`plot_lac_world_share_line`).

### 3.2 Balance comercial
Por año, área y categoría: `balance_1000usd = exports_1000usd - imports_1000usd`. 
Se grafica como exportaciones (positivas) e importaciones (negativas) más una línea de balance neto (`plot_trade_balance`).

### 3.3 Razón importaciones/exportaciones
`razon_import_export = imports_musd / exports_musd` (si `exports_musd > 0`, si no `NA`). 
Se calcula en la tabla de resultados por país (`render_country_results_table`).

### 3.4 Composición regional del comercio (contraparte)
Para cada año/área/flujo (exportación o importación): `share_flow_value = value_1000usd / sum(value_1000usd)` dentro de ese grupo, donde la región contraparte es el **destino** en exportaciones y el **origen** en importaciones. 
Alimenta las barras apiladas al 100 % (`plot_partner_region_100pct`).

### 3.5 Comercio intrarregional LAC (Sankey)
`prepare_sankey_data()` agrega `value_1000usd` por par origen–destino dentro de LAC para un año y categoría dados. 
Por defecto excluye flujos donde origen = destino (`keep_self_flows = FALSE`) y permite aplicar un umbral mínimo (`min_value_1000usd`) y quedarse solo con los `top_n_flows` de mayor valor.

### 3.6 Participación de las exportaciones hacia LAC por país
Para cada país de LAC:
- `intra_lac_export_share = exportaciones intrarregionales / exportaciones totales del país`
- **Principal destino LAC**: el país socio con mayor valor de exportación intrarregional, con su participación (`main_lac_destination_share`) sobre el total exportado por el país.

Ambos se calculan en `prepare_intra_lac_export_share()`, cruzando `trade_balance_lac` (total exportado) con `sankey_intra_lac` (flujos bilaterales).

### 3.7 CAGR (tasa de crecimiento anual compuesta)
Fórmula general (`calculate_cagr()`): 

```
CAGR = (valor_final / valor_inicial)^(1 / n_años) - 1
```

Devuelve `NA` si el valor inicial es ≤ 0 o si `n_años ≤ 0`. 
Se usa en dos ventanas para los KPI de la página de inicio (`prepare_landing_category_kpis()`), sobre exportaciones mundiales por categoría:
- **CAGR histórico completo**: desde el primer año disponible hasta el último.
- **CAGR de los últimos 5 años** (`cagr_window`, configurable, default = 5).

### 3.8 Crecimiento interanual / a n años (series de tendencia)
En `plot_overview_exports_growth_trends()`, para cada serie (`line_label` = ámbito × grupo de producto):

```
growth_rate = exports_1000usd(t) / exports_1000usd(t - lag_years) - 1
```

Si `annualized = TRUE` y `lag_years > 1`, el resultado se anualiza: `(1 + growth_rate)^(1/lag_years) - 1`. Con `lag_years = 1` (default) es simplemente el crecimiento interanual.

### 3.9 Distribución del comercio de un país entre categorías
Dentro de cada país y flujo (exportación/importación): `share_value = value_1000usd / total_value_1000usd_del_país`. 
Muestra cómo se compone el comercio de tecnologías sanitarias de cada país por categoría (`plot_overview_lac_country_category_share`).

### 3.10 Índice de Ventaja Comparativa Revelada (RCA de Balassa)
**Se calcula en el script 03_trade_indicators.R** 
La columna `rca_balassa` llega ya calculada desde la base `product_exports_lac_2024_by_country` (ver script de preparación de datos / cálculo del índice de Balassa). Este archivo solo la consume:
- En `render_product_exports_by_country_table()` **se promedia (`mean`) si hay múltiples registros por producto/país [quizás sacar???]**, y la tabla se filtra a productos con `rca_balassa > 1` **o** exportaciones `> US$ 1 millón`, para priorizar productos relevantes.

### 3.11 KPI de la página de inicio, por categoría
`prepare_landing_category_kpis()` combina, por categoría (`hc_cat2`) y último año disponible:
- Comercio total global (exportaciones mundiales de la categoría).
- CAGR histórico y CAGR a 5 años (ver 3.7).
- Exportaciones e importaciones de LAC en el último año.

Las categorías se agrupan en tres grupos temáticos (`landing_kpi_group`): dispositivos médicos, insumos/ingredientes farmacéuticos activos, y "otras tecnologías sanitarias" (resto), cada uno con un color asociado para las tarjetas de KPI.

## 4. Notas generales

- Todas las funciones de gráfico aceptan `interactive = TRUE/FALSE`: `TRUE` devuelve un widget `plotly`/`networkD3`; `FALSE` devuelve el objeto `ggplot` estático equivalente (mismo cálculo subyacente).
- `check_required_columns()` se llama al inicio de casi toda función que recibe una base externa: si falta una columna requerida, la función se detiene con un mensaje explícito en vez de fallar más adelante de forma críptica.
- Los `NA` en tasas de crecimiento y en participaciones (`share_*`) ocurren cuando el denominador es 0, negativo o no disponible; se excluyen o se muestran como "n/d" según el contexto (`format_landing_kpi_value`).

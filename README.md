# Spatio-temporal and food-exposure drivers of foodborne disease in Chile, 2011–2024: a nationwide analysis

**Kasim Allel, Maricel Vidal, José Conejeros, Rodrigo Aguilar, Felipe Contreras, Constanza Diaz-Gavidia, Dácil Rivera**

---

## Objective

To characterise the spatio-temporal distribution of confirmed foodborne disease (FBD) outbreaks across Chile’s 16 administrative regions between 2011 and 2024, identify key food-source and consumption-setting drivers, and quantify the association between climatic exposures (temperature, precipitation, and derived indicators) and FBD measures at the commune level (including commune–epidemiological week panels).

---

## Data sources

| Source | Variables | Coverage |
|--------|-----------|----------|
| DEIS – Ministry of Health (MINSAL) | Confirmed FBD outbreaks: dates, commune, exposed persons and cases, hospitalisations, deaths, food source, consumption setting, aetiological agent and group | 2011–2024, national |
| INE | Intercensal population projections by commune and year (`estimaciones-y-proyecciones-2002-2035-comunas.xlsx`) | 2011–2024 |
| ERA5-Land (ECMWF) via **Google Earth Engine** | Daily mean, min, max 2 m temperature (°C); daily sum/min/max of total precipitation (m, converted in processing); zonal means/sums by commune polygon—see `2.2 ERA5_daily_temp_precip_communes.py` | 2011–2024, native grid ~9–11 km; exported to commune-day CSVs |

**Inputs expected in the repo (not versioned if large or restricted):** `2-Data/Input/Brotes_ETA_2011_2024.xlsx` (FBD), `2-Data/Input/estimaciones-y-proyecciones-2002-2035-comunas.xlsx` (population). GEE exports land in `2-Data/Input/GEE_ClimData/*.csv` after running the Python extractor.

---

## Data processing pipeline (this repository)

Scripts are run from the project root. Order:

| Step | Script | Role |
|------|--------|------|
| Base | `1-Scripts/1.1 Settings.R` … `1.4 Mapping_vars.R` | Paths, package load (`1.2`), helpers (`1.3`), mapping variable definitions (`1.4`) |
| 2.1 | `1-Scripts/2.1 Process_FD_data.R` | Clean outbreak records; build commune/region population objects; **outputs:** `2-Data/Output/FD_2011_2024.RData`, `Population_municipality_2011_2024.RData`, `Population_region_2011_2024.RData`, and related objects |
| 2.2 | `1-Scripts/2.2 ERA5_daily_temp_precip_communes.py` | Authenticate to Earth Engine, export commune geometries (from RData) to GeoJSON if needed, run daily ERA5-Land zonal stats, export **per-year** tables to Google Drive; download those CSVs into `2-Data/Input/GEE_ClimData/` |
| 2.3 | `1-Scripts/2.3 Process_ERA5_data.R` | Read all yearly CSVs, `bind_rows`, drop communes with systematic NA in climate; **output:** `2-Data/Output/ERA5_climate_2011_2024.RData` |
| 2.4 | `1-Scripts/2.4 Join_Data.R` | Build commune × day grid; join outbreaks, population, and ERA5; optional zones; **one row per commune–day** for climate and **disease** indicators; global **p90 / p95 / p99** thresholds for **temperature** and **precipitation** (day counts and binary “≥1 day” flags); aggregate **longitudinal** panels (year, semester, month, epiweek) and **weekly** panel; **commune–year** incidence: exposed population per 100,000 (`expoused / pop_mun * 1e5`); **outputs:** `Daily_*`, `Longitudinal_*`, `Weekly_*` in `2-Data/Output/` |

**Output formats (2.4):** in addition to `.RData`, compact copies use **`qs`** (`.qs`, `preset = "archive"`) and **`saveRDS(…, compress = "xz")`** (`.rds`). Use `qs::qread()` or `readRDS()` to load. Longitudinal `.qs` / `.rds` store a **named list** (`data_year`, `data_semester`, `data_month`, `data_w`).

**Python / GEE (2.2):** set `GEE_PROJECT` and optionally `EE_EXPORT_FOLDER`; requires `earthengine-api`, `geopandas`, and a valid Earth Engine project with Drive export enabled.

---

## Methodological strategy (analysis plan)

### 1. Data preparation (implemented in 2.1–2.4)
Confirmed outbreaks are cleaned and linked to daily ERA5 fields and INE population. A balanced **commune × calendar day** grid includes zero-outbreak days where applicable; time stratifiers (year, semester, month, epiweek) and **pooled** notification-style summaries feed longitudinal and weekly **sf** objects. Categorical event indicators (e.g. food, place, diagnosis type/group) are expanded for aggregation. **Continental** analysis typically excludes communes with unreliable extraction (e.g. Antarctica, insular or edge cases) as defined in 2.3.

### 2. Descriptive analysis
Temporal trends, seasonal views (e.g. STL), and maps of FBD rates; food-source and setting associations across time, region, and context (e.g. χ² with adjusted residuals where appropriate; population weighting or stratification as in the analysis plan).

### 3. Climate–FBD association models
Generalised Additive Models (GAM) with count families and log(population) offset, **Distributed Lag Non-linear Models (DLNM)** for non-linear and lagged effects, and sensitivity on lag length and model family. The exposure suite may include **weekly** (or **period-aggregated**) temperature and precipitation and/or composite indices (e.g. Climate Stress Index) aligned to the same space–time support as the FBD panel.

### 4. Sensitivity analyses
Alternatives: Poisson / zero-inflated specifications, lag windows (e.g. 0–6 weeks), exclusion of known outlier events or periods, and robustness of any composite climate index.

*Sections 2–5 are the substantive epidemiological plan; corresponding scripts, tables, and figures can live under `3-Output/` as they are added.*

---

## Repository structure

```
├── 1-Scripts/
│   ├── 1.1 Settings.R
│   ├── 1.2 Packages.R          # install/load (includes rio, sf, tsibble, qs, …)
│   ├── 1.3 Functions.R
│   ├── 1.4 Mapping_vars.R
│   ├── 2.1 Process_FD_data.R   # outbreaks + population → Output/*.RData
│   ├── 2.2 ERA5_daily_temp_precip_communes.py
│   ├── 2.3 Process_ERA5_data.R
│   └── 2.4 Join_Data.R         # daily + longitudinal + weekly panels; compressed exports
├── 2-Data/
│   ├── Input/                  # raw or intermediate (Excel, GEE CSVs, …)
│   └── Output/                 # processed .RData / .rds / .qs (large files may be gitignored)
├── 3-Output/                  # model results, tables, figures (to be populated)
│   └── Models/
├── LICENSE
└── README.md
```

---

## Requirements

- **R** ≥ 4.3 — core packages are listed in `1-Scripts/1.2 Packages.R` (e.g. `dplyr`, `sf`, `lubridate`, `tsibble`, `chilemapas`, `rio`, `qs`, …; modelling stack such as `mgcv`, `dlnm` when you add analysis scripts)
- **Python** ≥ 3.10 — for GEE: `earthengine-api`, `geopandas` (and dependencies for the extractor in `2.2`)
- **SaTScan** v10 (free licence) — for scan statistics: [https://www.satscan.org](https://www.satscan.org)
- **Google Earth Engine** account and a cloud project with the Earth Engine API enabled, for `2.2`

---

## Funding and data access

*Funded by Universidad de las Américas. Raw outbreak data are available from DEIS–MINSAL and ACHIPIA public portals where policy allows. Reanalysis climate fields come from **ERA5-Land** (Copernicus) via **Google Earth Engine**; DMC data can be used for validation. Do not share identifiable patient or facility-level records outside approved agreements.*

# 2.4 Join data -----

# Load settings
source("1-Scripts/1.1 Settings.R")
source("1-Scripts/1.2 Packages.R")
source("1-Scripts/1.3 Functions.R")
source("1-Scripts/1.4 Mapping_vars.R")

# Paths 
output <- "2-Data/Output/"

############################################################################################################################/
# 2.4.1 Load Data ----------------------------------------------------------------
############################################################################################################################/

fd <- rio::import(paste0(output, "FD_2011_2024", ".RData")) |> 
  select(id, notification_date, consumption_date, cod_reg:mun, expoused:group_diagnosis)

population_mun <- rio::import(paste0(output, "Population_municipality_2011_2024", ".RData")) |> 
  select(-name_com) |> 
  rename(pop_mun = pop)

population_reg <- rio::import(paste0(output, "Population_region_2011_2024", ".RData")) |> 
  select(-c(region, geometry)) |> 
  rename(pop_reg = pop)

era5 <- rio::import(paste0(output, "ERA5_climate_2011_2024", ".RData")) |>
  dplyr::select(!name_com) |>
  dplyr::mutate(date = lubridate::as_date(date))

############################################################################################################################/
# 2.4.2 Daily Data ----------------------------------------------------------------
############################################################################################################################/

time <- seq(as.Date("2011-01-01"), as.Date("2024-12-31"), by = "days")
mun <- unique(population_mun$comuna)

# Grid with complete information -------
daily_data <- expand.grid(cod_mun = mun, consumption_date = time) 

daily_data <- daily_data |> # 1.769.444
  as_tibble() |> 
  mutate(
    year = year(consumption_date), 
    semester=as_semester(consumption_date), 
    trimester=as.yearqtr(consumption_date),
    month_num = month(consumption_date),
    week_num = week(consumption_date),
    day_num = day(consumption_date),
    day = consumption_date,
    epiweek = tsibble::yearweek(consumption_date),
    epiweek = lubridate::as_date(epiweek),
    epimonth= tsibble::yearmonth(consumption_date),
    epimonth = lubridate::as_date(epimonth)
  )

daily_data <- daily_data |> 
  left_join(
    population_mun, 
    by = c("cod_mun"="comuna", "year")
          ) 

ref_reg <- chilemapas::codigos_territoriales |> 
  select(codigo_comuna, codigo_region) |> 
  mutate_all(as.numeric) 

glimpse(daily_data) # 1.769.444

daily_data <- daily_data |> 
  left_join(ref_reg, by = c("cod_mun" = "codigo_comuna")) |> 
  left_join(population_reg, by = c("codigo_region" = "cod_reg", "year")) 

glimpse(daily_data) # 1.769.444

# Join data in the grid with complete information -------
# We will have NA for the days and municipalities without cases, which we will replace with 0 later.
local_ref <- fd |> 
  select(cod_reg, cod_mun, reg, mun) |> 
  distinct()

data <- daily_data |> 
  left_join(fd, by = c("codigo_region" = "cod_reg", "cod_mun", "consumption_date")) |> # 1.770.084
  mutate(
    notification_date = ifelse(is.na(notification_date), consumption_date, notification_date),
    expoused = ifelse(is.na(expoused), 0, expoused),
    sick = ifelse(is.na(sick), 0, sick),
    dead = ifelse(is.na(dead), 0, dead),
    hosp = ifelse(is.na(hosp), 0, hosp),
    n_sick_1_14 = ifelse(is.na(n_sick_1_14), 0, n_sick_1_14),
    n_sick_15_44 = ifelse(is.na(n_sick_15_44), 0, n_sick_15_44),
    n_sick_45_64 = ifelse(is.na(n_sick_45_64), 0, n_sick_45_64), 
    n_sick_65 = ifelse(is.na(n_sick_65), 0, n_sick_65)
  ) |> 
  select(-c(reg, mun)) |> 
  left_join(
    local_ref, 
    by = c("codigo_region" = "cod_reg", "cod_mun")
  )

data <- data |>
  left_join(era5, by = c("cod_mun" = "mun", "consumption_date" = "date"))

glimpse(data)

# Generate climate zones based on regions ------

data <- data |>
  mutate(
    zone = case_when(
      reg %in% c("Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo") ~ "North",
      reg %in% c("Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo OHiggins", "Maule", "Nuble", "Biobio") ~ "Center",
      reg %in% c("La Araucania", "Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo", "Magallanes y de la Antartica Chilena") ~ "South",
      TRUE ~ NA_character_
    ),
    zone = factor(zone, levels = c("North", "Center", "South"))
  ) |> 
# Climate zones 
  mutate(clim_zone = 
    case_when(
      reg %in% c("Arica y Parinacota") ~ "Desert hot",
      reg %in% c("Tarapaca", "Antofagasta", "Atacama", "Coquimbo") ~ "Desert cold",
      reg %in% c("Metropolitana de Santiago", "Libertador General Bernardo OHiggins", "Maule") ~ "Temperate dry, hot summer",
      reg %in% c("Valparaiso", "Nuble", "Biobio", "La Araucania") ~ "Temperate dry, warm summer",
      reg %in% c("Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo") ~ "Temperate, no dry season",
      reg %in% c("Magallanes y de la Antartica Chilena") ~ "Cold steppe",
      TRUE ~ NA_character_
    )) |>
  mutate(clim_zone=factor(clim_zone, 
                          levels=c("Desert hot", "Desert cold", "Temperate dry, hot summer", "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe")
  )) 

glimpse(data)
summary(data)

data <- data |> 
  drop_na(zone, temperature_2m)

## Arrange and save -------

data <- data |>
  select(
    id,
    consumption_date,
    notification_date,
    # Variables to construct with consumption_date
    year,
    semester,
    trimester,
    month_num,
    week_num,
    day_num,
    day,
    epiweek,
    epimonth,
    cod_mun,
    mun,
    codigo_region,
    region_code,
    reg,
    zone,
    clim_zone,
    pop_mun,
    pop_reg,
    geometry,
    temperature_2m,
    temperature_2m_min,
    temperature_2m_max,
    total_precipitation_sum,
    total_precipitation_min,
    total_precipitation_max,
    expoused,
    sick,
    dead,
    hosp,
    n_sick_1_14,
    n_sick_15_44,
    n_sick_45_64,
    n_sick_65,
    source_food,
    source_place,
    type_diagnosis,
    group_diagnosis
  ) |> 
  rename(
    cod_reg = codigo_region,
    abr_reg = region_code,
  )

glimpse(data)

# .RData: legacy. .qs (zstd) and .rds (xz) are usually smaller: qs::qread() / readRDS()
save(data, file = paste0(output, "Daily_FD_CLIM_2011_2024", ".RData"))

############################################################################################################################/
# 2.4.3 Longitudinal panels (year, semester, month, epiweek) -------------------------
############################################################################################################################/

geo_base <- c("zone", "reg", "cod_reg", "abr_reg", "cod_mun", "mun")
clim_temp_vars <- c("temperature_2m", "temperature_2m_min", "temperature_2m_max")
clim_precip_vars <- c(
  "total_precipitation_sum", "total_precipitation_min", "total_precipitation_max"
)
clim_vars <- c(clim_temp_vars, clim_precip_vars)
# Daily climate anomalies (mean deviation vs commune × ISO week climatology, 2011–2024)
clim_dev_vars <- c("t_des_media", "t_des_min", "t_des_max", "p_des")
epi_sum_vars <- c(
  "expoused", "sick", "dead", "hosp",
  "n_sick_1_14", "n_sick_15_44", "n_sick_45_64", "n_sick_65"
)
time_key_cols <- c(
  "year", "semester", "trimester", "month_num", "week_num",
  "day_num", "day", "epiweek", "epimonth", "consumption_date"
)

# One row per municipality for geometry (back-join after aggregates)
geom_mun <- data |>
  dplyr::distinct(cod_mun, geometry)

d_nosf <- if (inherits(data, "sf")) {
  sf::st_drop_geometry(data)
} else if ("geometry" %in% names(data)) {
  dplyr::select(data, -geometry)
} else {
  data
}

# Categorical levels: 1 per event row; levels + dummies: 1.3 Functions.R
d_nosf <- add_fbd_event_dummies(d_nosf, remove_source_vars = TRUE)

ind_epi_cat_vars <- fbd_ind_epi_cat_var_names(d_nosf)

# One row per municipality and day (climate); no duplicate days: thresholds and temperature means
d_clim_day <- d_nosf |>
  dplyr::arrange(cod_mun, consumption_date) |>
  dplyr::group_by(cod_mun, consumption_date) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(
    dplyr::all_of(geo_base),
    dplyr::any_of(time_key_cols),
    dplyr::all_of(clim_vars),
    pop_mun,
    pop_reg
  )

# Commune–year–ISO week stats for CSI (inter-annual mean/SD of weekly values, 2011–2024)
wk_annual <- d_clim_day |>
  dplyr::mutate(
    iso_w = lubridate::isoweek(consumption_date),
    yr = lubridate::year(consumption_date)
  ) |>
  dplyr::group_by(cod_mun, yr, iso_w) |>
  dplyr::summarise(
    t_mean_wk = mean(temperature_2m, na.rm = TRUE),
    t_max_wk = mean(temperature_2m_max, na.rm = TRUE),
    p_wk = sum(total_precipitation_sum, na.rm = TRUE),
    .groups = "drop"
  )
wk_csi_baseline <- wk_annual |>
  dplyr::group_by(cod_mun, iso_w) |>
  dplyr::summarise(
    csi_t_media_b = mean(t_mean_wk, na.rm = TRUE),
    csi_t_media_sd = stats::sd(t_mean_wk, na.rm = TRUE),
    csi_t_max_b = mean(t_max_wk, na.rm = TRUE),
    csi_t_max_sd = stats::sd(t_max_wk, na.rm = TRUE),
    csi_precip_b = mean(p_wk, na.rm = TRUE),
    csi_precip_sd = stats::sd(p_wk, na.rm = TRUE),
    .groups = "drop"
  )

# Per-commune daily quantiles (full 2011–2024; same distribution for threshold day flags)
q_mun <- d_clim_day |>
  dplyr::group_by(cod_mun) |>
  dplyr::summarise(
    t2m_q90 = stats::quantile(temperature_2m, 0.9, na.rm = TRUE, type = 7, names = FALSE),
    t2m_q95 = stats::quantile(temperature_2m, 0.95, na.rm = TRUE, type = 7, names = FALSE),
    t2m_q99 = stats::quantile(temperature_2m, 0.99, na.rm = TRUE, type = 7, names = FALSE),
    tmin_q90 = stats::quantile(temperature_2m_min, 0.9, na.rm = TRUE, type = 7, names = FALSE),
    tmin_q95 = stats::quantile(temperature_2m_min, 0.95, na.rm = TRUE, type = 7, names = FALSE),
    tmin_q99 = stats::quantile(temperature_2m_min, 0.99, na.rm = TRUE, type = 7, names = FALSE),
    tmax_q90 = stats::quantile(temperature_2m_max, 0.9, na.rm = TRUE, type = 7, names = FALSE),
    tmax_q95 = stats::quantile(temperature_2m_max, 0.95, na.rm = TRUE, type = 7, names = FALSE),
    tmax_q99 = stats::quantile(temperature_2m_max, 0.99, na.rm = TRUE, type = 7, names = FALSE),
    psum_q90 = stats::quantile(total_precipitation_sum, 0.9, na.rm = TRUE, type = 7, names = FALSE),
    psum_q95 = stats::quantile(total_precipitation_sum, 0.95, na.rm = TRUE, type = 7, names = FALSE),
    psum_q99 = stats::quantile(total_precipitation_sum, 0.99, na.rm = TRUE, type = 7, names = FALSE),
    pmin_q90 = stats::quantile(total_precipitation_min, 0.9, na.rm = TRUE, type = 7, names = FALSE),
    pmin_q95 = stats::quantile(total_precipitation_min, 0.95, na.rm = TRUE, type = 7, names = FALSE),
    pmin_q99 = stats::quantile(total_precipitation_min, 0.99, na.rm = TRUE, type = 7, names = FALSE),
    pmax_q90 = stats::quantile(total_precipitation_max, 0.9, na.rm = TRUE, type = 7, names = FALSE),
    pmax_q95 = stats::quantile(total_precipitation_max, 0.95, na.rm = TRUE, type = 7, names = FALSE),
    pmax_q99 = stats::quantile(total_precipitation_max, 0.99, na.rm = TRUE, type = 7, names = FALSE),
    .groups = "drop"
  )

# Commune × ISO week climatology: daily means and SDs (for anomalies t_des_*, p_des)
baseline_mun_w <- d_clim_day |>
  dplyr::mutate(iso_w = lubridate::isoweek(consumption_date)) |>
  dplyr::group_by(cod_mun, iso_w) |>
  dplyr::summarise(
    t_media_b = mean(temperature_2m, na.rm = TRUE),
    t_min_b = mean(temperature_2m_min, na.rm = TRUE),
    t_max_b = mean(temperature_2m_max, na.rm = TRUE),
    precip_b = mean(total_precipitation_sum, na.rm = TRUE),
    t_media_sd = stats::sd(temperature_2m, na.rm = TRUE),
    t_min_sd = stats::sd(temperature_2m_min, na.rm = TRUE),
    t_max_sd = stats::sd(temperature_2m_max, na.rm = TRUE),
    precip_sd = stats::sd(total_precipitation_sum, na.rm = TRUE),
    .groups = "drop"
  )

# Day > commune-specific threshold (1 = one day); then daily anomalies vs clim. same week
d_clim_day <- d_clim_day |>
  dplyr::mutate(iso_w = lubridate::isoweek(consumption_date)) |>
  dplyr::left_join(baseline_mun_w, by = c("cod_mun", "iso_w")) |>
  dplyr::left_join(q_mun, by = "cod_mun") |>
  dplyr::mutate(
    t_des_media = temperature_2m - t_media_b,
    t_des_min = temperature_2m_min - t_min_b,
    t_des_max = temperature_2m_max - t_max_b,
    p_des = total_precipitation_sum - precip_b,
    d_t2m_p90 = as.integer(
      !is.na(temperature_2m) & !is.na(t2m_q90) & temperature_2m > t2m_q90
    ),
    d_t2m_p95 = as.integer(
      !is.na(temperature_2m) & !is.na(t2m_q95) & temperature_2m > t2m_q95
    ),
    d_t2m_p99 = as.integer(
      !is.na(temperature_2m) & !is.na(t2m_q99) & temperature_2m > t2m_q99
    ),
    d_tmin_p90 = as.integer(
      !is.na(temperature_2m_min) & !is.na(tmin_q90) & temperature_2m_min > tmin_q90
    ),
    d_tmin_p95 = as.integer(
      !is.na(temperature_2m_min) & !is.na(tmin_q95) & temperature_2m_min > tmin_q95
    ),
    d_tmin_p99 = as.integer(
      !is.na(temperature_2m_min) & !is.na(tmin_q99) & temperature_2m_min > tmin_q99
    ),
    d_tmax_p90 = as.integer(
      !is.na(temperature_2m_max) & !is.na(tmax_q90) & temperature_2m_max > tmax_q90
    ),
    d_tmax_p95 = as.integer(
      !is.na(temperature_2m_max) & !is.na(tmax_q95) & temperature_2m_max > tmax_q95
    ),
    d_tmax_p99 = as.integer(
      !is.na(temperature_2m_max) & !is.na(tmax_q99) & temperature_2m_max > tmax_q99
    ),
    d_psum_p90 = as.integer(
      !is.na(total_precipitation_sum) & !is.na(psum_q90) & total_precipitation_sum > psum_q90
    ),
    d_psum_p95 = as.integer(
      !is.na(total_precipitation_sum) & !is.na(psum_q95) & total_precipitation_sum > psum_q95
    ),
    d_psum_p99 = as.integer(
      !is.na(total_precipitation_sum) & !is.na(psum_q99) & total_precipitation_sum > psum_q99
    ),
    d_pmin_p90 = as.integer(
      !is.na(total_precipitation_min) & !is.na(pmin_q90) & total_precipitation_min > pmin_q90
    ),
    d_pmin_p95 = as.integer(
      !is.na(total_precipitation_min) & !is.na(pmin_q95) & total_precipitation_min > pmin_q95
    ),
    d_pmin_p99 = as.integer(
      !is.na(total_precipitation_min) & !is.na(pmin_q99) & total_precipitation_min > pmin_q99
    ),
    d_pmax_p90 = as.integer(
      !is.na(total_precipitation_max) & !is.na(pmax_q90) & total_precipitation_max > pmax_q90
    ),
    d_pmax_p95 = as.integer(
      !is.na(total_precipitation_max) & !is.na(pmax_q95) & total_precipitation_max > pmax_q95
    ),
    d_pmax_p99 = as.integer(
      !is.na(total_precipitation_max) & !is.na(pmax_q99) & total_precipitation_max > pmax_q99
    )
  ) |>
  dplyr::select(
    -dplyr::all_of(
      c(
        "t_media_b", "t_min_b", "t_max_b", "precip_b", "t_media_sd", "t_min_sd", "t_max_sd",
        "precip_sd", "iso_w",
        "t2m_q90", "t2m_q95", "t2m_q99", "tmin_q90", "tmin_q95", "tmin_q99", "tmax_q90", "tmax_q95", "tmax_q99",
        "psum_q90", "psum_q95", "psum_q99", "pmin_q90", "pmin_q95", "pmin_q99", "pmax_q90", "pmax_q95", "pmax_q99"
      )
    )
  )

## Panel: climate (1 row per municipality–day) + epidemiology (all event rows)
aggregate_panel <- function(d_ep, d_clim, gvars) {
  g_clim <- d_clim |>
    dplyr::group_by(dplyr::across(dplyr::all_of(gvars))) |>
    dplyr::summarise(
      n_clim_days = dplyr::n(),
      dplyr::across(
        dplyr::all_of(clim_temp_vars),
        ~ mean(.x, na.rm = TRUE)
      ),
      dplyr::across(
        dplyr::all_of(clim_precip_vars),
        ~ sum(.x, na.rm = TRUE)
      ),
      n_t2m_p90 = sum(d_t2m_p90, na.rm = TRUE),
      n_t2m_p95 = sum(d_t2m_p95, na.rm = TRUE),
      n_t2m_p99 = sum(d_t2m_p99, na.rm = TRUE),
      n_tmin_p90 = sum(d_tmin_p90, na.rm = TRUE),
      n_tmin_p95 = sum(d_tmin_p95, na.rm = TRUE),
      n_tmin_p99 = sum(d_tmin_p99, na.rm = TRUE),
      n_tmax_p90 = sum(d_tmax_p90, na.rm = TRUE),
      n_tmax_p95 = sum(d_tmax_p95, na.rm = TRUE),
      n_tmax_p99 = sum(d_tmax_p99, na.rm = TRUE),
      n_psum_p90 = sum(d_psum_p90, na.rm = TRUE),
      n_psum_p95 = sum(d_psum_p95, na.rm = TRUE),
      n_psum_p99 = sum(d_psum_p99, na.rm = TRUE),
      n_pmin_p90 = sum(d_pmin_p90, na.rm = TRUE),
      n_pmin_p95 = sum(d_pmin_p95, na.rm = TRUE),
      n_pmin_p99 = sum(d_pmin_p99, na.rm = TRUE),
      n_pmax_p90 = sum(d_pmax_p90, na.rm = TRUE),
      n_pmax_p95 = sum(d_pmax_p95, na.rm = TRUE),
      n_pmax_p99 = sum(d_pmax_p99, na.rm = TRUE),
      t_des_media = mean(t_des_media, na.rm = TRUE),
      t_des_min = mean(t_des_min, na.rm = TRUE),
      t_des_max = mean(t_des_max, na.rm = TRUE),
      p_des = mean(p_des, na.rm = TRUE),
      pop_mun = dplyr::first(pop_mun),
      pop_reg = dplyr::first(pop_reg),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      t2m_p90 = as.integer(n_t2m_p90 > 0L), t2m_p95 = as.integer(n_t2m_p95 > 0L),
      t2m_p99 = as.integer(n_t2m_p99 > 0L),
      tmin_p90 = as.integer(n_tmin_p90 > 0L), tmin_p95 = as.integer(n_tmin_p95 > 0L),
      tmin_p99 = as.integer(n_tmin_p99 > 0L),
      tmax_p90 = as.integer(n_tmax_p90 > 0L), tmax_p95 = as.integer(n_tmax_p95 > 0L),
      tmax_p99 = as.integer(n_tmax_p99 > 0L),
      psum_p90 = as.integer(n_psum_p90 > 0L), psum_p95 = as.integer(n_psum_p95 > 0L),
      psum_p99 = as.integer(n_psum_p99 > 0L),
      pmin_p90 = as.integer(n_pmin_p90 > 0L), pmin_p95 = as.integer(n_pmin_p95 > 0L),
      pmin_p99 = as.integer(n_pmin_p99 > 0L),
      pmax_p90 = as.integer(n_pmax_p90 > 0L), pmax_p95 = as.integer(n_pmax_p95 > 0L),
      pmax_p99 = as.integer(n_pmax_p99 > 0L)
    )

  g_ep <- d_ep |>
    dplyr::group_by(dplyr::across(dplyr::all_of(gvars))) |>
    dplyr::summarise(
      n_events = dplyr::n(),
      dplyr::across(
        dplyr::all_of(c(epi_sum_vars, ind_epi_cat_vars)),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  thr_n <- c(
    "n_t2m_p90", "n_t2m_p95", "n_t2m_p99",
    "n_tmin_p90", "n_tmin_p95", "n_tmin_p99",
    "n_tmax_p90", "n_tmax_p95", "n_tmax_p99",
    "n_psum_p90", "n_psum_p95", "n_psum_p99",
    "n_pmin_p90", "n_pmin_p95", "n_pmin_p99",
    "n_pmax_p90", "n_pmax_p95", "n_pmax_p99"
  )
  thr_b <- c(
    "t2m_p90", "t2m_p95", "t2m_p99",
    "tmin_p90", "tmin_p95", "tmin_p99",
    "tmax_p90", "tmax_p95", "tmax_p99",
    "psum_p90", "psum_p95", "psum_p99",
    "pmin_p90", "pmin_p95", "pmin_p99",
    "pmax_p90", "pmax_p95", "pmax_p99"
  )

  # Climate as left table: one row per space–time unit in d_clim
  out <- dplyr::left_join(g_clim, g_ep, by = gvars) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c(
          epi_sum_vars, ind_epi_cat_vars,
          "n_events", "n_clim_days"
        )),
        ~ dplyr::coalesce(.x, 0)
      ),
      dplyr::across(
        dplyr::any_of(c(thr_n, thr_b)),
        ~ dplyr::coalesce(.x, 0L)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(clim_temp_vars),
        ~ dplyr::if_else(is.nan(.x), NA_real_, .x)
      ),
      dplyr::across(
        dplyr::all_of(clim_precip_vars),
        ~ dplyr::if_else(is.nan(.x), NA_real_, .x)
      ),
      dplyr::across(
        dplyr::all_of(clim_dev_vars),
        ~ dplyr::if_else(is.nan(.x), NA_real_, .x)
      )
    )
  out
}

## Climate zones (by region) ---------------------------------------------------
mutate_clim_zone <- function(df) {
  df |>
    dplyr::mutate(
      clim_zone = dplyr::case_when(
        reg %in% c("Arica y Parinacota") ~ "Desert hot",
        reg %in% c("Tarapaca", "Antofagasta", "Atacama", "Coquimbo") ~ "Desert cold",
        reg %in% c("Metropolitana de Santiago", "Libertador General Bernardo OHiggins", "Maule") ~ "Temperate dry, hot summer",
        reg %in% c("Valparaiso", "Nuble", "Biobio", "La Araucania") ~ "Temperate dry, warm summer",
        reg %in% c("Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo") ~ "Temperate, no dry season",
        reg %in% c("Magallanes y de la Antartica Chilena") ~ "Cold steppe",
        TRUE ~ NA_character_
      ),
      clim_zone = factor(
        clim_zone,
        levels = c(
          "Desert hot", "Desert cold", "Temperate dry, hot summer",
          "Temperate dry, warm summer", "Temperate, no dry season", "Cold steppe"
        )
      )
    )
}

## By year ----------
g_year <- c(geo_base, "year")
data_year <- aggregate_panel(d_nosf, d_clim_day, g_year) |>
  mutate_clim_zone() |>
  dplyr::mutate(
    incidence_exposed_100k = dplyr::if_else(
      !is.na(pop_mun) & pop_mun > 0,
      expoused / pop_mun * 100000,
      NA_real_
    )
  ) |>
  dplyr::left_join(geom_mun, by = "cod_mun") |>
  sf::st_as_sf()
glimpse(data_year)

## By semester ---------
g_sem <- c(geo_base, "year", "semester")
data_semester <- aggregate_panel(d_nosf, d_clim_day, g_sem) |>
  mutate_clim_zone() |>
  dplyr::left_join(geom_mun, by = "cod_mun") |>
  sf::st_as_sf()
glimpse(data_semester)

## By calendar month -----------
g_month <- c(geo_base, "year", "month_num")
data_month <- aggregate_panel(d_nosf, d_clim_day, g_month) |>
  dplyr::mutate(month_date = lubridate::make_date(year, month_num, 1L)) |>
  mutate_clim_zone() |>
  dplyr::left_join(geom_mun, by = "cod_mun") |>
  sf::st_as_sf()
glimpse(data_month)

## By epidemiological week (epiweek, ISO week starting Monday) + rates + CSI ------
g_week <- c(geo_base, "epiweek")
data_w <- aggregate_panel(d_nosf, d_clim_day, g_week) |>
  dplyr::mutate(iso_w = lubridate::isoweek(epiweek)) |>
  dplyr::left_join(wk_csi_baseline, by = c("cod_mun", "iso_w")) |>
  dplyr::mutate(
    delta_t_media = temperature_2m - csi_t_media_b,
    delta_t_max = temperature_2m_max - csi_t_max_b,
    delta_precip = total_precipitation_sum - csi_precip_b,
    z_t_media = dplyr::if_else(
      !is.na(csi_t_media_sd) & csi_t_media_sd > 0,
      delta_t_media / csi_t_media_sd,
      NA_real_
    ),
    z_t_max = dplyr::if_else(
      !is.na(csi_t_max_sd) & csi_t_max_sd > 0,
      delta_t_max / csi_t_max_sd,
      NA_real_
    ),
    # z_precip > 0 = more rain than normal; stress index uses (-z_precip)
    z_precip = dplyr::if_else(
      !is.na(csi_precip_sd) & csi_precip_sd > 0,
      delta_precip / csi_precip_sd,
      NA_real_
    ),
    CSI_v1 = z_t_media + (-z_precip),
    CSI_v2 = z_t_max + (-z_precip),
    CSI_v3 = ((z_t_media + z_t_max) / 2) + (-z_precip),
    rate_atack = dplyr::if_else(expoused > 0, sick / expoused, NA_real_),
    rate_dead = dplyr::if_else(expoused > 0, dead / expoused, NA_real_),
    rate_hosp = dplyr::if_else(expoused > 0, hosp / expoused, NA_real_)
  ) |>
  mutate_clim_zone() |>
  dplyr::left_join(geom_mun, by = "cod_mun") |>
  sf::st_as_sf()

glimpse(data_w)


############################################################################################################################/
# 2.4.4 Save Data ----------------------------------------------------------------
############################################################################################################################/

# Longitudinal: four objects in one file; .qs / .rds = named list (same element names)
longitudinal_bundle <- list(
  data_year = data_year,
  data_semester = data_semester,
  data_month = data_month,
  data_w = data_w
)

save(
  data_year,
  data_semester,
  data_month,
  data_w,
  file = paste0(output, "Longitudinal_FD_CLIM_2011_2024", ".RData")
)

saveRDS(
  longitudinal_bundle,
  file = paste0(output, "Longitudinal_FD_CLIM_2011_2024", ".rds"),
  compress = "xz"
)

save(data_w, file = paste0(output, "Weekly_FD_CLIM_2011_2024", ".RData"))

saveRDS(
  data_w,
  file = paste0(output, "Weekly_FD_CLIM_2011_2024", ".rds"),
  compress = "xz"
)


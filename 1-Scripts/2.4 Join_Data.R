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
  select(!name_com) |> 
  mutate(date = as.Date(date))

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

# Categorical levels: 1 per event row; multiple events on the same day are summed in aggregation
lev_source_food <- c(
  "Eggs", "Meat and meat products", "Milk and milk products", "Not determined",
  "Other food", "Ready to eat", "Seafood", "Water"
)
lev_source_place <- c(
  "Not determined", "Place not authorized or only for sale",
  "Place of production and/or consumption", "Place of self-consumption"
)
lev_type_diagnosis <- c(
  "Bacillus cereus", "Bacterium not classified", "Campylobacter spp.",
  "Chemical toxins", "Clostridium spp.", "Escherichia coli", "Listeria spp.",
  "Parasites", "Salmonella spp.", "Seafood toxins", "Shigella spp.",
  "Staphylococcus aureus", "Vibrio spp.", "Viruses", "Yersinia spp."
)
lev_group_diagnosis <- c("Bacteria", "Chemical agents", "Parasites", "Viruses")

add_event_indicators_by_levels <- function(df, col, ref_levels, prefix) {
  if (!col %in% names(df)) {
    return(df)
  }
  levs <- unique(c(ref_levels, as.character(na.omit(df[[col]]))))
  for (a in levs) {
    nm <- paste0(prefix, "_", janitor::make_clean_names(a))
    df[[nm]] <- as.integer(!is.na(df[[col]]) & df[[col]] == a)
  }
  df
}

d_nosf <- d_nosf |>
  add_event_indicators_by_levels("source_food", lev_source_food, "food") |>
  add_event_indicators_by_levels("source_place", lev_source_place, "place") |>
  add_event_indicators_by_levels("type_diagnosis", lev_type_diagnosis, "type") |>
  add_event_indicators_by_levels("group_diagnosis", lev_group_diagnosis, "group") |>
  dplyr::select(-dplyr::all_of(
    c("source_food", "source_place", "type_diagnosis", "group_diagnosis")
  ))

ind_epi_cat_vars <- grep(
  "^(food|place|type|group)_",
  names(d_nosf),
  value = TRUE
)

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

# Global percentiles: full series, unique municipality–date daily values
q2m <- stats::quantile(d_clim_day$temperature_2m, c(0.9, 0.95, 0.99), na.rm = TRUE, type = 7)
q2mmin <- stats::quantile(
  d_clim_day$temperature_2m_min, c(0.9, 0.95, 0.99), na.rm = TRUE, type = 7
)
q2mmax <- stats::quantile(
  d_clim_day$temperature_2m_max, c(0.9, 0.95, 0.99), na.rm = TRUE, type = 7
)

qpsum <- stats::quantile(
  d_clim_day$total_precipitation_sum, c(0.9, 0.95, 0.99), na.rm = TRUE, type = 7
)
qpmin <- stats::quantile(
  d_clim_day$total_precipitation_min, c(0.9, 0.95, 0.99), na.rm = TRUE, type = 7
)
qpmax <- stats::quantile(
  d_clim_day$total_precipitation_max, c(0.9, 0.95, 0.99), na.rm = TRUE, type = 7
)

# Day exceeds threshold (1 = one day); series: tmean, tmin, tmax; then psum, pmin, pmax
d_clim_day <- d_clim_day |>
  dplyr::mutate(
    d_t2m_p90 = as.integer(
      !is.na(temperature_2m) & !is.na(q2m[[1L]]) & temperature_2m > as.numeric(q2m[[1L]])
    ),
    d_t2m_p95 = as.integer(
      !is.na(temperature_2m) & !is.na(q2m[[2L]]) & temperature_2m > as.numeric(q2m[[2L]])
    ),
    d_t2m_p99 = as.integer(
      !is.na(temperature_2m) & !is.na(q2m[[3L]]) & temperature_2m > as.numeric(q2m[[3L]])
    ),
    d_tmin_p90 = as.integer(
      !is.na(temperature_2m_min) & !is.na(q2mmin[[1L]]) & temperature_2m_min > as.numeric(q2mmin[[1L]])
    ),
    d_tmin_p95 = as.integer(
      !is.na(temperature_2m_min) & !is.na(q2mmin[[2L]]) & temperature_2m_min > as.numeric(q2mmin[[2L]])
    ),
    d_tmin_p99 = as.integer(
      !is.na(temperature_2m_min) & !is.na(q2mmin[[3L]]) & temperature_2m_min > as.numeric(q2mmin[[3L]])
    ),
    d_tmax_p90 = as.integer(
      !is.na(temperature_2m_max) & !is.na(q2mmax[[1L]]) & temperature_2m_max > as.numeric(q2mmax[[1L]])
    ),
    d_tmax_p95 = as.integer(
      !is.na(temperature_2m_max) & !is.na(q2mmax[[2L]]) & temperature_2m_max > as.numeric(q2mmax[[2L]])
    ),
    d_tmax_p99 = as.integer(
      !is.na(temperature_2m_max) & !is.na(q2mmax[[3L]]) & temperature_2m_max > as.numeric(q2mmax[[3L]])
    ),
    d_psum_p90 = as.integer(
      !is.na(total_precipitation_sum) & !is.na(qpsum[[1L]]) & total_precipitation_sum > as.numeric(qpsum[[1L]])
    ),
    d_psum_p95 = as.integer(
      !is.na(total_precipitation_sum) & !is.na(qpsum[[2L]]) & total_precipitation_sum > as.numeric(qpsum[[2L]])
    ),
    d_psum_p99 = as.integer(
      !is.na(total_precipitation_sum) & !is.na(qpsum[[3L]]) & total_precipitation_sum > as.numeric(qpsum[[3L]])
    ),
    d_pmin_p90 = as.integer(
      !is.na(total_precipitation_min) & !is.na(qpmin[[1L]]) & total_precipitation_min > as.numeric(qpmin[[1L]])
    ),
    d_pmin_p95 = as.integer(
      !is.na(total_precipitation_min) & !is.na(qpmin[[2L]]) & total_precipitation_min > as.numeric(qpmin[[2L]])
    ),
    d_pmin_p99 = as.integer(
      !is.na(total_precipitation_min) & !is.na(qpmin[[3L]]) & total_precipitation_min > as.numeric(qpmin[[3L]])
    ),
    d_pmax_p90 = as.integer(
      !is.na(total_precipitation_max) & !is.na(qpmax[[1L]]) & total_precipitation_max > as.numeric(qpmax[[1L]])
    ),
    d_pmax_p95 = as.integer(
      !is.na(total_precipitation_max) & !is.na(qpmax[[2L]]) & total_precipitation_max > as.numeric(qpmax[[2L]])
    ),
    d_pmax_p99 = as.integer(
      !is.na(total_precipitation_max) & !is.na(qpmax[[3L]]) & total_precipitation_max > as.numeric(qpmax[[3L]])
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

## By epidemiological week (epiweek, ISO week starting Monday) + rates ------
g_week <- c(geo_base, "epiweek")
data_w <- aggregate_panel(d_nosf, d_clim_day, g_week) |>
  dplyr::mutate(
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


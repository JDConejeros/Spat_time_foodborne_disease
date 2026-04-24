# 2.1 Process Foodborne diseases data -----

# Load settings
source("1-Scripts/1.1 Settings.R")
source("1-Scripts/1.2 Packages.R")
source("1-Scripts/1.3 Functions.R")
source("1-Scripts/1.4 Mapping_vars.R")

# Paths 
input <- "2-Data/Input/"
output <- "2-Data/Output/"

############################################################################################################################/
# 2.1.1 Population Data ----------------------------------------------------------------
############################################################################################################################/

labs_mes <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
labs_reg <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", 
              "Valparaíso", "Metropolitana de Santiago", "Libertador General Bernardo O'Higgins", "Maule", "Ñuble",
              "Biobío", "La Araucanía", "Los Ríos", "Los Lagos",  "Aysén del General Carlos Ibáñez del Campo", 
              "Magallanes y de la Antártica Chilena")

# Regional data --------

# Population data to estimate incidence rates
pop <- rio::import(paste0(input, "estimaciones-y-proyecciones-2002-2035-comunas.xlsx")) |> clean_names()
glimpse(pop)
unique(pop$nombre_region)

# Calculate regional population by summing over municipalities, then reshape to long format
pop_reg <- pop |> 
  group_by(nombre_region) |> 
  summarise(
    pop_2011=sum(poblacion_2011, na.rm = TRUE),
    pop_2012=sum(poblacion_2012, na.rm = TRUE),
    pop_2013=sum(poblacion_2013, na.rm = TRUE),
    pop_2014=sum(poblacion_2014, na.rm = TRUE),
    pop_2015=sum(poblacion_2015, na.rm = TRUE),
    pop_2016=sum(poblacion_2016, na.rm = TRUE),
    pop_2017=sum(poblacion_2017, na.rm = TRUE),
    pop_2018=sum(poblacion_2018, na.rm = TRUE),
    pop_2019=sum(poblacion_2019, na.rm = TRUE),
    pop_2020=sum(poblacion_2020, na.rm = TRUE),
    pop_2021=sum(poblacion_2021, na.rm = TRUE),
    pop_2022=sum(poblacion_2022, na.rm = TRUE),
    pop_2023=sum(poblacion_2023, na.rm = TRUE),
    pop_2024=sum(poblacion_2024, na.rm = TRUE),
  ) |> 
  ungroup() |> 
  mutate(region=factor(
    nombre_region, 
    levels=labs_reg,
    labels=labs_reg)) |> 
  relocate(region) |> 
  select(-nombre_region) |> 
  pivot_longer(
    cols = !region, 
    names_to = "ano_estadistico",
    names_prefix = "pop_",
    values_to = "pop") |> 
  mutate(ano_estadistico=as.numeric(ano_estadistico)) |> 
  rename(year = ano_estadistico) 

region_codes <- data.frame(
  region = c(
    "Arica y Parinacota", 
    "Tarapacá", 
    "Antofagasta", 
    "Atacama", 
    "Coquimbo", 
    "Valparaíso", 
    "Metropolitana de Santiago", 
    "Libertador General Bernardo O'Higgins", 
    "Maule", 
    "Ñuble",
    "Biobío", 
    "La Araucanía", 
    "Los Ríos", 
    "Los Lagos", 
    "Aysén del General Carlos Ibáñez del Campo", 
    "Magallanes y de la Antártica Chilena"   
  
  ),
  region_code = c("AI",  # Arica y Parinacota
    "TA",  # Tarapacá
    "AN",  # Antofagasta
    "AP",  # Atacama
    "CO",  # Coquimbo
    "VA",  # Valparaíso
    "RM",  # Metropolitana de Santiago
    "OH",  # Libertador General Bernardo O'Higgins
    "MA",  # Maule
    "NB",  # Ñuble
    "BI",  # Biobío
    "AR",  # La Araucanía
    "LR",  # Los Ríos
    "LL",  # Los Lagos
    "AT",  # Aysén del General Carlos Ibáñez del Campo
    "MG")  # Magallanes y de la Antártica Chilena
)

pop_reg <- pop_reg |> 
  left_join(region_codes, by=c("region"="region")) |> 
  relocate(region_code, .after=region)

glimpse(pop_reg)

# Code region 
com <- chilemapas::codigos_territoriales |> 
  select(codigo_region, nombre_region) |> 
  distinct() |> 
  rename(cod_reg = codigo_region)

unique(pop_reg$region)

region_mapping <- data.frame(
  region = c("Antofagasta",                              
    "Arica y Parinacota",                       
    "Atacama",                                  
    "Aysén del General Carlos Ibáñez del Campo",
    "Biobío",                                   
    "Coquimbo",                                 
    "La Araucanía",                             
    "Libertador General Bernardo O'Higgins",    
    "Los Lagos",                                
    "Los Ríos",                                 
    "Magallanes y de la Antártica Chilena",     
    "Maule",                                    
    "Metropolitana de Santiago",                
    "Tarapacá",                                 
    "Valparaíso",                               
    "Ñuble"),
  
  region_adj = c("Antofagasta",                              
    "Arica y Parinacota",                       
    "Atacama",                                  
    "Aysen del General Carlos Ibanez del Campo",
    "Biobio",                                   
    "Coquimbo",                                 
    "La Araucania",                             
    "Libertador General Bernardo OHiggins",    
    "Los Lagos",                                
    "Los Rios",                                 
    "Magallanes y de la Antartica Chilena",     
    "Maule",                                    
    "Metropolitana de Santiago",                
    "Tarapaca",                                 
    "Valparaiso",                               
    "Nuble")
)

pop_reg <- pop_reg |> 
  left_join(region_mapping, by=c("region"="region")) |> 
  left_join(com, by=c("region_adj"="nombre_region")) |> 
  select(-region_adj) |> 
  relocate(cod_reg, .after=region) |> 
  mutate(cod_reg = as.numeric(cod_reg))

glimpse(pop_reg)

# Regional geometries (chilemapas, continental Chile only) ---------
# Exclude insular comunas: Isla de Pascua (Rapa Nui), Juan Fernández
insular_comunas <- c("05201", "05104")
mapa_comunas_continental <- chilemapas::mapa_comunas |>
  dplyr::filter(!codigo_comuna %in% insular_comunas)

geom_reg <- chilemapas::generar_regiones(mapa = mapa_comunas_continental) |>
  dplyr::mutate(cod_reg = as.integer(codigo_region)) |>
  dplyr::select(cod_reg, geometry)

pop_reg <- pop_reg |>
  dplyr::left_join(tibble::as_tibble(geom_reg), by = "cod_reg") |>
  sf::st_as_sf(crs = sf::st_crs(geom_reg))

# Visualize population by region for a specific year (e.g., 2024)
year_map <- 2024L
pop_reg |>
  dplyr::filter(year == year_map) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = log(pop)), color = "gray45", linewidth = 0.2) +
  ggplot2::scale_fill_viridis_c(
    name = "Log-Population",
    option = "C"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = paste("Population by region, continental Chile:", year_map))

save(pop_reg, file=paste0(output, "Population_region_2011_2024", ".RData"))

# Municipality data --------

# Calculate municipality population by summing over municipalities, then reshape to long format
pop_mun <- pop |> 
  select(comuna, poblacion_2011:poblacion_2024) |> 
  group_by(comuna) |> 
  summarise(
    pop_2011=sum(poblacion_2011, na.rm = TRUE),
    pop_2012=sum(poblacion_2012, na.rm = TRUE),
    pop_2013=sum(poblacion_2013, na.rm = TRUE),
    pop_2014=sum(poblacion_2014, na.rm = TRUE),
    pop_2015=sum(poblacion_2015, na.rm = TRUE),
    pop_2016=sum(poblacion_2016, na.rm = TRUE),
    pop_2017=sum(poblacion_2017, na.rm = TRUE),
    pop_2018=sum(poblacion_2018, na.rm = TRUE),
    pop_2019=sum(poblacion_2019, na.rm = TRUE),
    pop_2020=sum(poblacion_2020, na.rm = TRUE),
    pop_2021=sum(poblacion_2021, na.rm = TRUE),
    pop_2022=sum(poblacion_2022, na.rm = TRUE),
    pop_2023=sum(poblacion_2023, na.rm = TRUE),
    pop_2024=sum(poblacion_2024, na.rm = TRUE),
  ) |> 
  ungroup() |> 
  pivot_longer(cols = !comuna, 
    names_to = "ano_estadistico",
    names_prefix = "pop_",
    values_to = "pop") |> 
  mutate(ano_estadistico=as.numeric(ano_estadistico)) |> 
  rename(year = ano_estadistico) 

com <- chilemapas::codigos_territoriales |> 
  select(codigo_comuna, nombre_comuna) |>
  distinct() |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna))

pop_mun <- pop_mun |> 
  left_join(com, by=c("comuna"="codigo_comuna")) |> 
  rename(name_com = nombre_comuna) |> 
  relocate(name_com, .before=comuna)

glimpse(pop_mun)

# Municipality geometries (same continental comunas) ------
geom_mun <- mapa_comunas_continental |>
  dplyr::mutate(comuna = as.numeric(codigo_comuna)) |>
  dplyr::select(comuna, geometry)

pop_mun <- pop_mun |>
  dplyr::left_join(tibble::as_tibble(geom_mun), by = "comuna") |>
  sf::st_as_sf(crs = sf::st_crs(geom_mun))

year_map <- 2024L
pop_mun |>
  dplyr::filter(year == year_map) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = log(pop)), color = NA) +
  ggplot2::scale_fill_viridis_c(
    name = "Log-Population",
    option = "C"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = paste("Population by municipality, continental Chile: ", year_map))

save(pop_mun, file=paste0(output, "Population_municipality_2011_2024", ".RData"))

############################################################################################################################/
# 2.1.2 Foodborne Diseases Data ----------------------------------------------------------------
############################################################################################################################/

# Load data --------

fd <- rio::import(paste0(input, "Brotes_ETA_2011_2024.xlsx")) |> clean_names()
glimpse(fd)

# Edit variables --------

# Adjust municipality data 
com <- chilemapas::codigos_territoriales |> 
  distinct() |> 
  rename(cod_reg = codigo_region) |> 
  mutate(cod_mun = as.numeric(codigo_comuna)) |>
  mutate(cod_reg = as.numeric(cod_reg)) |> 
  rename(mun = nombre_comuna) |> 
  rename(reg = nombre_region) |> 
  select(cod_reg, reg, cod_mun, mun)

# FD data: 15255
fd <- fd |>
  left_join(comuna_mapping, by = c("comuna_de_consumo" = "comuna_eta")) |>
  left_join(com, by = c("comuna_com" = "mun")) |> 
  drop_na(cod_reg, cod_mun) # Loss data: 6 without spatial information 

# Adjust time vars: 15249
fd <- fd |> 
  filter(fecha_de_ingestion>= as.Date("2011-01-01") & fecha_de_ingestion <= as.Date("2024-12-31")) |>
  mutate(
    year = year(fecha_de_ingestion), 
    semester=as_semester(fecha_de_ingestion), 
    trimester=as.yearqtr(fecha_de_ingestion),
    month_num = month(fecha_de_ingestion),
    week_num = week(fecha_de_ingestion),
    day_num = day(fecha_de_ingestion),
    day = fecha_de_ingestion,
    epiweek = tsibble::yearweek(fecha_de_ingestion),
    epiweek = lubridate::as_date(epiweek),
    epimonth= tsibble::yearmonth(fecha_de_ingestion),
    epimonth = lubridate::as_date(epimonth)
    ) # Loss data: 165

# Adjust zone
fd <- fd |>
  mutate(
    zone = case_when(
      reg %in% c("Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo") ~ "North",
      reg %in% c("Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo OHiggins", "Maule", "Nuble", "Biobio") ~ "Center",
      reg %in% c("La Araucania", "Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo", "Magallanes y de la Antartica Chilena") ~ "South",
      TRUE ~ NA_character_
    ),
    zone = factor(zone, levels = c("North", "Center", "South"))
  )

# Climate zones 
fd <- fd |> 
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

# Outcomes: 15084 
fd <- fd |>
  mutate(
    expoused = expuestos,
    sick = enfermos,
    dead = fallecidos,
    hosp = total_de_hospitalizados, 
    rate_atack = sick/expoused,
    rate_dead = dead/expoused,
    rate_hosp = hosp/expoused,
    n_sick_1_14 = (total_enfermos_1 + total_enfermos_5_14),
    n_sick_15_44 = total_enfermos_15_44,
    n_sick_45_64 = total_enfermos_45_64,
    n_sick_65 = total_enfermos_65
  ) |> 
  filter(expoused <= 5000) |> #
  filter(sick <= 5000) |> # 7 cases with more than 5000 exposed, likely data entry errors. Exclude them to avoid biasing the analysis.
  filter(sick <= expoused) # 46 case with more sick than exposed, likely data entry error. Exclude it to avoid biasing the analysis.

# Filter data: 15031  
# Remove:  
# Otros resultados: 396
# Realizada, se descarta: 571
# NA: 832
# Realizada, en estudio: 70
fd <- fd |> 
  filter(conclusion_del_brote == "Realizada, se confirma") # Loss data: 1869 

# Remove duplicates: 13162 
fd<- fd |> 
  group_by(n_brote_eta) |> 
  mutate(dup = n()) |> 
  ungroup() |> 
  filter(dup==1) |> 
  select(-dup) # Loss data: 2

# Criteria of case definition: 13160
fd <- fd |>
  filter(!is.na(diagnostico_agrupado)) |> # 306 NA
  filter(!is.na(local_de_elaboracion)) |> # 172 NA
  filter(!is.na(grupo_alimento_sospechoso)) # 0 NA
  # Total Loss data: 399

# Final sample: 12761 cases with complete information on time, space, outcomes, and exposures.

# Adjust name dates 
fd <- fd |> 
  mutate(
    notification_date = as.Date(fecha_de_notificacion),
    consumption_date = as.Date(fecha_de_ingestion)
  )

# Adjust Source_food	Source_place	Type of diagnosis	Group diagnosis
sort(unique(fd$grupo_alimento_sospechoso))
sort(unique(fd$local_de_elaboracion))
sort(unique(fd$diagnostico_agrupado))

fd <- fd |>
  left_join(food_source_mapping, by = "grupo_alimento_sospechoso") |>
  mutate(
    # Create the new source_food variable
    source_food = case_when(
      !is.na(source_food) ~ source_food,
      TRUE ~ NA_character_
    )
  )

fd <- fd |>
  left_join(place_source_mapping, by = "local_de_elaboracion") |>
  mutate(
    # Create the new source_food variable
    source_place = case_when(
      !is.na(source_place) ~ source_place,
      TRUE ~ NA_character_
    )
  )

fd <- fd |>
  left_join(diagnosis_mapping, by = "diagnostico_agrupado") |>
  left_join(group_diagnosis_mapping, by = "type_diagnosis") |>
  mutate(
    type_diagnosis = case_when(
      !is.na(type_diagnosis) ~ type_diagnosis,
      TRUE ~ NA_character_
    ),
    group_diagnosis = case_when(
      !is.na(group_diagnosis) ~ group_diagnosis,
      TRUE ~ NA_character_
    )
  )

glimpse(fd)

# Final selection data: 12761
fd <- fd |> 
  rename(
    id = n_brote_eta,
    mun = comuna_com
  ) |> 
  select(
    id, notification_date, consumption_date, year:epimonth, 
    cod_reg:cod_mun, mun, zone, clim_zone, 
    expoused:n_sick_65, 
    source_food:group_diagnosis
  ) 

glimpse(fd)
summary(fd)

fd <-fd |> 
  drop_na() # Loss 2 NA (diagnosis variable )

# Join both data 
glimpse(fd) # 12759

# Save data 
save(fd, file=paste0(output, "FD_2011_2024", ".RData"))

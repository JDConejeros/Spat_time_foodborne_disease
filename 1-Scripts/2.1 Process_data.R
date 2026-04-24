# 1.0 Process data Foodborne diseases data -----

# Load settings
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")
source("Code/0.4 Mapping_vars.R")

# Paths 
input <- "02-Data/Input/"
output <- "02-Data/Output/"

# 1.1 Load data ----------------------------------------------------------------

# Population data to estimate incidence rates
pop <- rio::import("Data/estimaciones-y-proyecciones-2002-2035-comunas.xlsx") |> clean_names()
glimpse(pop)

# Foodborne diseases data
eta <- rio::import("Data/Brotes_ETA_2011_2024.xlsx") |> clean_names()
glimpse(eta)


############################################################################################################################/
# 3. Population Data ----------------------------------------------------------------
############################################################################################################################/

glimpse(pob)
unique(pob$nombre_region)

labs_mes <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
labs_reg <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", 
              "Valparaíso", "Metropolitana de Santiago", "Libertador General Bernardo O'Higgins", "Maule", "Ñuble",
              "Biobío", "La Araucanía", "Los Ríos", "Los Lagos",  "Aysén del General Carlos Ibáñez del Campo", 
              "Magallanes y de la Antártica Chilena")

# Region 
pob_reg <- pob |> 
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
  mutate(region=factor(nombre_region, 
                       levels=labs_reg,
                       labels=labs_reg)) |> 
  relocate(region) |> 
  select(-nombre_region) |> 
  pivot_longer(cols = !region, 
               names_to = "ano_estadistico",
               names_prefix = "pop_",
               values_to = "pop") |> 
  mutate(ano_estadistico=as.numeric(ano_estadistico)) |> 
  rename(year = ano_estadistico) 

region_codes <- data.frame(
  region = c("Arica y Parinacota", 
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
             "Magallanes y de la Antártica Chilena"),
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


pob_reg <- pob_reg |> 
  left_join(region_codes, by=c("region"="region")) |> 
  relocate(region_code, .after=region)

glimpse(pob_reg)

# Code region 
com <- chilemapas::codigos_territoriales |> 
  select(codigo_region, nombre_region) |> 
  distinct() |> 
  rename(cod_reg = codigo_region)

unique(pob_reg$region)

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

pob_reg <- pob_reg |> 
  left_join(region_mapping, by=c("region"="region")) |> 
  left_join(com, by=c("region_adj"="nombre_region")) |> 
  select(-region_adj) |> 
  relocate(cod_reg, .after=region) |> 
  mutate(cod_reg = as.numeric(cod_reg))

glimpse(pob_reg)

save(pob_reg, file=paste0("Data/", "Population_region_2011_2024", ".RData"))

# Municipality 
pob_mun <- pob |> 
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

pob_mun <- pob_mun |> 
  left_join(com, by=c("comuna"="codigo_comuna")) |> 
  rename(name_com = nombre_comuna) |> 
  relocate(name_com, .before=comuna)

glimpse(pob_mun)

save(pob_mun, file=paste0("Data/", "Population_municipality_2011_2024", ".RData"))

############################################################################################################################/
# 4. ETAS Data ----------------------------------------------------------------
############################################################################################################################/
glimpse(eta_cases)

# Adjust municipality
com <- chilemapas::codigos_territoriales |> 
  distinct() |> 
  rename(cod_reg = codigo_region) |> 
  mutate(cod_mun = as.numeric(codigo_comuna)) |>
  mutate(cod_reg = as.numeric(cod_reg)) |> 
  rename(mun = nombre_comuna) |> 
  rename(reg = nombre_region) |> 
  select(cod_reg, reg, cod_mun, mun)

etas <- eta_cases |>
  left_join(comuna_mapping, by = c("comuna_de_consumo" = "comuna_eta")) |>
  left_join(com, by = c("comuna_com" = "mun")) |> 
  filter(comuna_de_consumo!="Ignorada")

# Adjust time vars
etas <- etas |> 
  mutate(year = year(fecha_de_notificacion), 
         semester=as_semester(fecha_de_notificacion), 
         trimester=as.yearqtr(fecha_de_notificacion),
         month_num = month(fecha_de_notificacion),
         week_num = week(fecha_de_notificacion),
         day_num = day(fecha_de_notificacion),
         day = fecha_de_notificacion,
         epiweek = tsibble::yearweek(fecha_de_notificacion),
         epiweek = lubridate::as_date(epiweek),
         epimonth= tsibble::yearmonth(fecha_de_notificacion),
         epimonth = lubridate::as_date(epimonth)
         )

# Adjust zone
etas <- etas |> 
  mutate(zone = case_when(
           reg %in% c("Arica y Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo") ~ "North",
           reg %in% c("Valparaiso", "Metropolitana de Santiago", "Libertador General Bernardo OHiggins", "Maule", "Nuble", "Biobio") ~ "Center",
           reg %in% c("La Araucania", "Los Rios", "Los Lagos", "Aysen del General Carlos Ibanez del Campo", "Magallanes y de la Antartica Chilena") ~ "South",
           TRUE ~ NA_character_
         ), 
         zone=factor(zone, levels=c("North", "Center", "South")))

# Outcomes
etas <- etas |> 
  mutate(
    expoused = expuestos,
    sick = enfermos, 
    dead = fallecidos,
    hosp = total_de_hospitalizados 
         )

# Filter data 
etas <- etas |> 
  filter(conclusion_del_brote == "Realizada, se confirma") |> 
  group_by(n_brote_eta) |> 
  mutate(dup = n()) |> 
  ungroup() |> 
  filter(dup==1) |> 
  select(-dup) 

# Criteria of case definition
etas <- etas |> 
  mutate(
    minimum_cases = sick >= 2
  ) |> 
  filter(minimum_cases == 1) |>
  filter(!is.na(codigo_cie_10)) |> 
  filter(!is.na(diagnostico_agrupado)) |> 
  filter(!is.na(local_de_elaboracion)) |> 
  select(-minimum_cases)


# Adjust Source_food	Source_place	Type of diagnosis	Group diagnosis
sort(unique(etas$grupo_alimento_sospechoso))
sort(unique(etas$local_de_elaboracion))
sort(unique(etas$diagnostico_agrupado))

etas <- etas |>
  left_join(food_source_mapping, by = "grupo_alimento_sospechoso") |>
  mutate(
    # Create the new source_food variable
    source_food = case_when(
      !is.na(source_food) ~ source_food,
      TRUE ~ NA_character_
    )
  )

etas <- etas |>
  left_join(place_source_mapping, by = "local_de_elaboracion") |>
  mutate(
    # Create the new source_food variable
    source_place = case_when(
      !is.na(source_place) ~ source_place,
      TRUE ~ NA_character_
    )
  )

etas <- etas |>
  left_join(place_source_mapping, by = "local_de_elaboracion") |>
  mutate(
    # Create the new source_food variable
    source_place = case_when(
      !is.na(source_place) ~ source_place,
      TRUE ~ NA_character_
    )
  )

etas <- etas |>
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


glimpse(etas)

# Edit name variables and select
etas <- etas |> 
  rename(
    id = n_brote_eta,
    mun = comuna_com
  ) |> 
  select(id, year:epimonth, cod_reg:cod_mun, mun, zone, expoused:hosp, source_food:group_diagnosis) |> 
  drop_na()

# Join both data 
glimpse(etas) # 12806

save(etas, file=paste0("Data/", "ETA_cases_2011_2024", ".RData"))

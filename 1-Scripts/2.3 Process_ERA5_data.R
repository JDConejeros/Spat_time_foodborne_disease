# 2.3 Process Climate data -----

# Load settings
source("1-Scripts/1.1 Settings.R")
source("1-Scripts/1.2 Packages.R")
source("1-Scripts/1.3 Functions.R")
source("1-Scripts/1.4 Mapping_vars.R")

# Paths 
input <- "2-Data/Input/GEE_ClimData/"
output <- "2-Data/Output/"

############################################################################################################################/
# 2.3.1 Climate Data ----------------------------------------------------------------
############################################################################################################################/

# Load data --------
csv_files <- sort(list.files(input, pattern = "\\.csv$", full.names = TRUE))
csv_files 

era5_list <- lapply(csv_files, function(path) {
  rio::import(path, show_col_types = FALSE)
})
names(era5_list) <- sub("\\.csv$", "", basename(csv_files))

# Append data -------
era5 <- dplyr::bind_rows(era5_list)
glimpse(era5)

# Process data -------
era5_clean <- era5 |> 
  select(name_com, codigo_comuna, date, temperature_2m:total_precipitation_sum) |> 
  rename(
    mun=codigo_comuna
  )

glimpse(era5_clean)

# Check NA values for extraction 
test <- era5_clean |> 
  group_by(name_com, mun, year(date)) |> 
  summarise(
    across(temperature_2m:total_precipitation_sum, ~ sum(is.na(.)))
  ) |> 
  ungroup() |> 
  arrange(-temperature_2m) |> 
  filter(temperature_2m > 0)

unique(test$name_com) # 5 municipalities with NA values for temperature_2m, but not for the other variables.
#  "Antartica"      "Isla de Pascua" "Juan Fernandez" "Puqueldon"      "Quinchao"  
remove_mun <- unique(test$mun) # 12202  5201  5104 10206 10210

era5_clean <- era5_clean |> 
  filter(!mun %in% remove_mun)

# Save data -------
save(era5_clean, file=paste0(output, "ERA5_climate_2011_2024", ".RData"))
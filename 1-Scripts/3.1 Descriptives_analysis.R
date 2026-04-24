# 3.1 Descriptive Analysis -------

# Load settings
source("1-Scripts/1.1 Settings.R")
source("1-Scripts/1.2 Packages.R")
source("1-Scripts/1.3 Functions.R")
source("1-Scripts/1.4 Mapping_vars.R")

# Paths
input <- "2-Data/Output/"
output <- "3-Output/Descriptives/"

# Load data
fd <- rio::import(paste0(input, "FD_2011_2024.RData"))
fd_long <- readRDS(paste0(input, "Longitudinal_FD_CLIM_2011_2024.rds"))

# Keep sf geometry for maps; coerce non-sf panels to tibble only
as_tbl_keep_geom <- function(x) {
  if (inherits(x, "sf")) {
    return(x)
  }
  tibble::as_tibble(x)
}
fdy <- as_tbl_keep_geom(fd_long$data_year)
fdw <- as_tbl_keep_geom(fd_long$data_w) |>
  dplyr::mutate(wyear = lubridate::year(lubridate::as_date(.data$epiweek)))

glimpse(fd)
glimpse(fdy)
glimpse(fdw)

############################################################################################################################/
# 3.1.1 FD ----------------------------------------------------------------
############################################################################################################################/

# Table 1. Descriptive FD  ---------------------------------------------------

# Reference levels and dummies: 1.3 Functions.R (add_fbd_event_dummies, lev_*)
fd_evt <- add_fbd_event_dummies(fd, remove_source_vars = TRUE)

# --- Table 1: all Chile, zone, climate zone -------------------------------------
z_levels <- c("North", "Center", "South")
czone_levels <- if (is.factor(fd_evt$clim_zone)) {
  levels(forcats::fct_drop(droplevels(fd_evt$clim_zone)))
} else {
  sort(unique(as.character(fd_evt$clim_zone)))
}

p1 <- list(
  `Chile, full period` = combine_epiclimate(fd_evt, fdy, fdw)
)
for (z in z_levels) {
  de <- fd_evt |> dplyr::filter(.data$zone == z)
  dy <- fdy |> dplyr::filter(.data$zone == z)
  dw <- fdw |> dplyr::filter(.data$zone == z)
  p1[[paste0("Zone, ", z)]] <- combine_epiclimate(de, dy, dw)
}
for (cz in czone_levels) {
  de <- fd_evt |> dplyr::filter(as.character(.data$clim_zone) == cz)
  dy <- fdy |> dplyr::filter(as.character(.data$clim_zone) == cz)
  dw <- fdw |> dplyr::filter(as.character(.data$clim_zone) == cz)
  p1[[paste0("Clim. zone, ", janitor::make_clean_names(cz, case = "title"))]] <-
    combine_epiclimate(de, dy, dw)
}
table1 <- join_by_statistic(p1)

# --- Table 2: full period + years 2011–2024 -------------------------------------
years <- 2011:2024L
p2 <- list(
  `Full period` = combine_epiclimate(fd_evt, fdy, fdw)
)
for (y in years) {
  de <- fd_evt |> dplyr::filter(.data$year == y)
  dy <- fdy |> dplyr::filter(.data$year == y)
  dw <- fdw |> dplyr::filter(.data$wyear == y)
  p2[[as.character(y)]] <- combine_epiclimate(de, dy, dw)
}
table2 <- join_by_statistic(p2)

# --- Table 3: Chile, regions ---------------------------------------------------
regs <- sort(unique(fd_evt$reg))
p3 <- list(`Chile, full period` = combine_epiclimate(fd_evt, fdy, fdw))
for (r in regs) {
  de <- fd_evt |> dplyr::filter(.data$reg == r)
  dy <- fdy |> dplyr::filter(.data$reg == r)
  dw <- fdw |> dplyr::filter(.data$reg == r)
  p3[[as.character(r)]] <- combine_epiclimate(de, dy, dw)
}
table3 <- join_by_statistic(p3)

# --- Table 4: Chile, communes (wide) ------------------------------------------
mun_keys <- fd_evt |>
  dplyr::distinct(.data$cod_mun, .data$mun) |>
  dplyr::arrange(.data$cod_mun)
p4 <- list(`Chile, full period` = combine_epiclimate(fd_evt, fdy, fdw))
for (k in seq_len(nrow(mun_keys))) {
  cm <- mun_keys$cod_mun[[k]]
  mlabel <- as.character(mun_keys$mun[[k]])
  de <- fd_evt |> dplyr::filter(.data$cod_mun == cm)
  dy <- fdy |> dplyr::filter(.data$cod_mun == cm)
  dw <- fdw |> dplyr::filter(.data$cod_mun == cm)
  p4[[paste0(mlabel, " (", cm, ")")]] <- combine_epiclimate(de, dy, dw)
}
table4 <- join_by_statistic(p4)

# --- Table 5: Chile, region x year --------------------------------------------
p5 <- list(`Chile, full period` = combine_epiclimate(fd_evt, fdy, fdw))
for (r in regs) {
  for (y in years) {
    de <- fd_evt |> dplyr::filter(.data$reg == r, .data$year == y)
    dy <- fdy |> dplyr::filter(.data$reg == r, .data$year == y)
    dw <- fdw |> dplyr::filter(.data$reg == r, .data$wyear == y)
    p5[[paste0(r, " | ", y)]] <- combine_epiclimate(de, dy, dw)
  }
}
# Tall (statistic × stratum) and transposed: one row per region–year, columns = all statistics; split Region + time
table5_tall <- join_by_statistic(p5)
table5 <- transpose_stratum_by_statistic(table5_tall) |>
  format_table5_region_time()

# --- Excel: one sheet per table ------------------------------------------------
xl_path <- paste0(output, "Table1_descriptives_fd.xlsx")

openxlsx::write.xlsx(
  list(
    `Table1_zone_clim` = as.data.frame(table1, stringsAsFactors = FALSE),
    `Table2_year` = as.data.frame(table2, stringsAsFactors = FALSE),
    `Table3_region` = as.data.frame(table3, stringsAsFactors = FALSE),
    `Table4_municipality` = as.data.frame(table4, stringsAsFactors = FALSE),
    `Table5_region_year` = as.data.frame(table5, stringsAsFactors = FALSE)
  ),
  xl_path,
  overwrite = TRUE
)

# Figure 1. Descriptive FD  ---------------------------------------------------


############################################################################################################################/
# 3.1.2 Temp / Precipitation ----------------------------------------------------------------
############################################################################################################################/

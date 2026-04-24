# 0.3 Functions ----------------
# Note: add_event_indicators and level vectors require janitor (load via 1.2 Packages.R).

# --- Reference levels (food, place, type, group) for outbreak indicators ------
# Same definitions as the Join and descriptive pipelines (2011–2024, Chile).

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

# --- 0/1 columns per event row (prefix_food, place, type, group) ----------------
add_event_indicators_by_levels <- function(df, col, ref_levels, prefix) {
  if (!col %in% names(df)) {
    return(df)
  }
  levs <- unique(c(ref_levels, as.character(stats::na.omit(df[[col]]))))
  for (a in levs) {
    nm <- paste0(prefix, "_", janitor::make_clean_names(a))
    df[[nm]] <- as.integer(!is.na(df[[col]]) & df[[col]] == a)
  }
  df
}

# Apply all FBD dummies; optionally drop the four source string columns
add_fbd_event_dummies <- function(df, remove_source_vars = TRUE) {
  df <- df |>
    add_event_indicators_by_levels("source_food", lev_source_food, "food") |>
    add_event_indicators_by_levels("source_place", lev_source_place, "place") |>
    add_event_indicators_by_levels("type_diagnosis", lev_type_diagnosis, "type") |>
    add_event_indicators_by_levels("group_diagnosis", lev_group_diagnosis, "group")
  if (isTRUE(remove_source_vars)) {
    df <- df |>
      dplyr::select(-dplyr::all_of(
        c("source_food", "source_place", "type_diagnosis", "group_diagnosis")
      ))
  }
  df
}

# Column names of binary epi / exposure indicators after add_fbd_event_dummies
fbd_ind_epi_cat_var_names <- function(d) {
  grep("^(food|place|type|group)_", names(d), value = TRUE)
}

make_dummies <- function(v, prefix = "") {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  d
}

as_semester <- function(x, period = 6, sep = " S") {
  ym <- as.yearmon(x)
  paste(as.integer(ym), (cycle(ym) - 1) %/% period + 1, sep = sep)
}

# --- number formatting: decimal ".", no thousands separator; 2 d.p. if |x| >= 1, else 3 d.p. if |x| < 1
fmtC_num <- function(x, digits) {
  formatC(
    x,
    format = "f",
    digits = digits,
    big.mark = "",
    decimal.mark = "."
  )
}

format_num_cell <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("")
  }
  x <- suppressWarnings(as.numeric(x)[[1L]])
  if (is.na(x) || is.nan(x)) {
    return("")
  }
  if (abs(x) >= 1) {
    return(fmtC_num(x, 2L))
  }
  fmtC_num(x, 3L)
}

fmt_count <- function(n) {
  n <- as.numeric(n)
  if (length(n) != 1L || is.na(n)) {
    return("")
  }
  fmtC_num(n, 0L)
}

fmt_n_pct_of_exp <- function(n_num_v, n_den_v) {
  n_num <- sum(as.numeric(n_num_v), na.rm = TRUE)
  n_den <- sum(as.numeric(n_den_v), na.rm = TRUE)
  if (n_den <= 0) {
    return(paste0(fmt_count(n_num), " (—)"))
  }
  pct <- 100 * n_num / n_den
  ptxt <- if (abs(pct) >= 1) {
    fmtC_num(pct, 2L)
  } else {
    fmtC_num(pct, 3L)
  }
  paste0(fmt_count(n_num), " (", ptxt, ")")
}

# Count + % of total outbreaks in the stratum (categorical dummies: food, place, group, type)
fmt_n_pct_of_outbreaks <- function(n_count, n_outbreaks) {
  n_c <- as.numeric(n_count)[[1L]]
  n_o <- as.integer(n_outbreaks)[[1L]]
  if (length(n_o) != 1L || is.na(n_o) || n_o < 0L) {
    n_o <- 0L
  }
  if (n_o <= 0L) {
    return(paste0(fmt_count(n_c), " (—)"))
  }
  pct <- 100 * n_c / n_o
  ptxt <- if (abs(pct) >= 1) {
    fmtC_num(pct, 2L)
  } else {
    fmtC_num(pct, 3L)
  }
  paste0(fmt_count(n_c), " (", ptxt, ")")
}

fmt_mean_sd_minmax <- function(v) {
  v <- as.numeric(v)
  v <- v[!is.na(v)]
  if (length(v) < 1L) {
    return("")
  }
  m <- mean(v, na.rm = TRUE)
  s <- if (length(v) < 2L) {
    NA_real_
  } else {
    stats::sd(v, na.rm = TRUE)
  }
  mn <- min(v, na.rm = TRUE)
  mx <- max(v, na.rm = TRUE)
  core <- if (is.na(s)) {
    paste0(format_num_cell(m), " (—)")
  } else if (s == 0) {
    paste0(format_num_cell(m), " (0)")
  } else {
    paste0(format_num_cell(m), " (", format_num_cell(s), ")")
  }
  paste0(core, " [", format_num_cell(mn), " - ", format_num_cell(mx), "]")
}

stratum_epidemiology <- function(d) {
  d <- dplyr::as_tibble(d)
  if (nrow(d) < 1L) {
    return(tibble::tibble(
      n_outbreaks = "0", n_exposed = "0", n_ill = "— (—)", n_hosp = "— (—)", n_dead = "— (—)",
      n_ill_1_14 = "— (—)", n_ill_15_44 = "— (—)", n_ill_45_64 = "— (—)", n_ill_65 = "— (—)",
      food_eggs = "0 (—)", food_meat = "0 (—)", food_milk = "0 (—)", food_not_det = "0 (—)",
      food_ready = "0 (—)", food_seafood = "0 (—)", food_water = "0 (—)",
      place_unauth = "0 (—)", place_prod = "0 (—)", place_unk = "0 (—)", place_self = "0 (—)",
      group_bact = "0 (—)", group_chem = "0 (—)", group_par = "0 (—)", group_vir = "0 (—)",
      type_salmonella = "0 (—)"
    ))
  }
  n_out <- as.integer(nrow(d))
  tibble::tibble(
    n_outbreaks = as.character(n_out),
    n_exposed = fmt_count(sum(d$expoused, na.rm = TRUE)),
    n_ill = fmt_n_pct_of_exp(d$sick, d$expoused),
    n_hosp = fmt_n_pct_of_exp(d$hosp, d$expoused),
    n_dead = fmt_n_pct_of_exp(d$dead, d$expoused),
    n_ill_1_14 = fmt_n_pct_of_exp(d$n_sick_1_14, d$expoused),
    n_ill_15_44 = fmt_n_pct_of_exp(d$n_sick_15_44, d$expoused),
    n_ill_45_64 = fmt_n_pct_of_exp(d$n_sick_45_64, d$expoused),
    n_ill_65 = fmt_n_pct_of_exp(d$n_sick_65, d$expoused),
    food_eggs = fmt_n_pct_of_outbreaks(sum(d$food_eggs, na.rm = TRUE), n_out),
    food_meat = fmt_n_pct_of_outbreaks(sum(d$food_meat_and_meat_products, na.rm = TRUE), n_out),
    food_milk = fmt_n_pct_of_outbreaks(sum(d$food_milk_and_milk_products, na.rm = TRUE), n_out),
    food_not_det = fmt_n_pct_of_outbreaks(sum(d$food_not_determined, na.rm = TRUE), n_out),
    food_ready = fmt_n_pct_of_outbreaks(sum(d$food_ready_to_eat, na.rm = TRUE), n_out),
    food_seafood = fmt_n_pct_of_outbreaks(sum(d$food_seafood, na.rm = TRUE), n_out),
    food_water = fmt_n_pct_of_outbreaks(sum(d$food_water, na.rm = TRUE), n_out),
    place_unauth = fmt_n_pct_of_outbreaks(
      sum(d$place_place_not_authorized_or_only_for_sale, na.rm = TRUE), n_out
    ),
    place_prod = fmt_n_pct_of_outbreaks(
      sum(d$place_place_of_production_and_or_consumption, na.rm = TRUE), n_out
    ),
    place_unk = fmt_n_pct_of_outbreaks(sum(d$place_not_determined, na.rm = TRUE), n_out),
    place_self = fmt_n_pct_of_outbreaks(
      sum(d$place_place_of_self_consumption, na.rm = TRUE), n_out
    ),
    group_bact = fmt_n_pct_of_outbreaks(sum(d$group_bacteria, na.rm = TRUE), n_out),
    group_chem = fmt_n_pct_of_outbreaks(sum(d$group_chemical_agents, na.rm = TRUE), n_out),
    group_par = fmt_n_pct_of_outbreaks(sum(d$group_parasites, na.rm = TRUE), n_out),
    group_vir = fmt_n_pct_of_outbreaks(sum(d$group_viruses, na.rm = TRUE), n_out),
    type_salmonella = fmt_n_pct_of_outbreaks(sum(d$type_salmonella_spp, na.rm = TRUE), n_out)
  )
}

# Number of TRUE runs in order (RLE), e.g. consecutive epiweeks with ≥1 Tmax above-threshold day
rle_true_runs_count <- function(x) {
  x <- as.logical(x) & !is.na(x)
  if (!length(x)) {
    return(0L)
  }
  r <- base::rle(x)
  as.integer(sum(r$values, na.rm = TRUE))
}

# Heat waves: only Tmax; week is "hot" if any day in week has Tmax > commune P* threshold (Join flags).
# Per (municipality, year): number of non-overlapping runs of consecutive epiweeks with ≥1 such day.
# Summarise across commune–years in the stratum: Mean (SD) [min - max] of that count.
tmax_hw_commune_year <- function(fdw0) {
  if (!NROW(fdw0)) {
    return(tibble::tibble(
      hw_tmax_p90 = double(),
      hw_tmax_p95 = double(),
      hw_tmax_p99 = double()
    ))
  }
  wdf <- if (inherits(fdw0, "sf")) {
    sf::st_drop_geometry(fdw0)
  } else {
    fdw0
  }
  wdf <- tibble::as_tibble(wdf)
  if (!all(c("cod_mun", "epiweek", "n_tmax_p90", "n_tmax_p95", "n_tmax_p99") %in% names(wdf))) {
    return(tibble::tibble(
      hw_tmax_p90 = double(),
      hw_tmax_p95 = double(),
      hw_tmax_p99 = double()
    ))
  }
  wdf$wyr <- if ("wyear" %in% names(wdf)) {
    wdf$wyear
  } else {
    as.integer(lubridate::year(lubridate::as_date(wdf$epiweek)))
  }
  wdf |>
    dplyr::arrange(.data$cod_mun, .data$wyr, .data$epiweek) |>
    dplyr::group_by(.data$cod_mun, .data$wyr) |>
    dplyr::summarise(
      hw_tmax_p90 = rle_true_runs_count(.data$n_tmax_p90 > 0L),
      hw_tmax_p95 = rle_true_runs_count(.data$n_tmax_p95 > 0L),
      hw_tmax_p99 = rle_true_runs_count(.data$n_tmax_p99 > 0L),
      .groups = "drop"
    )
}

stratum_climate_fdy_fdw <- function(fdy0, fdw0) {
  chw <- tmax_hw_commune_year(fdw0)
  hw_p90 <- if (NROW(chw)) fmt_mean_sd_minmax(chw$hw_tmax_p90) else ""
  hw_p95 <- if (NROW(chw)) fmt_mean_sd_minmax(chw$hw_tmax_p95) else ""
  hw_p99 <- if (NROW(chw)) fmt_mean_sd_minmax(chw$hw_tmax_p99) else ""
  tibble::tibble(
    T2m = fmt_mean_sd_minmax(fdy0$temperature_2m),
    Tmin = fmt_mean_sd_minmax(fdy0$temperature_2m_min),
    Tmax = fmt_mean_sd_minmax(fdy0$temperature_2m_max),
    precip = fmt_mean_sd_minmax(fdy0$total_precipitation_sum),
    CSI1 = if ("CSI_v1" %in% names(fdw0)) fmt_mean_sd_minmax(fdw0$CSI_v1) else "",
    CSI2 = if ("CSI_v2" %in% names(fdw0)) fmt_mean_sd_minmax(fdw0$CSI_v2) else "",
    CSI3 = if ("CSI_v3" %in% names(fdw0)) fmt_mean_sd_minmax(fdw0$CSI_v3) else "",
    hw_tmax_p90 = hw_p90,
    hw_tmax_p95 = hw_p95,
    hw_tmax_p99 = hw_p99
  )
}

statistic_labels <- c(
  "N outbreaks", "N exposed", "N ill, % of exposed", "N hospitalised, % of exposed",
  "N dead, % of exposed", "N ill aged 0–14 y, % of exposed", "N ill aged 15–44 y, % of exposed",
  "N ill aged 45–64 y, % of exposed", "N ill aged 65+ y, % of exposed",
  "N outbreaks, implicated food: eggs (% of total outbreaks)",
  "N outbreaks, implicated food: meat and meat products (% of total outbreaks)",
  "N outbreaks, implicated food: milk and milk products (% of total outbreaks)",
  "N outbreaks, implicated food: not determined (% of total outbreaks)",
  "N outbreaks, implicated food: ready to eat (% of total outbreaks)",
  "N outbreaks, implicated food: seafood (% of total outbreaks)", "N outbreaks, implicated food: water (% of total outbreaks)",
  "N outbreaks, place: not authorized or for sale only (% of total outbreaks)",
  "N outbreaks, place: production and/or consumption (% of total outbreaks)",
  "N outbreaks, place: not determined (% of total outbreaks)", "N outbreaks, place: self-consumption (% of total outbreaks)",
  "N outbreaks, aetiology group: bacteria (% of total outbreaks)", "N outbreaks, aetiology group: chemical agents (% of total outbreaks)",
  "N outbreaks, aetiology group: parasites (% of total outbreaks)", "N outbreaks, aetiology group: viruses (% of total outbreaks)",
  "N outbreaks, agent: Salmonella spp. (% of total outbreaks)",
  "Mean (SD) [Min - Max] 2 m temperature, °C", "Mean (SD) [Min - Max] 2 m minimum temperature, °C", "Mean (SD) [Min - Max] 2 m maximum temperature, °C",
  "Mean (SD) [Min - Max] total precipitation, mm (annual sum, commune–year)",
  "Mean (SD) [Min - Max] climate stress index v1 (weekly)", "Mean (SD) [Min - Max] climate stress index v2 (weekly)", "Mean (SD) [Min - Max] climate stress index v3 (weekly)",
  "Mean (SD) [Min - Max] HW, Tmax P90 (n runs, commune–year)",
  "Mean (SD) [Min - Max] HW, Tmax P95 (n runs, commune–year)",
  "Mean (SD) [Min - Max] HW, Tmax P99 (n runs, commune–year)"
)

combine_epiclimate <- function(d_evt, fdy0, fdw0) {
  e <- stratum_epidemiology(d_evt)
  cld <- stratum_climate_fdy_fdw(fdy0, fdw0)
  vals <- c(as.character(e[1, ]), as.character(cld[1, ]))
  tibble::tibble(Statistic = statistic_labels, Value = vals)
}

join_by_statistic <- function(parts_list) {
  nms <- names(parts_list)
  name_col2 <- function(df, newname) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    names(df)[2L] <- newname
    tibble::as_tibble(df)
  }
  out <- name_col2(parts_list[[1L]], nms[[1L]])
  if (length(parts_list) < 2L) {
    return(out)
  }
  for (i in 2L:length(parts_list)) {
    out <- out |>
      dplyr::left_join(
        name_col2(parts_list[[i]], nms[[i]]),
        by = "Statistic"
      )
  }
  out
}

# Wide table: rows = stratum, columns = statistics (for region×year, etc.)
transpose_stratum_by_statistic <- function(df) {
  if (!NROW(df)) {
    return(tibble::tibble())
  }
  strata <- setdiff(names(df), "Statistic")
  m <- t(as.matrix(df[, strata, drop = FALSE]))
  cn <- as.character(df$Statistic)
  if (any(duplicated(cn))) {
    cn <- make.unique(cn, sep = " ")
  }
  colnames(m) <- cn
  out <- cbind(
    data.frame(Stratum = strata, stringsAsFactors = FALSE, check.names = FALSE),
    as.data.frame(m, stringsAsFactors = FALSE, check.names = FALSE)
  )
  tibble::as_tibble(out)
}

# Table 5: split "Region | Year" / "Chile, full period" into two leading columns
format_table5_region_time <- function(wide_tall) {
  if (!"Stratum" %in% names(wide_tall) || !NROW(wide_tall)) {
    return(wide_tall)
  }
  n <- nrow(wide_tall)
  reg <- rep(NA_character_, n)
  mtime <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    s <- as.character(wide_tall$Stratum[[i]])
    if (grepl("^Chile,\\s*full", s, ignore.case = TRUE) || s == "Chile, full period") {
      reg[i] <- "Chile"
      mtime[i] <- "Full period (2011–2024)"
    } else {
      p <- strsplit(s, " \\| ", fixed = FALSE)[[1L]]
      if (length(p) == 2L) {
        reg[i] <- gsub("^\\s+|\\s+$", "", p[[1L]])
        mtime[i] <- gsub("^\\s+|\\s+$", "", p[[2L]])
      } else {
        reg[i] <- s
        mtime[i] <- "—"
      }
    }
  }
  dplyr::bind_cols(
    tibble::tibble(Region = reg, `Measurement time` = mtime),
    wide_tall |>
      dplyr::select(-dplyr::all_of("Stratum"))
  )
}
# 02. Package Installation and Loading ------------------------------------------------------------

# Function install/load packages
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

# Grouped by purpose, then passed to install_load
pkgs_io <- c(
  "rio",      # import/export (many formats)
  "readr",    # fast read/write for delimited text
  "openxlsx"  # multi-sheet Excel
)

pkgs_wrangle <- c(
  "dplyr",     # data manipulation
  "tidyr",     # pivoting, nesting, tidy data
  "stringr",   # string operations
  "lubridate", # dates and times
  "purrr",     # functional tools on lists/vectors
  "tibble",    # modern data frames
  "forcats",   # factors
  "janitor"    # clean names, duplicates, tabulations
)

pkgs_time <- c(
  "zoo",     # time-indexed series
  "tsibble"  # tidy temporal data, calendars
)

pkgs_viz <- c(
  "ggplot2",    # core plotting
  "ggpubr",     # publication-style figures
  "ggthemes",   # extra themes and palettes
  "ggtext",     # rich text in plots
  "patchwork",  # compose ggplot panels
  "gridExtra",  # arrange grid graphics
  "grid",       # low-level layout
  "gridtext",   # text in grid
  "scales",     # scale helpers for ggplot
  "viridis",    # color scales
  "kableExtra"  # rich tables (e.g. with knitr)
)

pkgs_report <- c(
  "texreg"  # regression tables to LaTeX / HTML
)

pkgs_spatial <- c(
  "spData",    # example spatial data
  "sf",        # simple features vector data
  "spdep",     # spatial dependence
  "sfdep",     # spatial analysis on sf
  "chilemapas" # Chile admin boundaries for ggplot2
)

pkgs_workflow <- c(
  "future.apply", # parallel apply-style on future backend
  "tictoc"        # simple timing
)

install_load(c(
  pkgs_io, 
  pkgs_wrangle, 
  pkgs_time, 
  pkgs_viz, 
  pkgs_report,
  pkgs_spatial, 
  pkgs_workflow
))


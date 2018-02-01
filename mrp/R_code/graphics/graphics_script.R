# 2016 GRAPHICS

# COUNTY PLOTS
prefix_path <- paste0("county_figures/")
source('R_code/graphics/county_level.R')           # County level plots

# CHARTS
pstrat <- "pstrat_2016_modeled.RDS"
year <- 2016
prefix_path <- paste0("charts/", year, "/")
source('R_code/graphics/charts.R')                 # Charts (Model 1)
source('R_code/graphics/pooled_charts.R')          # Pooled charts
source('R_code/graphics/turnout_pooled_charts.R')  # Pooled Turnout
source('R_code/graphics/pooled_charts_whites.R')   # Pooled charts (Model 1 - white only)
source('R_code/graphics/gender_charts_whites.R')   # Gender charts (Model 1 - white only)

pstrat <- "pstrat_income.RDS"
source('R_code/graphics/income_charts.R')          # Charts (Model 2)

# MAPS

pstrat <- "pstrat_2016_modeled.RDS"
year <- 2016
prefix_path <- paste0("maps/", year, "/")
source('R_code/graphics/maps.R')               # Model 1
source('R_code/graphics/turnout_maps.R')       # Turnout

# 2012 GRAPHICS

# CHARTS
pstrat <- "pstrat_2012_modeled.RDS"
year <- 2012
prefix_path <- paste0("charts/", year, "/")
source('R_code/graphics/charts.R')                 # Charts (Model 1)
source('R_code/graphics/pooled_charts.R')          # Pooled charts
source('R_code/graphics/turnout_pooled_charts.R')  # Pooled Turnout
source('R_code/graphics/pooled_charts_whites.R')   # Pooled charts (Model 1 - white only)
source('R_code/graphics/gender_charts_whites.R')   # Gender charts (Model 1 - white only)

# MAPS

pstrat <- "pstrat_2012_modeled.RDS"
year <- 2012
prefix_path <- paste0("maps/", year, "/")
source('R_code/graphics/maps.R')               # Model 1
source('R_code/graphics/turnout_maps.R')       # Turnout


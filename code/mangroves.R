##########################
### X. Mangrove Habitats ###
##########################

# Clear environment
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,
               fasterize,
               ggplot2,
               plyr,
               raster,
               rgdal,
               rgeos,
               sf,
               sp,
               tidyr)

#####################################
#####################################

# Set directories
## Define data directory (as this is an R Project, pathnames are simplified)
### USGS Mangrove report
mangrove_dir <- "data/a_raw_data/US_mangroves_datarelease"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
oyster_gpkg <- "data/b_intermediate_data/gom_oyster.gpkg"

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################
#####################################

# Load oyster layerss
# USGS Mangrove distribution (source: https://www.sciencebase.gov/catalog/item/61eb07ddd34e8b818ada4948)
## Presence (1) and absence (0)
mangroves <- sf::st_read(dsn = mangrove_dir, layer = "US_mangroves_datarelease")
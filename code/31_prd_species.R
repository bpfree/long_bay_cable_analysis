#########################################################
### 31. Protected Resources Division -- Species Data  ###
#########################################################

# Clear environment
rm(list = ls())

# Calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(docxtractr,
               dplyr,
               elsa,
               fasterize,
               fs,
               ggplot2,
               janitor,
               ncf,
               paletteer,
               pdftools,
               plyr,
               purrr,
               raster,
               RColorBrewer,
               reshape2,
               rgdal,
               rgeoda,
               rgeos,
               rmapshaper,
               rnaturalearth, # use devtools::install_github("ropenscilabs/rnaturalearth") if packages does not install properly
               RSelenium,
               sf,
               shadowr,
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr,
               tidyverse)

#####################################
#####################################

# Set directories
## Define data directory (as this is an R Project, pathnames are simplified)
### Input directories
prd_dir <- "data/a_raw_data/prd_species_data"
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

### Output directories
#### Intermediate directory
prd_gpkg <- "data/b_intermediate_data/prd_species.gpkg"

#####################################
#####################################

# Load data
## Study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

## PRD scored data
### ***Note: score of 1 signifies no conflict occurs, 0 is unsuitable
prd_species <- sf::st_read(dsn = prd_dir, layer = "final_Scored_PRD_WP_Layer_clip") %>%
  # reproject the coordinate reference system
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # obtain species data within study area
  sf::st_intersection(study_area) %>%
  # keep "product" and "value" fields
  dplyr::select(PRODUCT,
                value) %>%
  # create field called "layer" and fill with "protected species division" for summary
  dplyr::mutate(layer = "protected species division",
                value = 1 - PRODUCT) %>%
  # select important fields
  dplyr::select(layer,
                value)

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = prd_species, dsn = analysis_gpkg, "prd_species", append = F)

## PRD Species geopackage
sf::st_write(obj = prd_species, dsn = prd_gpkg, "prd_species", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
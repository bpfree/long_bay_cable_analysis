##############################
### 20. Aids to Navigation ###
##############################

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
navigation_aids_dir <- "data/a_raw_data/AtoN/AtoN.gpkg"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
aids_navigation_gpkg <- "data/b_intermediate_data/navigation_aids.gpkg"

# View layer names within geodatabase
sf::st_layers(dsn = navigation_aids_dir,
              do_count = TRUE)

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################

# Load aids to navigation data (source: https://marinecadastre.gov/downloads/data/mc/AtoN.zip)
## Metadata: https://www.fisheries.noaa.gov/inport/item/56120
aids_to_navigation <- sf::st_read(dsn = navigation_aids_dir, layer = "AtoN") %>%
  # reproject the coordinate reference system to match study area data (EPSG:5070)
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # obtain only aids to navigation in the study area
  sf::st_intersection(study_area) %>%
  # create field called "layer" and fill with "aids to navigation" for summary
  dplyr::mutate(layer = "aids to navigation") %>%
  #  add a setback (buffer) distance of 500 meters
  sf::st_buffer(dist = 500) %>%
  # group all features by the "layer" and "value" fields to then have a single feature
  # "value" will get pulled in from the study area layer
  dplyr::group_by(layer,
                  value) %>%
  # summarise data to obtain single feature
  dplyr::summarise()

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = aids_to_navigation, dsn = analysis_gpkg, "aids_to_navigation", append = F)

## Aids to Navigation geopackage
sf::st_write(obj = aids_to_navigation, dsn = aids_navigation_gpkg, "aids_to_navigation", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
###########################
### 8. Lightering Zones ###
###########################

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
### Input directory
lightering_zone_dir <- "data/a_raw_data/LighteringZone/LighteringZone.gpkg"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
lightering_zone_gpkg <- "data/b_intermediate_data/lightering_zone.gpkg"

# View layer names within geodatabase
## FWS National Realty Boundaries directory
sf::st_layers(dsn = lightering_zone_dir,
              do_count = TRUE)

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################

# NOAA lightering zones (https://marinecadastre.gov/downloads/data/mc/LighteringZone.zip)
## Metadata: https://www.fisheries.noaa.gov/inport/item/66149
## For more detailed information on lightering zones and coordinates for polygons: https://www.govinfo.gov/content/pkg/CFR-2018-title33-vol2/xml/CFR-2018-title33-vol2-part156.xml#seqnum156.300
lightering_zones <- sf::st_read(dsn = lightering_zone_dir, layer = "LighteringZone") %>%
  # reproject the coordinate reference system to match study area data
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # limit reefs to only those within study area
  sf::st_intersection(study_area) %>%
  # create field to define as "lightering zone"
  dplyr::mutate(layer = "lightering zones") %>%
  # group all features by the "layer" and "value" fields to then have a single feature
  # "value" will get pulled in from the study area layer
  dplyr::group_by(layer,
                  value) %>%
  # summarise data
  dplyr::summarise()

#####################################

g <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = lightering_zones)
g

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(lightering_zones, dsn = analysis_gpkg, layer = "lightering_zones", append = F)

## Conservation Areas geopackage
sf::st_write(lightering_zones, dsn = lightering_zone_gpkg, layer = "lightering_zones", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
##################################
### 16. BOEM No Activity Zones ###
##################################

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
no_activity_dir <- "data/a_raw_data/BOEM_topo_pin.gdb"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
no_activity_gpkg <- "data/b_intermediate_data/no_activity_zones.gpkg"

#####################################

# View layer names within geodatabase
sf::st_layers(dsn = no_activity_dir,
              do_count = TRUE)

## ***Note: There are two layers:
### 1.) SDETABS_PINNACLE_SDE
### 2.) SDETABS_TOPO_ZN_SDE --> this is the one that contains "No Activity Zones"

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

# Load BOEM No Activity Zones data
## ***Note: These data yet to be made public. Mariana Steen (mariana.steen@boem.gov)
## For more information, can examine this document: https://www.boem.gov/sites/default/files/oil-and-gas-energy-program/Leasing/Regional-Leasing/Gulf-of-Mexico-Region/Topographic-Features-Stipulation-Map-Package.pdf
no_activity_zones <- sf::st_read(dsn = no_activity_dir, layer = "SDETABS_TOPO_ZN_SDE") %>%
  # reproject the coordinate reference system to match study area data (EPSG:5070)
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # obtain only zones in the study area
  sf::st_intersection(study_area) %>%
  # filter by zones that are designated as "No Activity"
  dplyr::filter(ZONE == "NO ACTIVITY") %>%
  # group all features by the "ZONE" and "value" fields to then have a single feature
  # "value" will get pulled in from the study area layer
  dplyr::group_by(ZONE,
                  value) %>%
  # summarise data to obtain single feature
  dplyr::summarise() %>%
  # rename "zone" field
  dplyr::rename("layer" = "ZONE") %>%
  # recode "layer" field so that "NO ACTIVITY" becomes "no activity zone"
  dplyr::mutate(layer = recode(layer,
                               "NO ACTIVITY" = "no activity zone"))

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = no_activity_zones, dsn = analysis_gpkg, "boem_no_activity_zones", append = F)

## No Activity Zones geopackage
sf::st_write(obj = no_activity_zones, dsn = no_activity_gpkg, "boem_no_activity_zones", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
#######################################
### 26. Carbon Capture Lease Blocks ###
#######################################

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
carbon_capture_dir <- "data/a_raw_data/GOM_Potential_CCUS_BlocksForSuitabilityModelRun"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
carbon_capture_gpkg <- "data/b_intermediate_data/carbon_capture.gpkg"

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################

# Data came from Tershara Matthews (Tershara.Matthews@boem.gov)
## For further questions, direct them to BOEM

### Carbon capture underground storage lease blocks
carbon_capture <- sf::st_read(dsn = carbon_capture_dir, layer = "GOM_Potential_CCUS_Blocks") %>%
  # reproject the coordinate reference system
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # create field called "layer" and fill with "carbon capture" for summary along with "value" field with 0
  dplyr::mutate(layer = "carbon capture",
                value = 0) %>%
  # group all features by the "layer" and "value" fields to then have a single feature
  dplyr::group_by(layer,
                  value) %>%
  # summarise data to obtain single feature
  dplyr::summarise()

g <- ggplot2::ggplot() + 
  ggplot2::geom_sf(data = study_area, fill = NA, color = "blue", linetype = "dashed") +
  ggplot2::geom_sf(data = carbon_capture, color = "orange") +
  ggplot2::geom_sf(data = not_carbon_capture, fill = NA, color = "red", linetype = "dashed")
g

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = carbon_capture, dsn = analysis_gpkg, "carbon_capture_lease_blocks", append = F)

## Carbon capture geopackage
sf::st_write(obj = carbon_capture, dsn = carbon_capture_gpkg, "carbon_capture_lease_blocks", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
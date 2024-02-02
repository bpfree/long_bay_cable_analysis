####################################################
### 25. Coral Habitat Area of Particular Concern ###
####################################################

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
coral_hapc_dir <- "data/a_raw_data/HAPCshapefiles/shpFinal2"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
coral_hapc_gpkg <- "data/b_intermediate_data/coral_hapc.gpkg"

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################
#####################################

clean_coral <- function(coral_data){
  coral_layer <- coral_data %>%
    # reproject the coordinate reference system
    sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
    # obtain coral data within study area
    sf::st_intersection(study_area) %>%
    # create field called "layer" and fill with "coral hapc" for summary
    dplyr::mutate(layer = "coral hapc") %>%
    # select key fields
    dplyr::select(layer,
                  value)
  return(coral_layer)
}

#####################################
#####################################

# Load coral habitat area of particular concern (source: http://portal.gulfcouncil.org/Regulations/HAPCshapefiles.zip)
## Habitat Areas of Particular Concern are a subset of Essential Fish Habitat
## Older areas can have regulations or no regulations; newer ones under Amendment 9 might have proposed regulations or none proposed
## Amendment 9 went into effect on November 16, 2020 (read more about amendment here: https://www.govinfo.gov/content/pkg/FR-2020-10-16/pdf/2020-21298.pdf)

### Coral HAPC with regulations
coral_hapc_regs <- sf::st_read(dsn = coral_hapc_dir, layer = "ExistingWithRegs") %>%
  clean_coral()

### Coral HAPC without regulations
#### ***Note: No areas fall within study area
coral_hapc_noregs <- sf::st_read(dsn = coral_hapc_dir, layer = "ExistingWithOutRegs") %>%
  clean_coral()

### Coral Amendment 9 HAPC with regulations
#### ***Note: No areas fall within study area
coral9_hapc_regs <- sf::st_read(dsn = coral_hapc_dir, layer = "Coral9Regs") %>%
  clean_coral()

### Coral Amendment 9 HAPC without regulations
#### ***Note: No areas fall within study area
coral9_hapc_noregs <- sf::st_read(dsn = coral_hapc_dir, layer = "Coral9NoRegs") %>%
  clean_coral()

#####################################

coral_hapc_combined <- coral_hapc_regs %>%
  # combine coral datasets
  rbind(coral_hapc_noregs,
        coral9_hapc_regs,
        coral9_hapc_noregs) %>%
  # group all features by the "layer" and "value" fields to then have a single feature
  dplyr::group_by(layer,
                  value) %>%
  # summarise data to obtain single feature
  dplyr::summarise()

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = coral_hapc_combined, dsn = analysis_gpkg, "coral_hapc", append = F)

## Coral HAPC geopackage
sf::st_write(obj = coral_hapc_combined, dsn = coral_hapc_gpkg, "coral_hapc", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
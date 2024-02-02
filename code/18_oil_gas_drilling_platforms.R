##########################################
### 18. Oil and Gas Drilling Platforms ###
##########################################

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
boem_platform_dir <- "data/a_raw_data/Platforms.gdb/Platforms.gdb"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
platforms_gpkg <- "data/b_intermediate_data/drilling_platforms.gpkg"

# View layer names within geodatabase
sf::st_layers(dsn = boem_platform_dir,
              do_count = TRUE)

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################

# Load BOEM drilling platform data (source: https://www.data.boem.gov/Mapping/Files/Platforms.gdb.zip)
## Metadata: https://www.data.boem.gov/Mapping/Files/platform_meta.html
### Note: These data came from the mapping page: https://www.data.boem.gov/Main/Mapping.aspx#ascii
### Note: These data are different from the platform query page that BOEM has: https://www.data.boem.gov/Platform/PlatformStructures/Default.aspx
### That query page seems to mirror the data that BSEE also has these data
boem_platforms <- st_read(dsn = boem_platform_dir, layer = "Platforms") %>%
  # reproject the coordinate reference system to match study area data (EPSG:5070)
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # obtain only active oil and gas lease blocks in the study area
  sf::st_intersection(study_area) %>%
  # Filter for platforms that have been installed but not yet removed
  dplyr::filter(!is.na(INSTALL_DATE) & # platforms that have an install date (so is not NA)
                is.na(REMOVAL_DATE)) %>% # platforms that lack a removal date (so is NA)
  #  add a setback (buffer) distance of 152.4 meters (500 feet) around each drilling platform
  sf::st_buffer(dist = 152.4) %>%
  # create field called "layer" and fill with "drilling platform" for summary
  dplyr::mutate(layer = "drilling platform") %>%
  # group all features by the "layer" and "value" fields to then have a single feature
  # "value" will get pulled in from the study area layer
  dplyr::group_by(layer,
                  value) %>%
  # summarise data to obtain single feature
  dplyr::summarise()

## Check units for determining cellsize of grid
sf::st_crs(boem_platforms, parameters = TRUE)$units_gdal

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = boem_platforms, dsn = analysis_gpkg, "drilling_platforms", append = F)

## Drilling Platforms geopackage
sf::st_write(obj = boem_platforms, dsn = platforms_gpkg, "drilling_platforms", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
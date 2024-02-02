###########################
### 17. Anchorage Areas ###
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
### Input directories
anchorage_areas_dir <- "data/a_raw_data/Anchorage/AnchorageAreas.gdb"

### Output directories
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"
anchorage_areas_gpkg <- "data/b_intermediate_data/anchorage_areas.gpkg"

#####################################

# View layer names within geodatabase
sf::st_layers(dsn = anchorage_areas_dir,
              do_count = TRUE)

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################

# Load anchorage area data (source: https://marinecadastre.gov/downloads/data/mc/Anchorage.zip)
## Metadata: https://www.fisheries.noaa.gov/inport/item/48849
anchorage_areas <- sf::st_read(dsn = anchorage_areas_dir, layer = "AnchorageAreas") %>%
  # change multistring to multipolygon (for 5 features are multisurface: 654, 661, 672, 673, 721)
  sf::st_cast(to = "MULTIPOLYGON") %>%
  # make sure all geometries are valid
  sf::st_make_valid() %>%
  # reproject the coordinate reference system to match study area data (EPSG:5070)
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # obtain only anchorage areas in the study area
  sf::st_intersection(study_area) %>%
  # create field called "layer" and fill with "anchorage areas" for summary
  dplyr::mutate(layer = "anchorage areas") %>%
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
sf::st_write(obj = anchorage_areas, dsn = analysis_gpkg, "anchorage_areas", append = F)

## Anchorage Areas geopackage
sf::st_write(obj = anchorage_areas, dsn = anchorage_areas_gpkg, "anchorage_areas", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
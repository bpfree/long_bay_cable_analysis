############################
### 2. Seagrass Habitats ###
############################

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

# Dissolve seagrass habitat function
## This function will take the imported data and reduce it down to a single feature.
clean_seagrass <- function(seagrass_data){
  seagrass_layer <- seagrass_data %>%
    # reproject the coordinate reference system
    sf::st_transform("EPSG:5070") %>%
    # have only the seagrass data that exists study area
    sf::st_intersection(study_area) %>%
    # create field to define as "seagrass"
    dplyr::mutate(layer = "seagrass") %>%
    # group all features by the "layer" and "value" fields to then have a single feature
    # "value" will get pulled in from the study area layer
    dplyr::group_by(layer,
                    value) %>%
    # summarise the single feature
    dplyr::summarise()
  return(seagrass_layer)
}

#####################################
#####################################

# Set directories
## Define data directories (as this is an R Project, pathnames are simplified)
tpwd_dir <- "data/a_raw_data/tpwd-seagrass/TPWD_Seagrass"
noaa_dir <- "data/a_raw_data/Seagrasses/Seagrasses.gdb"
gulfwide_sav_dir <- "data/a_raw_data/GulfwideSAV"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
seagrass_gpkg <- "data/b_intermediate_data/gom_seagrass.gpkg"

#####################################

# View layer names within geopackage
sf::st_layers(dsn = analysis_gpkg,
              do_count = TRUE)

# View layer names within geopackage
sf::st_layers(dsn = seagrass_noaa_dir,
              do_count = TRUE)

#####################################
#####################################

# Set parameters
## designate region name
region <- "cars"

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################
#####################################

# Load seagrass layers
## TPWD Seagrass (map viewer: https://tpwd.maps.arcgis.com/apps/webappviewer/index.html?id=af7ff35381144b97b38fe553f2e7b562)
### Source: https://tpwd.texas.gov/gis/resources/tpwd-seagrass.zip)

#### TPWD Seagrass 2012 data
seagrass_tpwd <- sf::st_read(dsn = tpwd_dir, layer = "TPWD_Seagrass_8_17_12") %>%
  clean_seagrass()

#### 2015 data (original data download site: https://tpwd.texas.gov/landwater/water/habitats/coastal-fisheries-habitat-assessment-team/)
seagrass_christmas_bay <- sf::st_read(dsn = tpwd_dir, layer = "TPWD_ChristmasBay_WestBay_Seagrass") %>%
  clean_seagrass()

## NOAA Seagrass (2012)
### Note: None of these data fall within the present study area
seagrass_tpwd_noaa <- sf::st_read(dsn = tpwd_dir, layer = "NOAA_Seagrass_8_17_12") %>%
  clean_seagrass

#####################################

## NOAA US + Territories (source: ftp://ftp.coast.noaa.gov/pub/MSP/Seagrasses.zip)
### Alternative data accessed here: https://marinecadastre.gov/downloads/data/mc/Seagrass.zip (has 3 fewer features -- none in study area)
seagrass_noaa <- sf::st_read(dsn = noaa_dir, layer = "Seagrasses") %>%
  clean_seagrass() %>%
  # change the field "Shape" to be "geometry" so it matches the other data layer field names
  dplyr::rename("geometry" = "Shape")

#####################################

## Gulfwide Seagrass (source: https://www.ncei.noaa.gov/waf/data-atlas-waf/biotic/documents/GulfwideSAV.zip)
seagrass_tx_ncei <- sf::st_read(dsn = gulfwide_sav_dir, layer = "Seagrass_ALFLMSTX") %>%
  clean_seagrass()

#####################################

# Combine layers
seagrass_study_area <- seagrass_tpwd %>%
  # bind all datasets as unique rows
  rbind(seagrass_christmas_bay,
        seagrass_tpwd_noaa, # these data will be in the overall bind, but will not appear in the final data
        seagrass_noaa,
        seagrass_tx_ncei) %>%
  clean_seagrass()

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(seagrass_study_area, dsn = analysis_gpkg, layer = "seagrass", append = F)

## Seagrass geopackage
sf::st_write(seagrass_tpwd, dsn = seagrass_gpkg, layer = "seagrass_tpwd", append = F)
sf::st_write(seagrass_christmas_bay, dsn = seagrass_gpkg, layer = "seagrass_tpwd_christmas_west_bays", append = F)
sf::st_write(seagrass_tpwd_noaa, dsn = seagrass_gpkg, layer = "seagrass_noaa2012", append = F)
sf::st_write(seagrass_noaa, dsn = seagrass_gpkg, layer = "seagrass_noaa", append = F)
sf::st_write(seagrass_tx_ncei, dsn = seagrass_gpkg, layer = "seagrass_tx_ncei", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

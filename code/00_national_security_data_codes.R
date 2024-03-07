#############################################
### 0. Download Data -- national security ###
#############################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# load packages
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
               sf,
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# set parameters
region <- "vcar"
submodel <- "ns"

#####################################
#####################################

# set directories
## define data directories (as this is an R Project, pathnames are simplified)
military_installations_gdb <- "data/a_raw_data/installations_ranges/FY22_MIRTA_FINAL_V2.gdb/FY22_MIRTA_FINAL_V2.gdb/FY22_MIRTA_FINAL_V2.gdb"
danger_zones_gpkg <- "data/a_raw_data/DangerZoneRestrictedArea/DangerZoneRestrictedArea.gpkg"
fuds_gpkg <- "data/a_raw_data/FormerlyUsedDefenseSite/FormerlyUsedDefenseSite.gpkg"
military_submarine_gpkg <- "data/a_raw_data/MilitarySubmarineTransitLane/MilitarySubmarineTransitLane.gpkg"
military_collection_gpkg <-"data/a_raw_data/MilitaryCollection/MilitaryCollection.gpkg"
military_surface_gpkg <- "data/a_raw_data/MilitarySurfaceGridArea/MilitarySurfaceGridArea.gpkg"
military_operating_gpkg <- "data/a_raw_data/MilitaryOperatingAreaBoundary/MilitaryOperatingAreaBoundary.gpkg"
munitions_explosives_gpkg <- "data/a_raw_data/MunitionsExplosivesConcern/MunitionsExplosivesConcern.gpkg"

## national security submodel geopackage
national_security_geopackage <- "data/a_raw_data/national_security.gpkg"

#####################################
#####################################

# load national security datasets
## military installations
military_installations <- sf::st_read(dsn = military_installations_gdb,
                                      layer = sf::st_layers(dsn = military_installations_gdb)[[1]][2])

## danger zones and restricted areas
danger_zones <- sf::st_read(dsn = danger_zones_gpkg,
                            layer = sf::st_layers(dsn = danger_zones_gpkg)[[1]][1])

## formerly used defense sites
fuds <- sf::st_read(dsn = fuds_gpkg,
                    layer = sf::st_layers(dsn = fuds_gpkg)[[1]][1])

## military submarine transit lines
military_submarine <- sf::st_read(dsn = military_submarine_gpkg,
                                  layer = sf::st_layers(dsn = military_submarine_gpkg)[[1]][1])

## military collection
### military ship shock boxes
### military regulated airspace
### military special use airspace
military_shock <- sf::st_read(dsn = military_collection_gpkg,
                              layer = sf::st_layers(dsn = military_collection_gpkg)[[1]][4])
military_regulated_air <- sf::st_read(dsn = military_collection_gpkg,
                                      layer = sf::st_layers(dsn = military_collection_gpkg)[[1]][3])
military_special_air <- sf::st_read(dsn = military_collection_gpkg,
                                    layer = sf::st_layers(dsn = military_collection_gpkg)[[1]][5])

# military surface grid areas
military_surface <- sf::st_read(dsn = military_surface_gpkg,
                                layer = sf::st_layers(dsn = military_surface_gpkg)[[1]][1])

# military operating area boundaries
military_operating <- sf::st_read(dsn = military_operating_gpkg,
                                  layer = sf::st_layers(dsn = military_operating_gpkg)[[1]][1])

# munitions and explosives of concern
munitions_explosives <- sf::st_read(dsn = munitions_explosives_gpkg,
                                    layer = sf::st_layers(dsn = munitions_explosives_gpkg)[[1]][1])

#####################################
#####################################

# create list of the datasets
data <- list(military_installations,
             danger_zones,
             fuds,
             military_submarine,
             military_shock,
             military_regulated_air,
             military_special_air,
             military_surface,
             military_operating,
             munitions_explosives)

#####################################

# create a sequence starting from 1 to the length of the number of the datasets by an increment of 1
data_order <- seq(from = 1,
                  to = length(data),
                  by = 1)

#####################################
#####################################

# create the new codes for the datasets
## create an empty vector for the data codes
data_code <- c()

## loop through the length of datasets and add code index to the region and submodel to have total name
for(i in 1:length(data)){
  data_code[i] <- c(paste(region, submodel, data_order[i], sep = "_"))
}

#####################################
#####################################

# export all datasets with new codes
for(i in seq_along(data)){
  # grab the dataset
  dataset <- data[[i]]
  
  # export the dataset
  sf::st_write(obj = dataset, dsn = national_security_geopackage, layer = data_code[i], append = F)
}

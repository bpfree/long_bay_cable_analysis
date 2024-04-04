##########################################
### 0. Data codes -- national security ###
##########################################

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
## submodel raw data directory
data_dir <- "data/a_raw_data/national_security"

list.files(data_dir)
list.dirs(data_dir, recursive = TRUE)

## define data directories (as this is an R Project, pathnames are simplified)
military_installations_gdb <- "data/a_raw_data/national_security/"
danger_zones_gpkg <- "data/a_raw_data/national_security/"
fuds_gpkg <- "data/a_raw_data/national_security/"
military_submarine_gpkg <- "data/a_raw_data/national_security/"
military_collection_gpkg <-"data/a_raw_data/national_security/"
military_surface_gpkg <- "data/a_raw_data/national_security/"
military_operating_gpkg <- "data/a_raw_data/national_security/"
munitions_explosives_gpkg <- "data/a_raw_data/national_security/"

## national security submodel geopackage
national_security_geopackage <- "data/a_raw_data/national_security/national_security.gpkg"

#####################################
#####################################

# load national security datasets
## military installations
military_installations <- sf::st_read(dsn = file.path(data_dir, "installations_ranges/FY22_MIRTA_FINAL_V2.gdb/FY22_MIRTA_FINAL_V2.gdb/FY22_MIRTA_FINAL_V2.gdb"),
                                      layer = sf::st_layers(dsn = file.path(data_dir, "installations_ranges/FY22_MIRTA_FINAL_V2.gdb/FY22_MIRTA_FINAL_V2.gdb/FY22_MIRTA_FINAL_V2.gdb"))[[1]][2]) %>%
  # make the "MULTISURFACE" be all "MULTIPOLYGON"
  sf::st_cast(to = "MULTIPOLYGON")

## danger zones and restricted areas
danger_zones <- sf::st_read(dsn = file.path(data_dir, "DangerZoneRestrictedArea/DangerZoneRestrictedArea.gpkg"),
                            layer = sf::st_layers(dsn = file.path(data_dir, "DangerZoneRestrictedArea/DangerZoneRestrictedArea.gpkg"))[[1]][1])

## formerly used defense sites
fuds <- sf::st_read(dsn = file.path(data_dir, "FormerlyUsedDefenseSite/FormerlyUsedDefenseSite.gpkg"),
                    layer = sf::st_layers(dsn = file.path(data_dir, "FormerlyUsedDefenseSite/FormerlyUsedDefenseSite.gpkg"))[[1]][1])

## military submarine transit lines
military_submarine <- sf::st_read(dsn = file.path(data_dir, "MilitarySubmarineTransitLane/MilitarySubmarineTransitLane.gpkg"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "MilitarySubmarineTransitLane/MilitarySubmarineTransitLane.gpkg"))[[1]][1])

## military collection
### military ship shock boxes
### military regulated airspace
### military special use airspace
military_shock <- sf::st_read(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"),
                              layer = sf::st_layers(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"))[[1]][grep(pattern = "Shock",
                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"))[[1]])])
military_regulated_air <- sf::st_read(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"),
                                      layer = sf::st_layers(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"))[[1]][grep(pattern = "Regulated",
                                                                                                                                               x = sf::st_layers(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"))[[1]])])
military_special_air <- sf::st_read(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"))[[1]][grep(pattern = "Special",
                                                                                                                                             x = sf::st_layers(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"))[[1]])])

# military surface grid areas
military_surface <- sf::st_read(dsn = file.path(data_dir, "MilitarySurfaceGridArea/MilitarySurfaceGridArea.gpkg"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "MilitarySurfaceGridArea/MilitarySurfaceGridArea.gpkg"))[[1]][1])

# military operating area boundaries
military_operating <- sf::st_read(dsn = file.path(data_dir, "MilitaryOperatingAreaBoundary/MilitaryOperatingAreaBoundary.gpkg"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "MilitaryOperatingAreaBoundary/MilitaryOperatingAreaBoundary.gpkg"))[[1]][1])

# munitions and explosives of concern
munitions_explosives <- sf::st_read(dsn = file.path(data_dir, "MunitionsExplosivesConcern/MunitionsExplosivesConcern.gpkg"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "MunitionsExplosivesConcern/MunitionsExplosivesConcern.gpkg"))[[1]][1])

# Navy assessment areas
navy_assessment <- sf::st_read(dsn = file.path(data_dir, "National_Security.gdb"),
                               layer = sf::st_layers(dsn = file.path(data_dir, "National_Security.gdb"))[[1]][grep(pattern = "Navy",
                                                                                                                   x = sf::st_layers(dsn = file.path(data_dir, "National_Security.gdb"))[[1]])]) %>%
  sf::st_zm()

# NASA assessment areas
nasa_assessment <- sf::st_read(dsn = file.path(data_dir, "National_Security.gdb"),
                               layer = sf::st_layers(dsn = file.path(data_dir, "National_Security.gdb"))[[1]][grep(pattern = "NASA",
                                                                                                                   x = sf::st_layers(dsn = file.path(data_dir, "National_Security.gdb"))[[1]])]) %>%
  sf::st_zm()

# Air Force assessment areas
air_force_assessment <- sf::st_read(dsn = file.path(data_dir, "National_Security.gdb"),
                               layer = sf::st_layers(dsn = file.path(data_dir, "National_Security.gdb"))[[1]][grep(pattern = "AirForce",
                                                                                                                   x = sf::st_layers(dsn = file.path(data_dir, "National_Security.gdb"))[[1]])]) %>%
  sf::st_zm()

# military flight track
military_flight <- sf::st_read(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"),
                              layer = sf::st_layers(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"))[[1]][grep(pattern = "Flight",
                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "MilitaryCollection/MilitaryCollection.gpkg"))[[1]])])

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
             munitions_explosives,
             navy_assessment,
             nasa_assessment,
             air_force_assessment,
             military_flight)

#####################################

# create a sequence starting from 1 to the length of the number of the datasets by an increment of 1
data_order <- seq(from = 1,
                  to = length(data),
                  by = 1)

## add extra "0" when needed to make all numbers three digits
for(i in 1:length(data_order)){
  data_order[i] <- ifelse(nchar(data_order[i]) < 2, paste0("00", data_order[i]),
                          ifelse(nchar(data_order[i]) == 2, paste0("0", data_order[i]), data_order[i]))
}

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

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

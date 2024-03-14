##################################################
### 0. Data Codes -- fisheries and aquaculture ###
##################################################

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
               RSQLite,
               sf,
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# set parameters
region <- "vcar"
submodel <- "fa"

#####################################
#####################################

# set directories
## submodel raw data directory
data_dir <- "data/a_raw_data/fisheries_aquaculture"

list.files(data_dir)
list.dirs(data_dir, recursive = TRUE)

## fisheries and aquaculture submodel geopackage
fisheries_aquaculture_geopackage <- "data/a_raw_data/fisheries_aquaculture/fisheries_aquaculture.gpkg"

#####################################
#####################################

# load fisheries and aquaculture datasets
## South Carolina trawl area boundaries
sc_trawl <- sf::st_read(dsn = file.path(data_dir, "SC_Official_Trawl_Area_Boundaries/SC_Official_Trawl_Area_Boundaries.shp"))

## North Carolina public fishing areas
nc_pfa <- sf::st_read(dsn = file.path(data_dir, "pfa/PFA.shp"))

## Virginia clamming grounds
clams <- sf::st_read(dsn = file.path(data_dir, "Clams.kml")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

## Virginia Baylor grounds
baylor <- sf::st_read(dsn = file.path(data_dir, "Baylor.kml")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

## North Carolina crab trawl line
nc_crab <- sf::st_read(dsn = file.path(data_dir, "crab_trawl_line/Crab Trawl Line.shp"))

## rock shrimp VMS
shrimp_vms <- sf::st_read(dsn = file.path(data_dir, "shrimp_vms/shrimp_vms.shp"))

## South Carolina shrimp landings
sc_shrimp_landings <- sf::st_read(dsn = file.path(data_dir, "sc_shrimp/shrimp_landings_south_carolina.shp"))

## aquaculture
aquaculture <- sf::st_read(dsn = file.path(data_dir, "aquaculture/aquaculture.gpkg"),
                           layer = sf::st_layers(file.path(data_dir, "aquaculture/aquaculture.gpkg"))[[1]][1])

## commercial fish landing summary
commercial_fish <- sf::st_read(dsn = file.path(data_dir, "CommercialFishLandingSummary/CommercialFishLandings.gdb"),
                           layer = sf::st_layers(file.path(data_dir, "CommercialFishLandingSummary/CommercialFishLandings.gdb"))[[1]][1])

## ACCSP fish landings (area code)
fish_landings_area_code <- sf::st_read(dsn = file.path(data_dir, "fish_landings/fish_landings.shp"))

## ACCSP fish landings (latitude and species)
fish_landings_species <- sf::st_read(dsn = file.path(data_dir, "fish_landings_lat_spp/accsp_fish_landings_by_latitude_and_species.shp"))

## North Carolina shrimp landings
nc_shrimp <- sf::st_read(dsn = file.path(data_dir, "nc_shrimp/north_carolina_shrimp_landings.shp"))

#####################################
#####################################

# create list of the datasets
data <- list(sc_trawl,
             nc_pfa,
             clams,
             baylor,
             nc_crab,
             shrimp_vms,
             sc_shrimp_landings,
             aquaculture,
             commercial_fish,
             fish_landings_area_code,
             fish_landings_species,
             nc_shrimp)

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
  sf::st_write(obj = dataset, dsn = fisheries_aquaculture_geopackage, layer = data_code[i], append = F)
}

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

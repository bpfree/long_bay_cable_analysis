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

# fishing nets
fyke_nets_points <- sf::st_read(dsn = file.path(data_dir, "fn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "POINT"))

fyke_nets_lines <- sf::st_read(dsn = file.path(data_dir, "fn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "LINESTRING")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

pound_nets_points <- sf::st_read(dsn = file.path(data_dir, "pn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "POINT"))

pound_nets_lines <- sf::st_read(dsn = file.path(data_dir, "pn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "LINESTRING")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

staked_gill_nets_points <- sf::st_read(dsn = file.path(data_dir, "sgn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "POINT"))

staked_gill_nets_lines <- sf::st_read(dsn = file.path(data_dir, "sgn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "LINESTRING")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

vms_demarcation <- sf::st_read(dsn = file.path(data_dir, "vms_demarcation_line_20140925?null/VMS_Demarcation_Line/VMS_Demarcation_Line.shp"))



clam_amendment <- sf::st_read(dsn = file.path(data_dir, "ClamAmendmentAreas/ClamAmendmentAreas.shp"))
# Atlantic Large Whale Take Reduction Plan Regulated and Exempted Waters
coral_amendment <- sf::st_read(dsn = file.path(data_dir, "CoralAmendmentAreas/CoralAmendmentAreas.shp"))
fishery_management_area <- sf::st_read(dsn = file.path(data_dir, "FisheryManagementAreasSample/FisheryManagementAreasSample.shp"))
# VMS All 2015 - 2019
# VMS Declared Out of Fishery 2015 - 2019
# VMS Monkfish May 2015 - April 2019
# VMS Multispecies May 2015 - April 2019
# VMS Pelagics 2015 - 2016 Less than 4 knots
# VMS Scallops April 2015 - March 2019
# VMS Squid 2015 - 2016 Less than 4 knots
# VMS Squid/Mackerel/Butterfish 2015 - 2019
# Communities at Sea - Total Bottom Trawl > 65 ft Activity 2011 - 2015
# Communities at Sea - Total Dredge Activity 2011 - 2015
# Communities at Sea - Total Gillnet Activity 2011 - 2015
# Communities at Sea - Total Longline Activity 2011 - 2015
# Communities at Sea - Total Pots and Traps Activity 2011 - 2015
# Communities at Sea - Total Bottom Trawl < 65 ft Activity 2011 - 2015
# Communities at Sea - Total Lobster Activity 2011 - 2015
# Communities at Sea - Total Party/Charter Activity 2011 - 2015
# Communities at Sea - Total Shrimp Activity 2011 - 2015

nc_sga_classifications <- sf::st_read(dsn = file.path(data_dir, "SGA_Current_Classifications/SGA_Current_Classifications.shp"))
sc_shellfish_harvest_classifications <- sf::st_read(dsn = file.path(data_dir, "Shellfish_Harvest_Classifications/Shellfish_Harvest_Classifications.shp"))
nc_cultch_1981_2002 <- sf::st_read(dsn = file.path(data_dir, "nc_cultch_1981_2002/CultchApp2022.shp"))
nc_cultch_2003_present <- sf::st_read(dsn = file.path(data_dir, "nc_cultch_2003_present/CultchApp2022.shp"))

open_harvest <- sf::st_read(dsn = file.path(data_dir, "OpenHarvest.kml")) %>%
  sf::st_make_valid() %>%
  sf::st_zm()
oyster_garden <- sf::st_read(dsn = file.path(data_dir, "OyGarden.kml"))
lease_applications <- sf::st_read(dsn = file.path(data_dir, "LeaseApplications.kml")) %>%
  sf::st_make_valid() %>%
  sf::st_zm()
private_leases <- sf::st_read(dsn = file.path(data_dir, "PrivateLeases.kml")) %>%
  sf::st_zm()

# NC Designated Pot Area (DPA)
# NC Attended Gillnet Areas
# NC Mechanical Clam Harvest Areas 2016

sc_shellfish_permit <- sf::st_read(dsn = file.path(data_dir, "MariculturePermitData_Public_View/MariculturePermitData_Public_View.shp"))
sc_monitoring_stations <- sf::st_read(dsn = file.path(data_dir, "monitoring_stations_shellfish/Monitoring_Stations_-_Shellfish.shp"))

lobster_gear <- sf::st_read(dsn = file.path(data_dir, "lobster_gear_areas_20160501/Lobster_Gear_Areas/Lobster_Gear_Areas.shp"))
illex_fishery <- sf::st_read(dsn = file.path(data_dir, "illex_fishery_mesh_exemption_area_20140501/Illex_Fishery_Mesh_Exemption_Area/Illex_Fishery_Mesh_Exemption_Area.shp"))
altantic_red_drum <- sf::st_read(dsn = file.path(data_dir, "atlantic-red-drum-fishery-harvest-or-possession-prohibition-area-20140915-noaa-garfo/Atlantic_Red_Drum_Fishery_Harvest_or_Possession_Prohibition_Area/Atlantic_Red_Drum_Fishery_Harvest_or_Possession_Prohibition_Area.shp"))

sne_ne_dogfish <- sf::st_read(dsn = file.path(data_dir, "sne-dogfish-gillnet-exemption-area-20150315-noaa-garfo/SNE_Dogfish_Gillnet_Exemption_Area/SNE_Dogfish_Gillnet_Exemption_Area.shp"))



# 
# Southern New England Regulated Mesh Area
# Southern New England Exemption Area
# Scup Transfer-at-Sea Boundary
# Southern New England Monkfish and Skate Trawl Exemption Area
# Southern New England Monkfish and Skate Gillnet Exemption Area
# Atlantic Sea Scallop Rotational Areas
# VA Pound Net Regulated Area
# Cape Lookout Lophelia Banks Deepwater Coral HAPC Fishery Management Area
# Cape Lookout Lophelia Banks Deepwater Coral HAPC Extension Fishery Management Area
# Cape Fear Lophelia Banks Deepwater Coral HAPC Fishery Management Area
# Stetson-Miami Terrace Deepwater Coral HAPC Fishery Management Area
# Blake Ridge Diapir Deepwater Coral HAPC Fishery Management Area
# Marine Protected Areas (MPAs) Fishery Management Areas
# Defined Fishery Management Areas Off South Atlantic States
# Commercial Vessel Permits for South Atlantic Snapper-Grouper Fishery Management 
# Sea Bass Pot and Associated Buoy Gear Identification Fishery Management Area
# Special Management Zones (SMZs) Fishery Management Areas
# Longline Prohibited Areas Fishery Management Areas
# Spawning Special Management Zones (SMZs) Fishery Management Area
# Commercial Black Sea Bass Pot Closure for November & April Fishery Management Area
# Commercial Black Sea Bass Pot Closure Dec 1 - Mar 31 Fishery Management Area
# South Atlantic Shrimp Cold Weather Closure Fishery Management Area
# Allowable Octocoral Closed Area Fishery Management Area
# Golden Crab Trap Fishing Zones and Closed Fishery Management Areas
# Charleston Bump Closed Area Fishery Management Area
# Pelagic Sargassum Habitat Area & Seasonal Restrictions Fishery Management Area
# King Mackerel Migratory Group Zones Fishery Management Areas
# Spanish Mackerel Migratory Group Zones Fishery Management Areas
# Cobia Migratory Group Zones Fishery Management Areas
# NC Designated Seed Oyster Management Area
# NC River Herring Management Area
# NC Striped Bass Management Areas
# SC Shellfish Management Areas
# NC CRAB HARVEST MANAGEMENT AREAS
# Monkfish Fishery Management Areas
# Skate Management Unit
# Management Units for Summer Flounder, Scup, and Black Sea Bass
# NC Oyster Sanctuaries
# VA Shellfish Condemnation Zones by VDH
# Bottom Longlines Restrictions
# Fish Traps Restrictions
# Octocoral Gear Restrictions
# Roller Rig Trawls Restrictions
# Sargassum Restrictions
# NC Mechanical Methods Prohibited
# NC Shrimp Trawl Prohibited Area
# NC Pound Net Prohibited Area
# NC Trawl Net Prohibited Areas
# Scup Gear Restricted Areas
# SAFMC Restrictions

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
             nc_shrimp,
             fkye_nets_points,
             fkye_nets_lines,
             pound_nets_points,
             pound_nets_lines,
             staked_gill_nets_points,
             staked_gill_nets_lines,
             vms_demarcation)

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

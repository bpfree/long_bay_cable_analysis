############################################
### 0. Data codes -- metocean and others ###
############################################

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
submodel <- "mo"

#####################################
#####################################

# set directories
## define data directories (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data/metocean_other"

list.files(data_dir)
list.dirs(data_dir, recursive = TRUE)

## national security submodel geopackage
metocean_other_geopackage <- "data/a_raw_data/metocean_other/metocean_other.gpkg"

#####################################
#####################################

# load metocean and other datasets
## South Carolina general geology
sc_geology <- sf::st_read(dsn = file.path(data_dir, "sc_geology.gpkg"),
                          layer = sf::st_layers(dsn = file.path(data_dir, "sc_geology.gpkg"))[[1]][1])

## South Carolina storm tracks
sc_storms <- sf::st_read(dsn = file.path(data_dir, "SC_Hurricanes_Public/SC_Hurricanes_Public.shp"),
                         layer = sf::st_layers(dsn = file.path(data_dir, "SC_Hurricanes_Public/SC_Hurricanes_Public.shp"))[[1]][1])

## South Carolina approved TMDL sites

## section 319 grant projects

## North Carolina NPDES stormwater permits

## Norht Carolina state stormwater permits

## North Carolina no exposure certifications

## North Carolina NPDES wastewater discharge permits

## North Carolina IHA environmental concern

## North Carolina oceanfront erosion rates
### 2020

### 20213

## North Carolina oceanfront shorelines (1848 - 2016)

## North Carolina ESMP structures (2010 - 2012)

## North Carolina estuarine shoreline (2010)

## North Carolina estuarine shoreline (2012)

## Southeast bathymetry

## South Carolina ESI 2015

## North Carolina ESI 2016

## tropical cyclone wind exposure -- North Atlantic

## sea surface height

## bathymetric contours
bathymetry_contours <- file.path(data_dir, "BathymetricContour/BathymetryContours.gdb")

## offshore wind resource potential -- Atlantic

## ENOW (2015)
### state statistics
enow <- file.path(data_dir, "ENOW2015/ENOW2015.gdb")

### state percentages

## undersea feature names

## mid-Atlantic benthic habitats

## submarine canyons

## middle Virginia (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## southern Virginia (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## Virginia SVI (2010)

## North Carolina AST incidents (petrolium spills)

## North Carolina UST incidents (petrolium spills)

## North Carolina hazardous waste sites

## North Carolina notice land use restrictions view

## North Carolina pre-regulatroy landfill sites

## North Carolina dry cleaning historical boiler inspections
cleaning_gpkg <- file.path(data_dir, "dry_cleaning.gpkg")

## acidification monitoring locations
acidification <- sf::st_read(dsn = file.path(data_dir, "AcidificationMonitoringMidA_Ver202310/AcidificationMonitoring_MidA_ver202310/AcidificationMonitoringMidA_ver202310.gdb"),
                             layer = sf::st_layers(file.path(data_dir, "AcidificationMonitoringMidA_Ver202310/AcidificationMonitoring_MidA_ver202310/AcidificationMonitoringMidA_ver202310.gdb"))[[1]][1])

## climate change vulnerabilities (compile)
ccv_gdb <- file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CC_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb")

## climate change vulnerabilities (employment)

## climate change vulnerabilities (fisheries)

## climate change vulnerabilities (flooding summary)

## climate change vulnerabilities (high)

## climate change vulnerabilities (ranking)

## climate change vulnerabilities (ocean employment)

## climate change vulnerabilities (population and housing ranking)

## climate change vulnerabilities (population and housing summary)

## climate change vulnerabilities (major road and highways)

## climate change vulnerabilities (social vulnerability summary)

## intrinsic seabed vulnerabilities (bottom trawl, median fishing effort)

## intrinsic seabed vulnerabilities (gillnet, median fishing effort)

## intrinsic seabed vulnerabilities (hydraulic clam dredge, median fishing effort)

## intrinsic seabed vulnerabilities (longline, median fishing effort)

## intrinsic seabed vulnerabilities (scallop dredge, median fishing effort)

## intrinsic seabed vulnerabilities (trap, median fishing effort)

## subcatchments

## VPDES permit outfalls

## VWP (general permits)

## VWP (individual permits)

## VWP (non-permits)

## WQA monitoring stations (2018)

## water quality monitoring plan stations (current)

## Chesapeake Bay ESI
### hydro -- line
cb_hydro_l <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                          layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "HYDROL",
                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])
### hydro -- polygon

### natural hazards

### political

### management

## middle Atlantic bight
### bottom shear stress (median)

### bottom shear stress (half interpercentile)

### bottom shear stress (95th percentile)

### seabed mobility percentage

### seabed mobility recurrence interval

## south Atlantic bight
### bottom shear stress (median)

### bottom shear stress (half interpercentile)

### bottom shear stress (95th percentile)

### seabed mobility percentage

### seabed mobility recurrence interval

## fishing effects model
### boulder

### cobbler

### granule and pebble

### mud

### sand

### steep / deep

### sediment data density

### sediment diversity

## northern North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## middle 1 North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## middle 2 North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## southern 1 North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## southern 2 North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## northern South Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## central South Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## southern South Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## South Carolina beachfront jurisdictional lines (setback)

## South Carolina beachfront jurisdictional lines (baseline)

## North Carolina no-wake zones

## North Carolina DWR regional office service areas

## North Carolina DBCJIW rule 2022(Coastal, join, inland waters)

## North Carolina ITP management areas

## limit of OCSLA 8(g) zone

## Submerged Land Acts boundary (3nm)

## South Carolina ESI (1996) managed lands for human uses

## South Carolina ESI (2015) parks and managed areas

## North Carolina ESI (2016) parks and managed areas

## North Carolina (2011) management areas

## marine ecoregions of the world

## deepwater marine protected areas

## wind stipulation areas

## coastal populated places
populated_gpkg <- file.path(data_dir, "CoastalPopulatedPlace/CoastalPopulatedPlace.gpkg")

## federal and state waters
federal_state_waters <- sf::st_read(dsn = file.path(data_dir, "FederalStateWaters/FederalAndStateWaters.gdb"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "FederalStateWaters/FederalAndStateWaters.gdb"))[[1]][1])

## coastal states
coastal_state <- sf::st_read(dsn = file.path(data_dir, "CoastalState/CoastalState.gpkg"),
                              layer = sf::st_layers(dsn = file.path(data_dir, "CoastalState/CoastalState.gpkg"))[[1]][1])

## coastal counties
coastal_county <- sf::st_read(dsn = file.path(data_dir, "CoastalCounty/CoastalCounty.gpkg"),
                              layer = sf::st_layers(dsn = file.path(data_dir, "CoastalCounty/CoastalCounty.gpkg"))[[1]][1])

## federal statutes


## state census statistics
census_gdb <- file.path(data_dir, "CensusStatistics.gdb")

## county census statistics
census_gdb <- file.path(data_dir, "CensusStatistics.gdb")

## Coastal Zone Management Act boundary
management_gpkg <- file.path(data_dir, "CoastalZoneManagementAct/CoastalZoneManagementAct.gpkg")

## National Marine Fisheries Service regions

## Atlantic administrative boundaries

## BOEM outer continental self areas withdrawn from leasing
withdraw_gdb <- file.path(data_dir, "boemoscareas-withdrawn-leasing-2021/Withdraw2021.gdb")

## National Park Service boundaries

## Native American reservation and land trusts

## United States census tracts (MARCO coastal zone and vicinity)


## federal consistency geographic location descriptions
federal_gpkg <- file.path(data_dir, "FederalConsistencyGeographicLocationDescription/FederalConsistencyGeographicLocationDescription.gpkg")

## United States maritime limits boundaries

## ShellBase (southeast shellfish water quality database for South Carolina coastal waters -- 1980s - 2020)

## ShellBase (southeast shellfish water quality database for North Carolina -- 1980s - 2020)

## current speed / direction

## significant wave height

## wave direction

## wave energy period

## wind speed and direction

## current speed and direction
current_speed_direction <- sf::st_read(dsn = file.path(data_dir, "CurrentSpeedDirection/Current_U_V_M.gdb"),
                                       layer = sf::st_layers(dsn = file.path(data_dir, "CurrentSpeedDirection/Current_U_V_M.gdb"))[[1]][1])

## significant wave height

## EMU -- water temperature, salinity, dissolved oxygen
emu_water_gdb <- file.path(data_dir, "EMUWaterQuality/EMU_WaterQuality.gdb")

## EMU -- nitrates, phosphates, silicates
emu_nutrient_gdb <- file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb")


#####################################
#####################################

# create list of the datasets
data <- list()

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

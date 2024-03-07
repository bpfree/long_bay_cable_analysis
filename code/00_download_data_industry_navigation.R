###################################################
### 0. Download Data -- industry and navigation ###
###################################################

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

# Commentary on R and code formulation:
## ***Note: If not familiar with dplyr notation
## dplyr is within the tidyverse and can use %>%
## to "pipe" a process, allowing for fluidity
## Can learn more here: https://style.tidyverse.org/pipes.html

## Another common coding notation used is "::"
## For instance, you may encounter it as dplyr::filter()
## This means "use the filter function from the dplyr package"
## Notation is used given sometimes different packages have
## the same function name, so it helps code to tell which
## package to use for that particular function.
## The notation is continued even when a function name is
## unique to a particular package so it is obvious which
## package is used

#####################################
#####################################

# Create function that will pull data from publicly available websites
## This allows for the analyis to have the most current data; for some
## of the datasets are updated with periodical frequency (e.g., every 
## month) or when needed. Additionally, this keeps consistency with
## naming of files and datasets.
### The function downloads the desired data from the URL provided and
### then unzips the data for use

data_download_function <- function(download_list, data_dir){
  
  # loop function across all datasets
  for(i in 1:length(download_list)){
    
    # designate the URL that the data are hosted on
    url <- download_list[i]
    
    # file will become last part of the URL, so will be the data for download
    file <- basename(url)
    
    # Download the data
    if (!file.exists(file)) {
      options(timeout=1000)
      # download the file from the URL
      download.file(url = url,
                    # place the downloaded file in the data directory
                    destfile = file.path(data_dir, file),
                    mode="wb")
    }
    
    # Unzip the file if the data are compressed as .zip
    ## Examine if the filename contains the pattern ".zip"
    ### grepl returns a logic statement when pattern ".zip" is met in the file
    if (grepl(".zip", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- sub(".zip", "", file)
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
    
    dir <- file.path(data_dir, new_dir_name)
  }
}

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data"

#####################################
#####################################

# download data
## industry and navigation
### 2019-2024 OCS Oil and Gas Leasing Draft Proposed Program
atl_oil_gas <- "https://www.boem.gov/atl-5yr-2019-2024.zip"

### SC Environment Sensitivity Index 2015 Navigation Marine
sc_esi_15 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_2015_GDB.zip"

### SC Environment Sensitivity Index 2015 Navigation Marine
sc_esi_16 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2016_GDB.zip"

### anchorage areas
anchorage_areas <- "https://marinecadastre.gov/downloads/data/mc/Anchorage.zip"

### pilot broading areas
pilot_areas <- "https://marinecadastre.gov/downloads/data/mc/PilotBoardingArea.zip"

### pilot broading stations
pilot_areas <- "https://marinecadastre.gov/downloads/data/mc/PilotBoarding.zip"

### principal ports
principal_ports <- "https://marinecadastre.gov/downloads/data/mc/PrincipalPort.zip"

### coastal maintained channels
navigation_channels <- "https://marinecadastre.gov/downloads/data/mc/NavigationChannel.zip"

### wrecks and obstructions
wrecks_obstructions <- "https://marinecadastre.gov/downloads/data/mc/WreckObstruction.zip"

### pipelines
pipelines <- "https://marinecadastre.gov/downloads/data/mc/Pipeline.zip"

### pipeline areas
pipeline_areas <- "https://marinecadastre.gov/downloads/data/mc/PipelineArea.zip"

### submarine cables
submarine_cable <- "https://marinecadastre.gov/downloads/data/mc/SubmarineCable.zip"

### submarine cable areas
submarine_cable_areas <- "https://marinecadastre.gov/downloads/data/mc/SubmarineCableArea.zip"

### wastewater outfalls
#### ***note: wastewater outfalls covers:
####          1.) wastewater outfalls (facilities)
####          2.) wastewater outfalls (outfall)
####          3.) wastewater outfalls (pipes)
wastewater_outfalls <- "https://marinecadastre.gov/downloads/data/mc/WastewaterOutfall.zip"

### lightering zones
lightering_zones <- "https://marinecadastre.gov/downloads/data/mc/LighteringZone.zip"

### aids to navigation
aids_navigation <- "https://marinecadastre.gov/downloads/data/mc/AtoN.zip"

### offshore wind planning areas
### offshore wind energy leases
offshore_wind <- "https://www.boem.gov/BOEM-Renewable-Energy-Geodatabase.zip"

### coastal energy facilities
energy_facilities <- "https://marinecadastre.gov/downloads/data/mc/CoastalEnergyFacility.zip"

### electric power substations
electric_substations <- "https://marinecadastre.gov/downloads/data/mc/ElectricPowerSubstation.zip"

### OCS Block Areas with Sand Resources - Atlantic
sand_blocks <- "https://mmis.doi.gov/boemmmis/downloads/layers/ATLSandAliquots_shp.zip"

### Beach Nourishment Projects
beach_nourishment <- "https://marinecadastre.gov/downloads/data/mc/BeachNourishment.zip"

### Surficial Sediment Texture
surficial_sediment <- "https://marinecadastre.gov/downloads/data/mc/SurficialSedimentClassification.zip"

### Ocean Disposal Sites
ocean_disposal <- "https://marinecadastre.gov/downloads/data/mc/OceanDisposalSite.zip"

### Shipping Fairways Lanes and Zones
shipping_lanes <- "http://encdirect.noaa.gov/theme_layers/data/shipping_lanes/shippinglanes.zip"

### VA Fixed Gear: Fkye Nets
fkye_nets <- "https://webapps.mrc.virginia.gov/public/maps/kml/FN.kmz"

### VA Fixed Gear: Pound Nets
pound_nets <- "https://webapps.mrc.virginia.gov/public/maps/kml/PN.kmz"

### VA Fixed Gear: Staked Gill Nets
staked_gill <- "https://webapps.mrc.virginia.gov/public/maps/kml/SGN.kmz"

### VA Underwater Exploration Exclusion Zones
underwater_exploration <- "https://webapps.mrc.virginia.gov/public/maps/kml/Exploration_ExclusionZones.kml"




### Proposed Passive Acoustic Monitoring Network
passive_acoustic <- "https://www.northeastoceandata.org/files/metadata/Themes/ProposedPassiveAcousticNetwork.zip"

### AIS Vessel Tracks 2022
ais <- "https://marinecadastre.gov/downloads/data/ais/ais2022/AISVesselTracks2022.zip"

### 2022 Vessel Transit Counts: All
### 2022 Vessel Transit Counts: Cargo
### 2022 Vessel Transit Counts: Fishing
### 2022 Vessel Transit Counts: Other
### 2022 Vessel Transit Counts: Passenger
### 2022 Vessel Transit Counts: Pleasure Craft/Sailing
### 2022 Vessel Transit Counts: Tanker
### 2022 Vessel Transit Counts: Tug-Tow
ais_all <- "https://services.northeastoceandata.org/downloads/AIS/AIS2022_Annual.zip"

### Proposed Right Whale Seasonal Speeding Zones
right_whale <- "https://www.fisheries.noaa.gov/s3/2023-05/Proposed-Right-Whale-Seasonal-Speed-Zones.zip"

### NC Underground Storage Tank Active Facilities
storage_tank <- "https://data-ncdenr.opendata.arcgis.com/datasets/ncdenr::ust-active-facilities/about"

### Offshore Wind Planned Ports 2023
planned_ports <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Maritime/OffshoreWindPlannedPorts2023_MidAtl.zip"

### Registered Petroleum Tank Facilities
petroleum_tank <- "https://geohub-vadeq.hub.arcgis.com/api/download/v1/items/137437097e1444a6aed31081b9812330/shapefile?layers=102"

### Petroleum Release Sites
petroleum_release <- "https://geohub-vadeq.hub.arcgis.com/api/download/v1/items/57759688e4944bb987add68c4f0c5ada/shapefile?layers=104"

### VMS Demarcation Line
vms_demarcation <- "https://www.fisheries.noaa.gov/s3/2020-04/vms_demarcation_line_20140925.zip?null"

### Chesapeake Bay Environmental Sensitivty Index: Navigational Marine
cb_esi <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/ChesapeakeBay_2016_GDB.zip"




### NC Division of Aviation Airport Points
### NC Division of Aviation Airport Pavements
### NC Federal Navigation Channels
### Ferry Routes
### Navigable Waterway Network Lines

### Electric Transmission Lines


### Intermodal Freight Facilities Marine Roll-on/Roll-off


### Marine Highways
### Offshore Wind Energy Interarray Cables


### Virginia Beach Channel Markers and Navigation Aids
### Offshore Wind - Export Cable Locations (Proposed or Installed)

#####################################
#####################################

# Download list
download_list <- c(
  
)

data_download_function(download_list, data_dir)

#####################################
#####################################

# list all files in data directory
list.files(data_dir)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

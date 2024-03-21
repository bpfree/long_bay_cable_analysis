##############################################
### 0. Download Data -- natural resources ###
##############################################

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

i <- 1
download_list <- c("https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:coastmigpelhapc")

data_download_function <- function(download_list, data_dir){
  
  # loop function across all datasets
  for(i in 1:length(download_list)){
    
    # designate the URL that the data are hosted on
    url <- download_list[i]
    
    # file will become last part of the URL, so will be the data for download
    file <- basename(url)
    
    # Download the data
    if (!file.exists(file)) {
      options(timeout=100000)
      # download the file from the URL
      download.file(url = url,
                    # place the downloaded file in the data directory
                    destfile = file.path(data_dir, file),
                    mode="wb")
    }
    
    if (grepl("coastmigpelhapc", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "coastmigpelhapc"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      file <- list.files(data_dir)[grep(pattern = "coastmigpelhapc",
                                        x = list.files(data_dir))]
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
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
    
    if (grepl("682786", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "nps_historic"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
    
    if (grepl(".kmz", file)){
      
      ## Virginia habitat permit applications (2023)
      file.rename(from=file.path(data_dir, file),  # Make default download directory flexible
                  # send to the raw data directory
                  to=file.path(data_dir, "va_habitat.zip"))
      
      unzip(zipfile = file.path(data_dir, "va_habitat.zip"),
            # export file to the new data directory
            exdir = data_dir)
      
      file.rename(from=file.path(data_dir, "doc.kml"),  # Make default download directory flexible
                  # send to the raw data directory
                  to=file.path(data_dir, "va_habitat_permit.kml"))
      
      ## remove original zipped file
      file.remove(file.path(data_dir, "va_habitat.zip"))
    }
  }
}

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data/natural_resources"

#####################################
#####################################

# download data
## natural resources
"https://ncnhde.natureserve.org/system/files/nhna_NaturalArea_2024-01.zip"
"https://ncnhde.natureserve.org/system/files/Biodiversity_and_Habitat_Assessment_2022-07_0.zip"
"https://ncnhde.natureserve.org/system/files/marea_ManagedArea_2024-01.zip"












"https://media.fisheries.noaa.gov/2023-09/summer-flounder-fishery-sea-turtle-protection-area-20140501-1.zip"

"https://media.fisheries.noaa.gov/2020-04/bdtrp_n_nc_po.zip"
"https://media.fisheries.noaa.gov/2020-04/bdtrp_sc_fl_po_0.zip"




"https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_1996_GDB.zip"

"https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_2015_GDB.zip"

"https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2016_GDB.zip"

"https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2011_GDB.zip"

"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:coralhapc"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:coastmigpelhapc"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:dolphin_wahoo_efh_hapc"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:snapper_grouper_efh_hapc"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:tilefish_efh_hapc"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:snapper_grouper_efh"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:shrimpefh"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:dolphin_wahoo_efh"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:spinylobsterefh"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:goldencrabefh"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:deepwater_coral_hapcs"
"https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip"
"https://data.axds.co/gs/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa_shorebirds"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:seamap_turtle"

"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:in_water_kemps"
"https://marinecadastre.gov/downloads/data/mc/CoastalCriticalHabitat.zip"
"https://marinecadastre.gov/downloads/data/mc/EFHHighlyMigratorySpecies.zip"
"http://cetsound.noaa.gov/Assets/cetsound/data/CetMap_BIA_WGS84.zip"
"https://marinecadastre.gov/downloads/data/mc/AudubonImportantBirdArea.zip"
"https://marinecadastre.gov/downloads/data/mc/ProtectedArea.zip"
"https://marinecadastre.gov/downloads/data/mc/DeepSeaCoralObservation.zip"
"https://marinecadastre.gov/downloads/data/mc/DeepSeaCoralHabitatSuitability.zip"
"https://marinecadastre.gov/downloads/data/mc/CoastalBarrierResourceAreas.zip"






"https://webapps.mrc.virginia.gov/public/maps/kml/prfcWeb.kml"
"https://webapps.mrc.virginia.gov/public/maps/kml/State.kmz"
"https://webapps.mrc.virginia.gov/public/maps/kml/VIMS_SAV_2023_p1.kmz"

"https://dwr.virginia.gov/-/gis-data/VDGIF_Wildlife_Management_Area_WMA_Boundaries.zip"
"https://dwr.virginia.gov/-/gis-data/pca_shapefile.zip"
"https://dwr.virginia.gov/-/gis-data/veva_shapefile.zip"
"https://dwr.virginia.gov/-/gis-data/Tier1_EssHab.zip"
"https://dwr.virginia.gov/-/gis-data/EssHabTier1_2.zip"
"https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Fishing/ArtificialReefs2023FebUpdate.zip"

"https://sanctuaries.noaa.gov/media/gis/mbpr_py.zip"
"https://marinecadastre.gov/downloads/data/mc/NationalEstuarineResearchReserveSystem.zip"
"https://marinecadastre.gov/downloads/data/mc/CoastalWetland.zip"


"https://media.fisheries.noaa.gov/2020-04/frank_r_lautenberg_deep_sea_coral_protection_areas_20180409.zip"
"https://media.fisheries.noaa.gov/2020-04/hptrp_mid-atlantic_regulated_and_exempted_waters_20140915.zip"
"https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/ChesapeakeBay_2016_GDB.zip"






"https://marinecadastre.gov/downloads/data/mc/Seagrass.zip"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:coral_mounds"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:artificial_reefs"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:north_american_right_whale"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:pipingplover_se"

"https://media.fisheries.noaa.gov/2020-04/shapefile-cape-lookout-orig-ext.zip"

"https://media.fisheries.noaa.gov/2020-04/capefear.zip"
"https://media.fisheries.noaa.gov/2020-04/stetson_miami.zip"
"https://media.fisheries.noaa.gov/2020-04/blakeridge.zip"
"https://www.sciencebase.gov/catalog/file/get/64f8da38d34ed30c20546a6a?name=Southeast_Blueprint_2023_Data_Download.zip"






"https://marinecadastre.gov/downloads/data/mc/NorthAtlanticRightWhaleSMA.zip"

"https://media.fisheries.noaa.gov/2020-04/mpas.zip"
"https://www.fisheries.noaa.gov/s3/2020-04/sa_eez_off_states.zip"
"https://media.fisheries.noaa.gov/2020-04/comm_permits_sa_sg.zip"
"https://media.fisheries.noaa.gov/2020-04/seabass_potid.zip"

"https://media.fisheries.noaa.gov/2020-04/ll_prohibareas_n_s.zip"
"https://media.fisheries.noaa.gov/2020-04/spawning_smzs.zip"
"https://media.fisheries.noaa.gov/2020-04/bsb_pot_nov_apr.zip"
"https://media.fisheries.noaa.gov/2020-04/bsb_pot_dec_mar.zip"
"https://media.fisheries.noaa.gov/2020-04/sa_shrimp_cold_weather.zip"
"https://media.fisheries.noaa.gov/2020-04/octocoral.zip"
"https://media.fisheries.noaa.gov/2020-04/goldencrab.zip"
"https://media.fisheries.noaa.gov/2020-04/pelagicll_charleston.zip"
"https://media.fisheries.noaa.gov/2020-04/pelagic_sargassum.zip"
"https://media.fisheries.noaa.gov/2020-04/king_mackerel.zip"
"https://media.fisheries.noaa.gov/2020-04/spanish_mackerel_.zip"
"https://media.fisheries.noaa.gov/2020-04/cobia.zip"









"https://webapps.mrc.virginia.gov/public/maps/kml/crabs.kml"
"https://webapps.mrc.virginia.gov/public/maps/kml/OpenHarvest.kmz"
"https://webapps.mrc.virginia.gov/public/maps/kml/OyGarden.kmz"
"https://webapps.mrc.virginia.gov/public/maps/kml/LeaseApplications.kmz"
"https://webapps.mrc.virginia.gov/public/maps/kml/oyster_sanctuaries.kml"
"https://webapps.mrc.virginia.gov/public/maps/kml/PrivateLeases.kmz"
"https://apps.vdh.virginia.gov/kml/CondemnationZones_sim.kmz"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:bottom_longlines_restrictions"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:fish_traps_restrictions"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:octocoral_gear_restrictions"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:recreational_fishing_seasons_and_closures"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:roller_rig_trawls_restrictions"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:sargassum_restrictions"











"https://media.fisheries.noaa.gov/2020-04/scup-gear-restricted-areas-20161114-noaa-garfo.zip"


"https://media.fisheries.noaa.gov/2023-09/summer-flounder-fishery-sea-turtle-protection-area-20140501-1.zip"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:weakfish"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:atlantic_sharpnose_shark"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:atlantic_croaker"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:spot"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:bluefish"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:southern_kingfish"





"https://media.fisheries.noaa.gov/2020-04/lobster_gear_areas_20160501.zip"
"https://media.fisheries.noaa.gov/2020-04/illex_fishery_mesh_exemption_area_20140501.zip"
"https://media.fisheries.noaa.gov/2020-04/atlantic-red-drum-fishery-harvest-or-possession-prohibition-area-20140915-noaa-garfo.zip"
"https://media.fisheries.noaa.gov/2020-04/sne-dogfish-gillnet-exemption-area-20150315-noaa-garfo.zip"
"https://media.fisheries.noaa.gov/2020-04/sne-regulated-mesh-area-20150315-noaa-garfo.zip"
"https://media.fisheries.noaa.gov/2020-04/skate-management-unit-20140501-noaa-garfo.zip"
"https://media.fisheries.noaa.gov/2020-04/sne_exemption_area_20150315.zip"
"https://media.fisheries.noaa.gov/2020-04/management-units-sf-scup-bsb-20140501-noaa-garfo.zip"
"https://media.fisheries.noaa.gov/2020-04/scup-transfer-at-sea-20140501-noaa-garfo.zip"
"https://media.fisheries.noaa.gov/2020-04/sne_monkfish_and_skate_trawl_exemption_area_20150315.zip"
"https://media.fisheries.noaa.gov/2020-04/sne_monkfish_and_skate_gillnet_exemption_area_20150315.zip"
"https://media.fisheries.noaa.gov/2023-04/Scallop-Rotational-Areas-20230419.zip"
"https://media.fisheries.noaa.gov/2022-05/Virginia_Pound_Net_Regulated_Areas_2022523.zip"

"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:fish_traps_restrictions"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_biodiv_index"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_vermillion_snapper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_snowy_grouper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_scup"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_scamp_grouper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_blackbelly_rosefish"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_red_grouper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_scamp_grouper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_sand_perch"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_red_porgy"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_red_snapper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_red_grouper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_knobbed_porgy"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_white_grunt"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:long_bottom_longline_snowy_grouper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_gag_grouper"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_golden_tilefish"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_banksea_bass"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_blacksea_bass"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_grey_triggerfish"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_tomtate"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_spotted_moray_eel"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_spottail_pinfish"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:long_bottom_longline_golden_tilefish"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:white_shrimp"
"https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:brown_shrimp"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_blackfish_trap_survey_1990_2009"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_bottom_longline_1990_2009_"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_isaacs_kidd_midwater_trawl_1990_2009"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_kali_pole_1990_2009"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_short_bottom_longline_1990_2009"
"https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_yankee_trawl_1990_2009"

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

# file.rename(from = file.path(data_dir, list.files(data_dir,
#                                                   # get the Southeast_Blueprint directory
#                                                   pattern = "Southeast_Blueprint")),
#             to = file.path(data_dir, "Southeast_Blueprint"))

list.files(data_dir)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

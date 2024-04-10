#####################################################
### 0. Download Data -- fisheries and aquaculture ###
#####################################################

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
  
  # designate the URL that the data are hosted on
  url <- download_list
  
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
  
  if (grepl("shrimp_vms", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "shrimp_vms"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "shrimp_vms",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("shrimp_landings_south_carolina", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "sc_shrimp"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "shrimp_landings_south_carolina",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("accsp_fish_landings_by_latitude_and_species", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "fish_landings_lat_spp"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "accsp_fish_landings_by_latitude_and_species",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("fish_landings$", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "fish_landings"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "fish_landings",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("north_carolina_shrimp_landings", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "nc_shrimp"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "north_carolina_shrimp_landings",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("bottom_longlines_restrictions", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "bottom_longlines_restrictions"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "bottom_longlines_restrictions",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("fish_traps_restrictions", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "fish_traps_restrictions"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "fish_traps_restrictions",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("octocoral_gear_restrictions", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "octocoral_gear_restrictions"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "octocoral_gear_restrictions",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("roller_rig_trawls_restrictions", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "roller_rig_trawls_restrictions"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "roller_rig_trawls_restrictions",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("sargassum_restrictions", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "sargassum_restrictions"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "sargassum_restrictions",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("recreational_fishing_seasons_and_closures", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "recreational_fishing_seasons_and_closures"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "recreational_fishing_seasons_and_closures",
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
  
  if (grepl(".kmz", file)){
    
    ## clam grounds
    file.rename(from=file.path(data_dir, file),  # Make default download directory flexible
                # send to the raw data directory
                to=file.path(data_dir, paste0(sub(".kmz", "", file), ".zip")))
    
    unzip(zipfile = file.path(data_dir, paste0(sub(".kmz", "", file), ".zip")),
          # export file to the new data directory
          exdir = data_dir)
    
    file.rename(from=file.path(data_dir, "doc.kml"),  # Make default download directory flexible
                # send to the raw data directory
                to=file.path(data_dir, paste0(sub(".kmz", "", file), ".kml")))
    
    ## remove original zipped file
    file.remove(file.path(data_dir, paste0(sub(".kmz", "", file), ".zip")))
  }
}

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data/fisheries_aquaculture"

#####################################
#####################################

# download data
## fisheries and aquaculture
### Virginia public clamming grounds
clams <- "https://webapps.mrc.virginia.gov/public/maps/kml/Clams.kmz"

### Virginia public Baylor grounds
baylor <- "https://webapps.mrc.virginia.gov/public/maps/kml/Baylor.kmz"

### rock shrimp VMS
shrimp_vms <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:shrimp_vms"

### South Carolina shrimp landings
sc_shrimp <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:shrimp_landings_south_carolina"

### aquaculture
aquaculture <- "https://marinecadastre.gov/downloads/data/mc/Aquaculture.zip"

### commercial fishing landing summary
commercial_fish <- "https://marinecadastre.gov/downloads/data/mc/CommercialFishLandingSummary.zip"

### ACCSP fish landings (area code)
fish_landings_area <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:fish_landings"

### ACCSP fish landings (latitude and species)
fish_landings_species <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:accsp_fish_landings_by_latitude_and_species"

### North Carolina shrimp landings
nc_shrimp <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:north_carolina_shrimp_landings"

### VA Fixed Gear: Fyke Nets
fyke_nets <- "https://webapps.mrc.virginia.gov/public/maps/kml/FN.kmz"

### VA Fixed Gear: Pound Nets
pound_nets <- "https://webapps.mrc.virginia.gov/public/maps/kml/PN.kmz"

### VA Fixed Gear: Staked Gill Nets
staked_gill <- "https://webapps.mrc.virginia.gov/public/maps/kml/SGN.kmz"

### VMS Demarcation Line
vms_demarcation <- "https://www.fisheries.noaa.gov/s3/2020-04/vms_demarcation_line_20140925.zip"

open_harvest <- "https://webapps.mrc.virginia.gov/public/maps/kml/OpenHarvest.kmz"
oyster_garden <- "https://webapps.mrc.virginia.gov/public/maps/kml/OyGarden.kmz"
lease_applications <- "https://webapps.mrc.virginia.gov/public/maps/kml/LeaseApplications.kmz"
private_leases <- "https://webapps.mrc.virginia.gov/public/maps/kml/PrivateLeases.kmz"

lobster_gear <- "https://media.fisheries.noaa.gov/2020-04/lobster_gear_areas_20160501.zip"
fishery_mesh <- "https://media.fisheries.noaa.gov/2020-04/illex_fishery_mesh_exemption_area_20140501.zip"
atlantic_red_drum <- "https://media.fisheries.noaa.gov/2020-04/atlantic-red-drum-fishery-harvest-or-possession-prohibition-area-20140915-noaa-garfo.zip"
sne_dogfish <- "https://media.fisheries.noaa.gov/2020-04/sne-dogfish-gillnet-exemption-area-20150315-noaa-garfo.zip"
sne_regulated_mesh <- "https://media.fisheries.noaa.gov/2020-04/sne-regulated-mesh-area-20150315-noaa-garfo.zip"
sne_exemption_area <- "https://media.fisheries.noaa.gov/2020-04/sne_exemption_area_20150315.zip"
scup_transfer <- "https://media.fisheries.noaa.gov/2020-04/scup-transfer-at-sea-20140501-noaa-garfo.zip"
sne_monkfish_skate_trawl <- "https://media.fisheries.noaa.gov/2020-04/sne_monkfish_and_skate_trawl_exemption_area_20150315.zip"
sne_monkfish_skate_gillnet <- "https://media.fisheries.noaa.gov/2020-04/sne_monkfish_and_skate_gillnet_exemption_area_20150315.zip"
scallop_rotational <- "https://media.fisheries.noaa.gov/2023-04/Scallop-Rotational-Areas-20230419.zip"
va_pound_net <- "https://media.fisheries.noaa.gov/2022-05/Virginia_Pound_Net_Regulated_Areas_2022523.zip"
cape_lookout <- "https://media.fisheries.noaa.gov/2020-04/shapefile-cape-lookout-orig-ext.zip"
cape_fear <- "https://media.fisheries.noaa.gov/2020-04/capefear.zip"
stetson_miami <- "https://media.fisheries.noaa.gov/2020-04/stetson_miami.zip"
blake_ridge <- "https://media.fisheries.noaa.gov/2020-04/blakeridge.zip"
mpas <- "https://media.fisheries.noaa.gov/2020-04/mpas.zip"
sa_eez <- "https://www.fisheries.noaa.gov/s3/2020-04/sa_eez_off_states.zip"
comm_permits <- "https://media.fisheries.noaa.gov/2020-04/comm_permits_sa_sg.zip"
seabass_potid <- "https://media.fisheries.noaa.gov/2020-04/seabass_potid.zip"
smz <- "https://www.fisheries.noaa.gov/s3/2020-04/smz.zip"
prohibareas <- "https://media.fisheries.noaa.gov/2020-04/ll_prohibareas_n_s.zip"
spawning_smzs <- "https://media.fisheries.noaa.gov/2020-04/spawning_smzs.zip"
bsb_pot_nov_apr <- "https://media.fisheries.noaa.gov/2020-04/bsb_pot_nov_apr.zip"
bsb_pot_dec_mar <- "https://media.fisheries.noaa.gov/2020-04/bsb_pot_dec_mar.zip"
sa_shrimp <- "https://media.fisheries.noaa.gov/2020-04/sa_shrimp_cold_weather.zip"
octocoral <- "https://media.fisheries.noaa.gov/2020-04/octocoral.zip"
golden_crab <- "https://media.fisheries.noaa.gov/2020-04/goldencrab.zip"
pelagiclly_charleston <- "https://media.fisheries.noaa.gov/2020-04/pelagicll_charleston.zip"
pelagic_sargassum <- "https://media.fisheries.noaa.gov/2020-04/pelagic_sargassum.zip"
king_mackerel <- "https://media.fisheries.noaa.gov/2020-04/king_mackerel.zip"
spanish_mackerel <- "https://media.fisheries.noaa.gov/2020-04/spanish_mackerel_.zip"
cobia <- "https://media.fisheries.noaa.gov/2020-04/cobia.zip"

monkfish <- "https://media.fisheries.noaa.gov/2023-11/MonkfishFisheryManagementAreasShapefile.zip"
mgmt_units_skate <- "https://media.fisheries.noaa.gov/2020-04/skate-management-unit-20140501-noaa-garfo.zip"
mgmt_units_scuup <- "https://media.fisheries.noaa.gov/2020-04/management-units-sf-scup-bsb-20140501-noaa-garfo.zip"

condemation_zones <- "https://apps.vdh.virginia.gov/kml/CondemnationZones_sim.kmz"
btm_long_restrictions <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:bottom_longlines_restrictions"
fish_traps_restrictions <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:fish_traps_restrictions"
octocoral_gear <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:octocoral_gear_restrictions"
roller_rig_trawls <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:roller_rig_trawls_restrictions"
sargassum_restriction <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:sargassum_restrictions"

scup_gear <- "https://media.fisheries.noaa.gov/2020-04/scup-gear-restricted-areas-20161114-noaa-garfo.zip"

recreational_fishing_season_closure <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:recreational_fishing_seasons_and_closures"

#####################################
#####################################

# Download list
download_list <- c(clams,
                   baylor,
                   shrimp_vms,
                   sc_shrimp,
                   aquaculture,
                   commercial_fish,
                   fish_landings_area,
                   fish_landings_species,
                   nc_shrimp,
                   
                   fyke_nets,
                   pound_nets,
                   staked_gill,
                   vms_demarcation,
                   
                   open_harvest,
                   oyster_garden,
                   lease_applications,
                   private_leases,
                   
                   lobster_gear,
                   fishery_mesh,
                   atlantic_red_drum,
                   sne_dogfish,
                   sne_regulated_mesh,
                   sne_exemption_area,
                   scup_transfer,
                   sne_monkfish_skate_trawl,
                   sne_monkfish_skate_gillnet,
                   scallop_rotational,
                   va_pound_net,
                   cape_lookout,
                   cape_fear,
                   stetson_miami,
                   blake_ridge,
                   mpas,
                   sa_eez,
                   comm_permits,
                   seabass_potid,
                   smz,
                   prohibareas,
                   spawning_smzs,
                   bsb_pot_nov_apr,
                   bsb_pot_dec_mar,
                   sa_shrimp,
                   octocoral,
                   golden_crab,
                   pelagiclly_charleston,
                   pelagic_sargassum,
                   king_mackerel,
                   spanish_mackerel,
                   cobia,
                   monkfish,
                   mgmt_units_skate,
                   mgmt_units_scuup,
                   condemation_zones,
                   btm_long_restrictions,
                   fish_traps_restrictions,
                   octocoral_gear,
                   roller_rig_trawls,
                   sargassum_restriction,
                   scup_gear,
                   recreational_fishing_season_closure)
  
# data_download_function(download_list, data_dir)

#####################################
#####################################

cl <- parallel::makeCluster(spec = 12, # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = download_list, fun = data_download_function, data_dir = data_dir)

parallel::stopCluster(cl = cl)

# list all files in data directory
list.files(data_dir)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

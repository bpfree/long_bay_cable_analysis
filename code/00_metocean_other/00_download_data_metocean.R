##############################################
### 0. Download Data -- metocean and other ###
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

data_download_function <- function(download_list, data_dir){
  
  # loop function across all datasets
  for(i in 1:length(download_list)){
    
    # designate the URL that the data are hosted on
    url <- download_list[i]
    
    # file will become last part of the URL, so will be the data for download
    file <- basename(url)
    
    # Download the data
    if (!file.exists(file)) {
      options(timeout=1000000)
      # download the file from the URL
      download.file(url = url,
                    # place the downloaded file in the data directory
                    destfile = file.path(data_dir, file),
                    mode="wb")
    }
    
    if (grepl("southeast_bathymetry_meters", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "southeast_bathymetry_meters"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      file <- list.files(data_dir)[grep(pattern = "southeast_bathymetry_meters",
                                        x = list.files(data_dir))]
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
    
    if (grepl("undersea_features", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "undersea_features"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      file <- list.files(data_dir)[grep(pattern = "undersea_features",
                                        x = list.files(data_dir))]
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
    
    if (grepl("limit_of_ocsla_8g_zone", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "limit_of_ocsla_8g_zone"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      file <- list.files(data_dir)[grep(pattern = "limit_of_ocsla_8g_zone",
                                        x = list.files(data_dir))]
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
    
    if (grepl("submerged_land_acts_boundary_3_nm", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "submerged_land_acts_boundary_3_nm"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      file <- list.files(data_dir)[grep(pattern = "submerged_land_acts_boundary_3_nm",
                                        x = list.files(data_dir))]
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
    
    if (grepl("dod_wind", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "dod_wind"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      file <- list.files(data_dir)[grep(pattern = "dod_wind",
                                        x = list.files(data_dir))]
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
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
    
    if (grepl("ShellBase-NC", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "ShellBase-NC"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      file <- list.files(data_dir)[grep(pattern = "ShellBase-NC",
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
    
    if (grepl("698044", file)){
      
      new_dir_name <- "nps_boundaries"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
    
    if (grepl("NREL-HourlyWind", file)){
      
      new_dir_name <- "nrel_wind"
      
      # create new directory for data
      new_dir <- file.path(data_dir, new_dir_name)
      
      # unzip the file
      unzip(zipfile = file.path(data_dir, file),
            # export file to the new data directory
            exdir = new_dir)
      # remove original zipped file
      file.remove(file.path(data_dir, file))
    }
  }
}

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data/metocean_other"

#####################################
#####################################

# download data
## metocean
southeast_bathymetry <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:southeast_bathymetry_meters"
sc_esi_2015 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_2015_GDB.zip"
nc_esi_2015 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2016_GDB.zip"
current_spd_dir <- "https://marinecadastre.gov/downloads/data/mc/CurrentSpeedDirection.zip"
wave_hgt_dir <- "https://marinecadastre.gov/downloads/data/mc/SignificantWaveHeightDirection.zip"


wind_spd_dir <- "https://marinecadastre.gov/downloads/data/mc/WindSpeedDirection.zip"
natl_cyclone_wind <- "https://marinecadastre.gov/downloads/data/mc/TropicalCycloneWindExposure.zip"
sea_surface_hgt <- "https://marinecadastre.gov/downloads/data/mc/SeaSurfaceHeight.zip"
emu_water_quality <- "https://marinecadastre.gov/downloads/data/mc/EMUWaterQuality.zip"
emu_nutrient <- "https://marinecadastre.gov/downloads/data/mc/EMUNutrient.zip"
surface_aragonite <- "https://marinecadastre.gov/downloads/data/mc/SurfaceAragonite.zip"
light_kd_par <- "https://marinecadastre.gov/downloads/data/mc/LightAttenuationKdPAR.zip"
light_kd_490 <- "https://marinecadastre.gov/downloads/data/mc/LightAttenuationKd490.zip"
cholorophyll_a <- "https://marinecadastre.gov/downloads/data/mc/Chlorophylla.zip"
bathymetry_dem <- "https://marinecadastre.gov/downloads/data/mc/BathymetryDEM.zip"
bathymetric_contour <- "https://marinecadastre.gov/downloads/data/mc/BathymetricContour.zip"
nrel_wind_atl <- "https://www.boem.gov/NREL-HourlyWind-Atlantic-polysandpoints/"
enow_2015 <- "https://marinecadastre.gov/downloads/data/mc/ENOW2015.zip"

undersea <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:undersea_features"
benthic_habitats <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Marine_Life/Benthic_Habitats.zip"
canyons <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Marine_Life/Major_Canyons_Update.zip"
va_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/VA/VA_Middle_slr_data_dist.zip"

va_south_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/VA/VA_Southern_slr_data_dist.zip"

va_svi_2010 <- "https://coast.noaa.gov/htdata/SocioEconomic/SoVI2010/SoVI_2010_VA.zip"






acidification_va <- "https://portal.midatlanticocean.org/static/data_manager/data-download/AcidificationMonitoringMidA_Ver202310.zip"
ccv <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Socioeconomic/CCV_CoastalMidAtlantic_Counties.zip"

fishing_effects_vul <- "https://www.northeastoceandata.org/files/metadata/Themes/Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability.zip"







chesapeake_2016 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/ChesapeakeBay_2016_GDB.zip"

mab_median <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/MAB_median.zip"
mab_hipr <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/MAB_hIPR.zip"
mab_95th_pct <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/MAB_95th_perc.zip"
mab_mobile <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/MAB_mobile_perc.zip"
mab_mobile_freq <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/MAB_mobile_freq_v1_1.zip"
sab_median <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/SAB_median.zip"
sab_hipr <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/SAB_hIPR.zip"
sab_95th_pct <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/SAB_95th_perc.zip"
sab_mobile <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/SAB_mobile_perc.zip"
sab_mobile_freq <- "https://woodshole.er.usgs.gov/project-pages/mobility/ArcData/SAB_mobile_freq_v1_1.zip"
fishing_effects_sediment <- "https://www.northeastoceandata.org/files/metadata/Themes/Fishing_Effects_Sediment.zip"

nc_north_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/NC/NC_Northern_slr_data_dist.zip"

nc_middle1_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/NC/NC_Middle1_slr_data_dist.zip"

nc_middle2_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/NC/NC_Middle2_slr_data_dist.zip"

nc_south1_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/NC/NC_Southern1_slr_data_dist.zip"

nc_south2_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/NC/NC_Southern2_slr_data_dist.zip"

sc_north_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/SC/SC_North_slr_data_dist.zip"

sc_central_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/SC/SC_Central_slr_data_dist.zip"

sc_south_slr <- "https://chs.coast.noaa.gov/htdata/Inundation/SLR/SLRdata/SC/SC_South_slr_data_dist.zip"

sc_beachfront <- "https://www.scdhec.gov/sites/default/files/GIS/SHORELINE/bline-reports/data/BaselineSetbackline2018.zip"





oscla_zone <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:limit_of_ocsla_8g_zone"
submerged_land_acts <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:submerged_land_acts_boundary_3_nm"
sc_esi_1996 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_1996_GDB.zip"


nc_esi_2011 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2011_GDB.zip"
meow <- "https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/7gger8mmke_MEOW_FINAL.zip"
deepwater_mpa <- "https://ocean.floridamarine.org/efh_coral/zip/MPA_update.zip"
dod_wind <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:dod_wind"
coastal_pop_places <- "https://marinecadastre.gov/downloads/data/mc/CoastalPopulatedPlace.zip"
federal_state_waters <- "https://marinecadastre.gov/downloads/data/mc/FederalStateWaters.zip"
coastal_states <- "https://marinecadastre.gov/downloads/data/mc/CoastalState.zip"
coastal_counties <- "https://marinecadastre.gov/downloads/data/mc/CoastalCounty.zip"
federal_statutes <- "https://marinecadastre.gov/downloads/data/mc/FederalStatute.zip"
census_statistics <- "https://marinecadastre.gov/downloads/data/mc/CensusStatistics.zip"

coastal_zone_act <- "https://marinecadastre.gov/downloads/data/mc/CoastalZoneManagementAct.zip"
nmfs_regions <- "https://marinecadastre.gov/downloads/data/mc/NationalMarineFisheriesServiceRegions.zip"
atl_admin <- "https://www.boem.gov/sites/default/files/uploadedFiles/BOEM/Oil_and_Gas_Energy_Program/Mapping_and_Data/Administrative_Boundaries/Atl_admn.zip"
boem_ocs_areas <- "https://www.boem.gov/oil-gas-energy/leasing/boemocsareas-withdrawn-leasing-2021.zip"
nps_boundaries <- "https://irma.nps.gov/DataStore/DownloadFile/698044"
reservation_trusts <- "https://www2.census.gov/geo/tiger/TIGER2018/AIANNH/tl_2018_us_aiannh.zip"
census_tracts <- "https://portal.midatlanticocean.org/static/data_manager/data-download/census_tracts_coastal_counties.zip"
federal_location <- "https://marinecadastre.gov/downloads/data/mc/FederalConsistencyGeographicLocationDescription.zip"

cb_esi_2016 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/ChesapeakeBay_2016_GDB.zip"
us_maritime <- "https://maritimeboundaries.noaa.gov/downloads/USMaritimeLimitsAndBoundariesSHP.zip"
shrimp_vms <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:shrimp_vms"
shellbase_nc <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:SECOORA-ShellBase-NC"

#####################################
#####################################

# Download list
download_list <- c(
  southeast_bathymetry,
  sc_esi_2015,
  nc_esi_2015,
  current_spd_dir,
  wave_hgt_dir,
  wind_spd_dir,
  natl_cyclone_wind,
  sea_surface_hgt,
  emu_water_quality,
  emu_nutrient,
  surface_aragonite,
  light_kd_par,
  light_kd_490,
  cholorophyll_a,
  bathymetry_dem,
  bathymetric_contour,
  nrel_wind_atl,
  enow_2015,
  undersea,
  benthic_habitats,
  canyons,
  va_slr,
  va_south_slr,
  va_svi_2010,
  acidification_va,
  ccv,
  fishing_effects_vul,
  chesapeake_2016,
  mab_median,
  mab_hipr,
  mab_95th_pct,
  mab_mobile,
  mab_mobile_freq,
  sab_median,
  sab_hipr,
  sab_95th_pct,
  sab_mobile,
  sab_mobile_freq,
  fishing_effects_sediment,
  nc_north_slr,
  nc_middle1_slr,
  nc_middle2_slr,
  nc_south1_slr,
  nc_south2_slr,
  sc_north_slr,
  sc_central_slr,
  sc_south_slr,
  sc_beachfront,
  oscla_zone,
  submerged_land_acts,
  sc_esi_1996,
  nc_esi_2011,
  meow,
  deepwater_mpa,
  dod_wind,
  coastal_pop_places,
  federal_state_waters,
  coastal_states,
  coastal_counties,
  federal_statutes,
  census_statistics,
  coastal_zone_act,
  nmfs_regions,
  atl_admin,
  boem_ocs_areas,
  nps_boundaries,
  reservation_trusts,
  census_tracts,
  federal_location,
  cb_esi_2016,
  us_maritime,
  shrimp_vms,
  shellbase_nc
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

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
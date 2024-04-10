################################################
### 0. Download Data -- REST server download ###
################################################

# clear environment
rm(list = ls())

# calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(arcpullr,
               docxtractr,
               dplyr,
               elsa,
               fasterize,
               fs,
               ggplot2,
               httr,
               janitor,
               ncf,
               paletteer,
               pdftools,
               plyr,
               purrr,
               raster,
               RColorBrewer,
               reshape2,
               # rgdal,
               rgeoda,
               # rgeos,
               rmapshaper,
               rnaturalearth, # use devtools::install_github("ropenscilabs/rnaturalearth") if packages does not install properly
               sf,
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

data_dir <- "data/a_raw_data/fisheries_aquaculture"

#####################################
#####################################

rest_services_function <- function(url_list, base_url, data_dir){
  # define base URL (the service path)
  base_url <- base_url
  
  # define the unique dataset URL ending
  full_url <- url_list
  
  # combine the base with the dataset URL to create the entire data URL
  data_url <- file.path(base_url, full_url)
  
  # pull the spatial layer from the REST server
  data <- arcpullr::get_spatial_layer(data_url)
  
  # get the unique data name (when applicable)
  dir_name <- stringr::str_split(url_list, pattern = "/")[[1]][2]
  
  # create new directory for data
  dir_create <- dir.create(file.path(data_dir, dir_name))
  
  # set the new pathname to export the data
  new_dir <- file.path(data_dir, dir_name)
  
  # export the dataset
  sf::st_write(obj = data, dsn = file.path(new_dir, paste0(dir_name, ".shp")), delete_layer = F)
}

#####################################

# ocean uses data

url_list <- c(
  "OceanUses/ClamAmendmentAreas/MapServer/1",
  "OceanUses/CommercialFishingManagementAreas/MapServer/10",
  "OceanUses/CoralAmendmentAreas/MapServer/0",
  "OceanUses/FisheryManagementAreasSample/MapServer/43"
)

parallel::detectCores()[1]
cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = url_list, fun = rest_services_function,
                            base_url = "https://services.northeastoceandata.org/arcgis1/rest/services", data_dir = data_dir)

parallel::stopCluster(cl = cl)

#####################################

# SAFMC

url_list <- c(
  "SAFMC/SAFMC_Regulations/MapServer/18"
)

parallel::detectCores()[1]
cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = url_list, fun = rest_services_function,
                            base_url = "https://ocean.floridamarine.org/arcgis/rest/services/", data_dir = data_dir)

parallel::stopCluster(cl = cl)

#####################################

# North Carolina Department of Environmental and Natural Resources
## have to do one at a time for the directories do not have unique names

rest_services_function <- function(url_list, base_url, data_dir){
  base_url <- base_url
  full_url <- url_list
  data_url <- file.path(base_url, full_url)
  
  data <- arcpullr::get_spatial_layer(data_url)
  
  # ***warning: change depending on the dataset wanted
  dir_name <- "pound_net_prohibited"
  
  # create new directory for data
  dir_create <- dir.create(file.path(data_dir, dir_name))
  
  new_dir <- file.path(data_dir, dir_name)
  
  sf::st_write(obj = data, dsn = file.path(new_dir, paste0(dir_name, ".shp")), delete_layer = F)
}

url_list <- c(
  # "MFCRules_view/FeatureServer/7" # mechanical methods prohibited
  # "MFCRules_view/FeatureServer/16" # shrimp trawl prohibited area
  "MFCRules_view/FeatureServer/12" # pound net prohibited area
)

parallel::detectCores()[1]
cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = url_list, fun = rest_services_function,
                            base_url = "https://services2.arcgis.com/kCu40SDxsCGcuUWO/arcgis/rest/services/", data_dir = data_dir)

parallel::stopCluster(cl = cl)

#####################################

# list all files in data directory
list.files(data_dir)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

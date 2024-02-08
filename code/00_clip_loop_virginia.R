###########################################
### 0. Data layer clip loop -- Virginia ###
###########################################

# Clear environment
rm(list = ls())

# Calculate start time of code (determine how long it takes to complete all code)
start <- Sys.time()

#####################################
#####################################

# Load packages
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
               RSelenium,
               sf,
               shadowr,
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr,
               tidyverse)

#####################################
#####################################

# set directories
## designate geodatabase
study_region <- "data/a_raw_data/Carolinas_Database.gdb"
virginia_gdb <- "data/a_raw_data/VA_data_mining_20240104.gdb"

## export data geopackage
data_gpkg <- "data/b_intermediate_data/virginia.gpkg"

#####################################

# inspect
## study region
sf::st_layers(dsn = study_region,
              do_count = T)

sf::st_layers(dsn = virginia_gdb,
              do_count = T)

## geometry type to identify which ones are vectors
vector <- which(!is.na(sf::st_layers(dsn = virginia_gdb,
                                     do_count = T)$geomtype == "NA"))

### alternative to find rasters use which(is.na(sf::st_layers(dsn = marine_cadastre_gdb, do_count = T)$geomtype == "NA"))

## see length of data layers (70 data layers)
length(sf::st_layers(dsn = virginia_gdb,
                     do_count = T)[[1]])

## see length of vector data layers (70 data layers)
length(vector)

#####################################
#####################################

# parameters
# set the coordinate reference system that data should become (NAD83 / Conus Albers: https://epsg.io/5070)
crs <- "EPSG:5070"

# cat(crs(study_area))

#####################################
#####################################

study_area <- sf::st_read(dsn = study_region,
                          layer = sf::st_layers(dsn = study_region)[[1]][13]) %>%
  sf::st_transform(x = .,
                   crs = crs)

# loop through all layers
for(i in 1:length(vector)){ # use length(sf::st_layers(dsn = marine_cadastre_gdb, do_count = T)[[1]]) if all data are vector data
  start2 <- Sys.time()
  
  # i <- 1
  
  # ***Note: some data do not exist within the study region, to avoid having the code stop when it reaches
  #          such a data layer, the code uses try(), which will continue the code even if an error gets
  #          generated. See ?try() for more information about the function. silent = F will show error
  
  # ignore
  # skip analysis when i is equal to 103, 113, 137, 140, and 142
  ## 103 = CB_ESIP_2016
  ## 113 = CB_ESI_BIRDS_2016
  ## 137 = CB_ESI_POLITICAL_POLY_2016
  ## 140 = CB_ESI_NAT_HAZARD_POLYS_2016
  ## 142 = CB_ESI_HYDROP_2016
  if(i == 103 || i == 113 || i == 137 || i == 140 || i == 142){
    # print message saying why skipped
    print(paste("Skip interation", i, "for these data are located in the Chesapeake Bay and are outside the study region"), sep = "/")
    # go to next iteration
    next
  }
  
  if(!(i == 103 || i == 113 || i == 137 || i == 140 || i == 142)){
    data_name <- paste(sf::st_layers(dsn = virginia_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")
    
    # load data and clip
    try(data_set <- sf::st_read(dsn = virginia_gdb,
                                layer = sf::st_layers(virginia_gdb)[[1]][i]) %>%
          # reproject data to match study region coordinate reference system
          sf::st_transform(x = .,
                           # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
                           crs = crs) %>%
          # clip the data to the study region
          rmapshaper::ms_clip(target = .,
                              clip = study_area), silent = F)
    
    # give the data layer name to the generic data_set object
    try(assign(data_name, data_set), silent = F)
    
    # export data to geopackage
    try(sf::st_write(obj = data_set, dsn = data_gpkg, layer = paste(data_name), append = F), silent = F)
    
    # remove the current data set
    try(rm(data_set), silent = F)
    
    # print how long it takes to calculate
    print(paste("Iteration", i, "of", length(vector), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete creating and adding", data_name, "data to dataframe", sep = " "))
  }
}

#####################################
#####################################

sf::st_layers(dsn = data_gpkg,
              do_count = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate clip all data
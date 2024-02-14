##################################################
### 0. Data layer clip loop -- Marine Cadastre ###
##################################################

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
marine_cadastre_gdb <- "data/a_raw_data/Carolinas_Marine_Cadastre.gdb"

## export data geopackage
data_gpkg <- "data/b_intermediate_data/carl.gpkg"

#####################################

# inspect
## study region
sf::st_layers(dsn = study_region,
              do_count = T)

sf::st_layers(dsn = marine_cadastre_gdb,
              do_count = T)

## geometry type to identify which ones are vectors
vector <- which(!is.na(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)$geomtype == "NA"))

## inspect data layer number (will help in the for loop if not all vector datasets are sequential)
vector

### alternative to find rasters use which(is.na(sf::st_layers(dsn = marine_cadastre_gdb, do_count = T)$geomtype == "NA"))

## see length of data layers (70 data layers)
length(sf::st_layers(dsn = marine_cadastre_gdb,
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


  ## bathymetry contours
  ### run analysis when i is equal to 1 -- this data set requires extra handling steps
  if(i == 1){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")

    # load data and clip
    try(data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                                layer = sf::st_layers(marine_cadastre_gdb)[[1]][i]) %>%
          # reproject data to match study region coordinate reference system
          sf::st_transform(x = .,
                           # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
                           crs = crs) %>%
          # select a single field -- is not particular for which one it is (this just uses rule ID as a proxy)
          dplyr::select(RuleID) %>%
          # make all geometries "MULTILINESTRING"
          sf::st_cast("MULTILINESTRING") %>%
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

  #####################################

  ## most data
  ### run analysis when i is not equal to 1, 21, 24, 37, 55, 59, or 63
  if(!(i == 1 || i == 21 || i == 24 || i == 37 || i == 56 || i == 59 || i == 63 || i == 68 | i == 69 | i == 70)){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")

    # load data and clip
    try(data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                                layer = sf::st_layers(marine_cadastre_gdb)[[1]][i]) %>%
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

  #####################################

  ## coastal critical habitat designations
  ### run analysis when i equal to 21 -- help with processing time
  if(i == 21){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")

    # load data
    data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                            layer = sf::st_layers(marine_cadastre_gdb)[[1]][i])

    # create a smaller dataframe of the data
    data_project <- data_set %>%
      # reproject data to match study region coordinate reference system
      sf::st_transform(x = .,
                       # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
                       crs = crs) %>%
      # select a single field -- is not particular for which one it is (this just uses State as a proxy)
      dplyr::select(State)

    # create a clipped dataset
    data_clip <- data_project %>%
      # clip the data to the study region
      rmapshaper::ms_clip(clip = study_area)

    # give the data layer name to the generic data_set object
    assign(data_name, data_clip)

    # export data to geopackage
    sf::st_write(obj = data_clip, dsn = data_gpkg, layer = paste(data_name), append = F)

    # print how long it takes to calculate
    print(paste("Iteration", i, "of", length(vector), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete creating and adding", data_name, "data to dataframe", sep = " "))
  }

  #####################################

  ## coastal critical habitat designations
  ### run analysis when i equal to 24 -- help with processing time
  if(i == 24){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")

    # load data
    data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                            layer = sf::st_layers(marine_cadastre_gdb)[[1]][i]) %>%
      # make all geometries "MULTIPOLYGON"
      sf::st_cast("MULTIPOLYGON") %>%
      # drop dimensions to make geometry valid
      sf::st_zm(.)

    # create a smaller dataframe of the data
    data_project <- data_set %>%
      # reproject data to match study region coordinate reference system
      sf::st_transform(x = .,
                       # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
                       crs = crs) %>%
      # select a single field -- is not particular for which one it is (this just uses site ID as a proxy)
      dplyr::select(SITE_ID)

    # create a clipped dataset
    data_clip <- data_project %>%
      # clip the data to the study region
      rmapshaper::ms_clip(clip = study_area)

    # give the data layer name to the generic data_set object
    assign(data_name, data_clip)

    # export data to geopackage
    sf::st_write(obj = data_clip, dsn = data_gpkg, layer = paste(data_name), append = F)

    # print how long it takes to calculate
    print(paste("Iteration", i, "of", length(vector), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete creating and adding", data_name, "data to dataframe", sep = " "))
  }

  #####################################

  ## commercial fish landings
  ### run analysis when i equal to 35 -- help with processing time
  # if(i == 35){
  #   # grab data layer name and add "clip" at the end
  #   data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
  #                                    do_count = T)[[1]][i], "clip", sep = "_")
  #
  #   # load data
  #   data_set <- sf::st_read(dsn = marine_cadastre_gdb,
  #                           layer = sf::st_layers(marine_cadastre_gdb)[[1]][i]) %>%
  #     # make all geometries "MULTIPOLYGON"
  #     sf::st_cast("MULTIPOLYGON") %>%
  #     # drop dimensions to make geometry valid
  #     sf::st_zm(.)
  #
  #   # create a smaller dataframe of the data
  #   data_project <- data_set %>%
  #     # reproject data to match study region coordinate reference system
  #     sf::st_transform(x = .,
  #                      # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
  #                      crs = crs) %>%
  #     # select a single field -- is not particular for which one it is (this just uses geographic ID as a proxy)
  #     dplyr::select(StateUSPS) %>%
  #     dplyr::filter(StateUSPS %in% c("NC", "SC"))
  #
  #   # create a clipped dataset
  #   data_clip <- data_project %>%
  #     # clip the data to the study region
  #     rmapshaper::ms_clip(clip = study_area)
  #
  #   # give the data layer name to the generic data_set object
  #   assign(data_name, data_clip)
  #
  #   # export data to geopackage
  #   sf::st_write(obj = data_clip, dsn = data_gpkg, layer = paste(data_name), append = F)
  #
  #   # print how long it takes to calculate
  #   print(paste("Iteration", i, "of", length(vector), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete creating and adding", data_name, "data to dataframe", sep = " "))
  # }

  #####################################

  ## anchorage areas
  ### run analysis when i equal to 37 -- help with processing time
  if(i == 37){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")

    # load data
    data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                            layer = sf::st_layers(marine_cadastre_gdb)[[1]][i]) %>%
      # make all geometries "MULTIPOLYGON"
      sf::st_cast("MULTIPOLYGON")

    # create a smaller dataframe of the data
    data_project <- data_set %>%
      # reproject data to match study region coordinate reference system
      sf::st_transform(x = .,
                       # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
                       crs = crs)

    # create a clipped dataset
    data_clip <- data_project %>%
      # clip the data to the study region
      rmapshaper::ms_clip(clip = study_area)

    # give the data layer name to the generic data_set object
    assign(data_name, data_clip)

    # export data to geopackage
    sf::st_write(obj = data_clip, dsn = data_gpkg, layer = paste(data_name), append = F)

    # print how long it takes to calculate
    print(paste("Iteration", i, "of", length(vector), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete creating and adding", data_name, "data to dataframe", sep = " "))
  }

  #####################################

  ## aids to navigation
  ### run analysis when i equal to 56 -- help with processing time
  if(i == 56){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")

    # load data
    data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                            layer = sf::st_layers(marine_cadastre_gdb)[[1]][i]) %>%
      # make all geometries "POINT"
      sf::st_cast("POINT")

    # create a smaller dataframe of the data
    data_project <- data_set %>%
      # reproject data to match study region coordinate reference system
      sf::st_transform(x = .,
                       # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
                       crs = crs)

    # create a clipped dataset
    data_clip <- data_project %>%
      # clip the data to the study region
      rmapshaper::ms_clip(clip = study_area)

    # give the data layer name to the generic data_set object
    assign(data_name, data_clip)

    # export data to geopackage
    sf::st_write(obj = data_clip, dsn = data_gpkg, layer = paste(data_name), append = F)

    # print how long it takes to calculate
    print(paste("Iteration", i, "of", length(vector), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete creating and adding", data_name, "data to dataframe", sep = " "))
  }

  #####################################

  ## current
  ### run analysis when i equal to 59 -- help with processing time
  if(i == 59){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")

    # load data
    data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                            layer = sf::st_layers(marine_cadastre_gdb)[[1]][5i])

    # create a smaller dataframe of the data
    data_project <- data_set %>%
      # reproject data to match study region coordinate reference system
      sf::st_transform(x = .,
                       # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
                       crs = crs) %>%
      # select fields -- is not particular for which one they are (this just uses longitude and latitude as a proxy)
      dplyr::select(lon, lat)

    # create a clipped dataset
    data_clip <- data_project %>%
      # clip the data to the study region
      rmapshaper::ms_clip(clip = study_area)

    # give the data layer name to the generic data_set object
    assign(data_name, data_clip)

    # export data to geopackage
    sf::st_write(obj = data_clip, dsn = data_gpkg, layer = paste(data_name), append = F)

    # print how long it takes to calculate
    print(paste("Iteration", i, "of", length(vector), "takes", Sys.time() - start2, units(Sys.time() - start2), "to complete creating and adding", data_name, "data to dataframe", sep = " "))
  }

  # ignore tropical cyclone wind exposure North America 1900 - 2016
  # skip analysis when i is equal to 63
  if(i == 63){
    # print message saying why skipped
    print(paste("Skip interation", i, "for it cannot get completed due to taking too much memory "), sep = "/")
    # go to next iteration
    next
  }
  
  # for EMU nitrate, phosphate, and silicate data
  ## need to make point so readable in ArcGIS
  if(i == 68 | i == 69 | i == 70){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")
    
    # load data
    data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                            layer = sf::st_layers(marine_cadastre_gdb)[[1]][i]) %>%
      # drop the M geometry (these EMU data have ZM geometry)
      sf::st_zm() %>%
      # make all geometries "POINT"
      sf::st_cast("POINT")
    
    # create a smaller dataframe of the data
    data_project <- data_set %>%
      # reproject data to match study region coordinate reference system
      sf::st_transform(x = .,
                       # coordinate reference system (EPSG:5070, NAD83 / Conus Albers)
                       crs = crs)
    
    # create a clipped dataset
    data_clip <- data_project %>%
      # clip the data to the study region
      rmapshaper::ms_clip(clip = study_area)
    
    # give the data layer name to the generic data_set object
    assign(data_name, data_clip)
    
    # export data to geopackage
    sf::st_write(obj = data_clip, dsn = data_gpkg, layer = paste(data_name), append = F)
    
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
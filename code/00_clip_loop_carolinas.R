###########################################
### 0. Data layer clip loop -- Carolinas ###
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
carolinas_gdb <- "data/a_raw_data/Carolinas.gdb"

## export data geopackage
data_gpkg <- "data/b_intermediate_data/carolinas.gpkg"

#####################################

# inspect
## study region
sf::st_layers(dsn = study_region,
              do_count = T)

sf::st_layers(dsn = carolinas_gdb,
              do_count = T)

## geometry type to identify which ones are vectors
vector <- which(!is.na(sf::st_layers(dsn = carolinas_gdb,
                                     do_count = T)$geomtype == "NA"))

## inspect data layer number (will help in the for loop if not all vector datasets are sequential)
vector

### alternative to find rasters use which(is.na(sf::st_layers(dsn = marine_cadastre_gdb, do_count = T)$geomtype == "NA"))

## see length of data layers (575 data layers)
length(sf::st_layers(dsn = carolinas_gdb,
                     do_count = T)[[1]])

## see length of vector data layers (521 data layers)
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
  
  #Note: some data do not exist within the study region, to avoid having the code stop when it reaches
  #          such a data layer, the code uses try(), which will continue the code even if an error gets
  #          generated. See ?try() for more information about the function. silent = F will show error
  
  # when getting this error: "Error in if (is_url(geojson)) { : argument is of length zero"
  ## inspect 218, 244, 246, 248, 250, 256, 259, 260, 263, 266, 279, 282, 290, 386, 523
  ## 218: NC_ESI_POLITICAL_POLY_2016
  ## 244: NC_ESI_BIRDS_2016
  ## 246: NC_ESI_ESIL_2016
  ## 248: NC_ESI_ESIP_2016
  ## 250: NC_ESI_FISH_2016
  ## 256: NC_ESI_HERP_2016
  ## 259: NC_ESI_HYDROL_2016
  ## 260: NC_ESI_HYDROP_2016
  ## 263: NC_ESI_INVERT_2016
  ## 266: NC_ESI_M_MAMMAL_2016
  ## 279: SC_ESI_esil_1996
  ## 280: SC_ESI_ESIL_2015
  ## 282: SC_ESI_ESIP_2015
  ## 290: SC_ESI_HYDROP_2015
  ## 386: Seagrass
  ## 523: cpt_bwha_230710
  
  # run clip for datasets when datasets have issue with geometry MULTIPOLYGON
  if(i == 218 || i == 244 || i == 248 || i == 250 || i == 256 || i == 260 || i == 263 || i == 266 || i == 282 || i == 290){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = carolinas_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")
    
    # load data
    data_set <- sf::st_read(dsn = carolinas_gdb,
                            layer = sf::st_layers(carolinas_gdb)[[1]][i]) %>%
      # make all geometries "MULTIPOLYGON" -- this will fix the error dealing with "argument is of length zero)
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
  
  # run clip for datasets when datasets have issue with geometry MULTILINESTRING
  if(i == 246 || i == 259 || i == 279 || i == 280){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = carolinas_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")
    
    # load data
    data_set <- sf::st_read(dsn = carolinas_gdb,
                            layer = sf::st_layers(carolinas_gdb)[[1]][i]) %>%
      # make all geometries "MULTILINESTRING" -- this will fix the error dealing with "argument is of length zero)
      sf::st_cast("MULTILINESTRING")
    
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
  
  # run clip for seagrass dataset (limit to only North Carolina and Virginia -- only states with seagrass)
  if(i == 386){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = carolinas_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")
    
    # load data
    data_set <- sf::st_read(dsn = carolinas_gdb,
                            layer = sf::st_layers(carolinas_gdb)[[1]][i]) %>%
      dplyr::filter(stateUSPS %in% c("NC", "VA")) %>%
      # make all geometries "MULTIPOLYGON" -- this will fix the error dealing with "argument is of length zero)
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
  
  # skip data layers known to be outside of the study region:
  ## CommercialWhaleWatchingAreas (391)
  ## Surface_Water_Activities (394)
  ## NC north sea level rise 0 - 10 feet (397 - 407)
  ## NC middle 1 sea level rise 0 - 10 feet (408 - 418)
  ## NC middle 2 sea level rise 0 - 10 feet (419 - 429)
  ## SC south sea level rise 0 - 10 feet (474 - 484)
  if(i == 391 || i == 394 || 
     i == 397 || i == 398 || i == 399 || i == 400 || i == 401 || i == 402 || i == 403 || i == 404 || i == 405 || i == 406 || i == 407 ||
     i == 408 || i == 409 || i == 410 || i == 411 || i == 412 || i == 413 || i == 414 || i == 415 || i == 416 || i == 417 || i == 418 ||
     i == 419 || i == 420 || i == 421 || i == 422 || i == 423 || i == 424 || i == 425 || i == 426 || i == 427 || i == 428 || i == 429 ||
     i == 474 || i == 475 || i == 476 || i == 477 || i == 478 || i == 479 || i == 480 || i == 481 || i == 482 || i == 483 || i == 484){
    data_name <- paste(sf::st_layers(dsn = carolinas_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")
    
    # print message saying why skipped
    print(paste("Skip interation", i, "for", data_name, "sea level rise scenario data are known to exist outside the study region"), sep = "/")
    # go to next iteration
    next
  }
  
  #####################################
  
  # ignore data that take too much memory
  # skip analysis when i is equal to:
  ## 29 = South Carolina shellfish management areas
  ## 90 = North Carolina DCM oceanfront shorelines
  ## 113 = NC_2022_Integrated_Report_ParameterAssessments
  ## 114 = NC_2022_Integrated_Report_Polygons__Parameter_Assessment_
  ## 238 = NC_ESI_NAT_HAZARD_POLY_2016
  ## 381 = AISVesselTracks2022
  ## 441 - 451 = North Carolina south 2 sea level rise (0 - 10 ft)
  ## 452 - 453 = South Carolina north sea level rise (0 - 1 ft)
  ## 462 - 473 = South Carolina central sea level rise (0 - 10 ft)
  if(i == 29 || i == 90 || i == 113 || i == 114 || i == 238 || i == 239 || i == 240 || i == 381 ||
     i == 441 || i == 442 || i == 443 || i == 444 || i == 445 || i == 446 || i == 447 || i == 448 || i == 449 || i == 450 || i == 451 ||
     i == 452 || i == 453 || 
     i == 463 || i == 464 || i == 465 || i == 466 || i == 467 || i == 468 || i == 469 || i == 470 || i == 471 || i == 472 || i == 473){
    data_name <- paste(sf::st_layers(dsn = carolinas_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")
    
    # print message saying why skipped
    print(paste("Skip interation", i, "for taking too much memory to complete", data_name, "dataset"), sep = "/")
    # go to next iteration
    next
  }
  
  #####################################
  
  # cpt_bwha_230710 data
  if(i == 521){
    j <- vector[i]
    
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = carolinas_gdb,
                                     do_count = T)[[1]][j], "clip", sep = "_")
    
    # load data and clip
    try(data_set <- sf::st_read(dsn = carolinas_gdb,
                                layer = sf::st_layers(carolinas_gdb)[[1]][j]) %>%
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
  
  # for all other data
  if(!(i == 218 || i == 244 || i == 248 || i == 250 || i == 256 || i == 260 || i == 263 || i == 266 || i == 282 || i == 290 ||
       i == 246 || i == 259 || i == 279 || i == 280 ||
       i == 386 ||
       i == 391 || i == 394 || 
       i == 397 || i == 398 || i == 399 || i == 400 || i == 401 || i == 402 || i == 403 || i == 404 || i == 405 || i == 406 || i == 407 ||
       i == 408 || i == 409 || i == 410 || i == 411 || i == 412 || i == 413 || i == 414 || i == 415 || i == 416 || i == 417 || i == 418 ||
       i == 419 || i == 420 || i == 421 || i == 422 || i == 423 || i == 424 || i == 425 || i == 426 || i == 427 || i == 428 || i == 429 ||
       i == 474 || i == 475 || i == 476 || i == 477 || i == 478 || i == 479 || i == 480 || i == 481 || i == 482 || i == 483 || i == 484 ||
       i == 29 || i == 90 || i == 113 || i == 114 || i == 238 || i == 239 || i == 240 || i == 381 ||
       i == 441 || i == 442 || i == 443 || i == 444 || i == 445 || i == 446 || i == 447 || i == 448 || i == 449 || i == 450 || i == 451 ||
       i == 452 || i == 453 || 
       i == 463 || i == 464 || i == 465 || i == 466 || i == 467 || i == 468 || i == 469 || i == 470 || i == 471 || i == 472 || i == 473 ||
       i == 521)){
    # grab data layer name and add "clip" at the end
    data_name <- paste(sf::st_layers(dsn = carolinas_gdb,
                                     do_count = T)[[1]][i], "clip", sep = "_")
    
    # load data and clip
    try(data_set <- sf::st_read(dsn = carolinas_gdb,
                                layer = sf::st_layers(carolinas_gdb)[[1]][i]) %>%
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

# inspect all data that got clipped
sf::st_layers(dsn = data_gpkg,
              do_count = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate clip all data
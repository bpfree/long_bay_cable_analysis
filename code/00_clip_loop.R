#########################################################
### 0. Spatial extent calculations -- Marine Cadastre ###
#########################################################

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

## top 10 layer names
sf::st_layers(dsn = marine_cadastre_gdb,
              do_count = T)[[1]][1:10]

## all layers
### ***Note: if not all rows are printed (and you see something like: 
###          "reached 'max' / getOption("max.print") -- omitted 495 rows"
###          then type into console options(max.print = 500). This should
###          print out all results 70 (rows) * 5 (fields) = 350
sf::st_layers(dsn = marine_cadastre_gdb,
              do_count = T)

## geometry type to identify which ones are vectors
vector <- which(!is.na(sf::st_layers(dsn = marine_cadastre_gdb,
                                     do_count = T)$geomtype == "NA"))

### alternative to find rasters use which(is.na(sf::st_layers(dsn = marine_cadastre_gdb, do_count = T)$geomtype == "NA"))

## see length of data layers (70 data layers)
length(sf::st_layers(dsn = marine_cadastre_gdb,
                     do_count = T)[[1]])

## see length of vector data layers (70 data layers)
length(vector)

#####################################
#####################################

# parameters
# set the coordinate reference system that data should become (WGS84: https://epsg.io/4326)
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
  
  # i <- 3
  
  # ***Note: some data do not exist within the study region, to avoid having the code stop when it reaches
  #          such a data layer, the code uses try(), which will continue the code even if an error gets
  #          generated. See ?try() for more information about the function. silent = F will show error
  
  # ignore 21 / 59 / 63
  # skip analysis when i is equal to 21, 59, or 63
  if((i == 21 || i == 59 || i == 63)){
    # print message saying why skipped
    print(paste("Skip interation", i, "for it cannot get completed due to taking too much memory "), sep = "/")
    # go to next iteration
    next
  }

  # run analysis when i is not equal to 21 or 59
  # grab data layer name and add "clip" at the end
  data_name <- paste(sf::st_layers(dsn = marine_cadastre_gdb,
                                   do_count = T)[[1]][i], "clip", sep = "_")
  
  # load data and clip
  try(data_set <- sf::st_read(dsn = marine_cadastre_gdb,
                              layer = sf::st_layers(marine_cadastre_gdb)[[1]][i]) %>%
        # reproject data to match study region coordinate reference system
        sf::st_transform(x = .,
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
#####################################

sf::st_layers(dsn = data_gpkg,
              do_count = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
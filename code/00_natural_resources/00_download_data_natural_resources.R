#############################################
### 0. Download Data -- natural resources ###
#############################################

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
    options(timeout=100000)
    # download the file from the URL
    download.file(url = url,
                  # place the downloaded file in the data directory
                  destfile = file.path(data_dir, file),
                  mode="wb")
  }
  
  if (grepl("coralhapc", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "coralhapc"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "coralhapc",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
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
  
  if (grepl("dolphin_wahoo_efh_hapc", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "dolphin_wahoo_efh_hapc"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "dolphin_wahoo_efh_hapc",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("snapper_grouper_efh_hapc", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "snapper_grouper_efh_hapc"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "snapper_grouper_efh_hapc",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("tilefish_efh_hapc", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "tilefish_efh_hapc"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "tilefish_efh_hapc",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("shrimpefh", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "shrimpefh"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "shrimpefh",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("dolphin_wahoo_efh$", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "dolphin_wahoo_efh"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "dolphin_wahoo_efh$",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("spinylobsterefh", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "spinylobsterefh"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "spinylobsterefh",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("goldencrabefh", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "goldencrabefh"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "goldencrabefh",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("deepwater_coral_hapcs", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "deepwater_coral_hapcs"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "deepwater_coral_hapcs",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("shorebirds", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "shorebirds"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "shorebirds",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("seamap_turtle", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "seamap_turtle"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "seamap_turtle",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("in_water_kemps", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "in_water_kemps"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "in_water_kemps",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("coral_mounds", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "coral_mounds"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "coral_mounds",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("artificial_reefs", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "artificial_reefs"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "artificial_reefs",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("north_american_right_whale", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "north_american_right_whale"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "north_american_right_whale",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("pipingplover_se", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "pipingplover_se"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "pipingplover_se",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("weakfish", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "weakfish"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "weakfish",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("atlantic_sharpnose_shark", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "atlantic_sharpnose_shark"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "atlantic_sharpnose_shark",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("atlantic_croaker", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "atlantic_croaker"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "atlantic_croaker",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("spot", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "spot"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "spot",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("bluefish", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "bluefish"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "bluefish",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("southern_kingfish", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "southern_kingfish"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "southern_kingfish",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("marmap", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "marmap"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "marmap",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_vermillion_snapper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_vermillion_snapper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_vermillion_snapper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("short_bottom_longline_snowy_grouper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "short_bottom_longline_snowy_grouper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "short_bottom_longline_snowy_grouper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_scup", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_scup"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_scup",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_scup", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_scup"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_scup",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_scamp_grouper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_scamp_grouper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_scamp_grouper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("short_bottom_longline_blackbelly_rosefish", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "short_bottom_longline_blackbelly_rosefish"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "short_bottom_longline_blackbelly_rosefish",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_red_grouper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_red_grouper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_red_grouper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("short_bottom_longline_scamp_grouper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "short_bottom_longline_scamp_grouper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "short_bottom_longline_scamp_grouper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_sand_perch", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_sand_perch"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_sand_perch",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_red_porgy", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_red_porgy"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_red_porgy",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_red_snapper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_red_snapper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_red_snapper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("short_bottom_longline_red_grouper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "short_bottom_longline_red_grouper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "short_bottom_longline_red_grouper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_knobbed_porgy", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_knobbed_porgy"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_knobbed_porgy",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_white_grunt", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_white_grunt"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_white_grunt",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("long_bottom_longline_snowy_grouper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "long_bottom_longline_snowy_grouper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "long_bottom_longline_snowy_grouper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }

  if (grepl("trap_gag_grouper", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_gag_grouper"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_gag_grouper",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }

  if (grepl("short_bottom_longline_golden_tilefish", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "short_bottom_longline_golden_tilefish"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "short_bottom_longline_golden_tilefish",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_banksea_bass", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_banksea_bass"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_banksea_bass",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_blacksea_bass", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_blacksea_bass"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_blacksea_bass",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_grey_triggerfish", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_grey_triggerfish"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_grey_triggerfish",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_tomtate", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_tomtate"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_tomtate",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_spotted_moray_eel", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_spotted_moray_eel"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_spotted_moray_eel",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("trap_spottail_pinfish", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "trap_spottail_pinfish"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "trap_spottail_pinfish",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("long_bottom_longline_golden_tilefish", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "long_bottom_longline_golden_tilefish"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "long_bottom_longline_golden_tilefish",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("white_shrimp", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "white_shrimp"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "white_shrimp",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("brown_shrimp", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "brown_shrimp"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "brown_shrimp",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("brown_shrimp", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "brown_shrimp"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "brown_shrimp",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("marmap_blackfish_trap_survey_1990_2009", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "marmap_blackfish_trap_survey_1990_2009"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "marmap_blackfish_trap_survey_1990_2009",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }

  if (grepl("marmap_bottom_longline_1990_2009_", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "marmap_bottom_longline_1990_2009_"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "marmap_bottom_longline_1990_2009_",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("marmap_isaacs_kidd_midwater_trawl_1990_2009", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "marmap_isaacs_kidd_midwater_trawl_1990_2009"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "marmap_isaacs_kidd_midwater_trawl_1990_2009",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("marmap_kali_pole_1990_2009", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "marmap_kali_pole_1990_2009"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "marmap_kali_pole_1990_2009",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("marmap_short_bottom_longline_1990_2009", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "marmap_short_bottom_longline_1990_2009"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "marmap_short_bottom_longline_1990_2009",
                                      x = list.files(data_dir))]
    
    # unzip the file
    unzip(zipfile = file.path(data_dir, file),
          # export file to the new data directory
          exdir = new_dir)
    # remove original zipped file
    file.remove(file.path(data_dir, file))
  }
  
  if (grepl("marmap_yankee_trawl_1990_2009", file)){
    
    # grab text before ".zip" and keep only text before that
    new_dir_name <- "marmap_yankee_trawl_1990_2009"
    
    # create new directory for data
    new_dir <- file.path(data_dir, new_dir_name)
    
    file <- list.files(data_dir)[grep(pattern = "marmap_yankee_trawl_1990_2009",
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
data_dir <- "data/a_raw_data/natural_resources"

#####################################
#####################################

# download data
## natural resources
nhpna <- "https://ncnhde.natureserve.org/system/files/nhna_NaturalArea_2024-01.zip"
marea <- "https://ncnhde.natureserve.org/system/files/marea_ManagedArea_2024-01.zip"

summer_flounder <- "https://media.fisheries.noaa.gov/2023-09/summer-flounder-fishery-sea-turtle-protection-area-20140501-1.zip"
bottlenose_dolphin_n_ncar <- "https://media.fisheries.noaa.gov/2020-04/bdtrp_n_nc_po.zip"
bottlenose_dolphin_s_ncar <- "https://media.fisheries.noaa.gov/2020-04/bdtrp_s_nc_po.zip"
bottlenose_dolphin_sc_ga_fl <- "https://media.fisheries.noaa.gov/2020-04/bdtrp_sc_fl_po_0.zip"

sc_esi_1996 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_1996_GDB.zip"
sc_esi_2015 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_2015_GDB.zip"
nc_esi_2016 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2016_GDB.zip"
nc_esi_2011 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2011_GDB.zip"

coral_hapc <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:coralhapc"
coast_mig_pelagic <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:coastmigpelhapc"
dolphin_wahoo_efh_hapc <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:dolphin_wahoo_efh_hapc"
snapper_grouper <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:snapper_grouper_efh_hapc"
tilefish <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:tilefish_efh_hapc"

shrimp <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:shrimpefh"
dolphin_wahoo_efh <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:dolphin_wahoo_efh"
spiny_lobster <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:spinylobsterefh"
golden_crab <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:goldencrabefh"
deepwater_coral <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:deepwater_coral_hapcs"
critical_habitat <- "https://ecos.fws.gov/docs/crithab/crithab_all/crithab_all_layers.zip"

shorebird_nests <- "https://data.axds.co/gs/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa_shorebirds"
seamap_loggerhead <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:seamap_turtle"
kemp_ridley_seaturtle <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:in_water_kemps"
coastal_critical_habitat <- "https://marinecadastre.gov/downloads/data/mc/CoastalCriticalHabitat.zip"
highly_migratory_species <- "https://marinecadastre.gov/downloads/data/mc/EFHHighlyMigratorySpecies.zip"
cetacean_bia <- "http://cetsound.noaa.gov/Assets/cetsound/data/CetMap_BIA_WGS84.zip"
audubon_bird_areas <- "https://marinecadastre.gov/downloads/data/mc/AudubonImportantBirdArea.zip"
protected_areas <- "https://marinecadastre.gov/downloads/data/mc/ProtectedArea.zip"
deepsea_coral_observations <- "https://marinecadastre.gov/downloads/data/mc/DeepSeaCoralObservation.zip"

deepsea_coral_habitat_suitability <- "https://marinecadastre.gov/downloads/data/mc/DeepSeaCoralHabitatSuitability.zip"
coastal_barrier <- "https://marinecadastre.gov/downloads/data/mc/CoastalBarrierResourceAreas.zip"

va_jones_shore <- "https://webapps.mrc.virginia.gov/public/maps/kml/prfcWeb.kml"
va_state_marsh <- "https://webapps.mrc.virginia.gov/public/maps/kml/State.kmz"
va_submerged_aquatic <- "https://webapps.mrc.virginia.gov/public/maps/kml/VIMS_SAV_2023_p1.kmz"
va_es_submerged <- "https://webapps.mrc.virginia.gov/public/maps/kml/sav.kml"
va_wma <- "https://dwr.virginia.gov/-/gis-data/VDGIF_Wildlife_Management_Area_WMA_Boundaries.zip"
va_priority_conservation <- "https://dwr.virginia.gov/-/gis-data/pca_shapefile.zip"
veva <- "https://dwr.virginia.gov/-/gis-data/veva_shapefile.zip"
mid_atl_artificial_reefs <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Fishing/ArtificialReefs2023FebUpdate.zip"

mallow_bay <- "https://sanctuaries.noaa.gov/media/gis/mbpr_py.zip"
national_estuarine_researche <- "https://marinecadastre.gov/downloads/data/mc/NationalEstuarineResearchReserveSystem.zip"
coastal_wetland <- "https://marinecadastre.gov/downloads/data/mc/CoastalWetland.zip"

lautenberg_coral <- "https://media.fisheries.noaa.gov/2020-04/frank_r_lautenberg_deep_sea_coral_protection_areas_20180409.zip"
harbor_porpoise <- "https://media.fisheries.noaa.gov/2020-04/hptrp_mid-atlantic_regulated_and_exempted_waters_20140915.zip"
cb_esi_2016 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/ChesapeakeBay_2016_GDB.zip"

seagrass <- "https://marinecadastre.gov/downloads/data/mc/Seagrass.zip"
coral_mounds <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:coral_mounds"
se_atl_artificial_reef <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:artificial_reefs"
na_right_whale_sighting <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:north_american_right_whale"
piping_plover <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:pipingplover_se"
na_right_whale_season <- "https://marinecadastre.gov/downloads/data/mc/NorthAtlanticRightWhaleSMA.zip"

crabs <- "https://webapps.mrc.virginia.gov/public/maps/kml/crabs.kml"
oyster_sanctuaries <- "https://webapps.mrc.virginia.gov/public/maps/kml/oyster_sanctuaries.kml"

weakfish <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:weakfish"
sharpnose_shark <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:atlantic_sharpnose_shark"
croaker <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:atlantic_croaker"
spot <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:spot"
bluefish <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:bluefish"
southern_kingfish <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:southern_kingfish"

marmap <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_biodiv_index"
vermillion_snapper_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_vermillion_snapper"
snow_grouper_short_bottom_ll  <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_snowy_grouper"
scup_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_scup"
scamp_grouper_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_scamp_grouper"
blackbelly_rosefish_short_bottom_ll <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_blackbelly_rosefish"
red_grouper_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_red_grouper"
scamp_grouper_short_bottom_ll <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_scamp_grouper"
sand_perch_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_sand_perch"
red_porgy_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_red_porgy"
red_snapper_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_red_snapper"
red_grouper_short_bottom_ll <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_red_grouper"
knobbed_porgy_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_knobbed_porgy"
white_grunt_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_white_grunt"
snow_grouper_long_bottom_ll <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:long_bottom_longline_snowy_grouper"
gag_grouper_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_gag_grouper"
golden_tilefish_short_bottom_ll <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:short_bottom_longline_golden_tilefish"
bank_sea_bass_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_banksea_bass"
black_sea_bass_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_blacksea_bass"
grey_triggerfish_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_grey_triggerfish"
tomtate_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_tomtate"
moray_eel_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_spotted_moray_eel"
spottail_pinfish_trap <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:trap_spottail_pinfish"
golden_tilefish_long_bottom_ll <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:long_bottom_longline_golden_tilefish"
white_shrimp <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:white_shrimp"
brown_shrimp <- "https://data.axds.co/gs/gsaa/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=gsaa:brown_shrimp"
blackfish_trap <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_blackfish_trap_survey_1990_2009"
bottom_longline <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_bottom_longline_1990_2009_"
isaacs_kidd <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_isaacs_kidd_midwater_trawl_1990_2009"
kali_pole <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_kali_pole_1990_2009"
short_bottom_longline <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_short_bottom_longline_1990_2009"
yankee_trawl <- "https://data.axds.co/gs/secoora/wfs?service=WFS&version=1.0.0&request=GetFeature&outputFormat=SHAPE-ZIP&typeName=secoora:marmap_yankee_trawl_1990_2009"
surficial_sediment_texture <- "https://marinecadastre.gov/downloads/data/mc/SurficialSedimentClassification.zip"

nc_biodiversity_wildlife <- "https://ncnhde.natureserve.org/system/files/Biodiversity_and_Habitat_Assessment_2022-07_0.zip"
va_tier_i_species <- "https://dwr.virginia.gov/-/gis-data/Tier1_EssHab.zip"
va_tier_ii_species <- "https://dwr.virginia.gov/-/gis-data/EssHabTier1_2.zip"
se_blueprint <- "https://www.sciencebase.gov/catalog/file/get/64f8da38d34ed30c20546a6a?name=Southeast_Blueprint_2023_Data_Download.zip"

artificial_reef <- "https://marinecadastre.gov/downloads/data/mc/ArtificialReef.zip"

#####################################
#####################################

# Download list
download_list <- c(
  nhpna,
  marea,
  
  summer_flounder,
  bottlenose_dolphin_n_ncar,
  bottlenose_dolphin_s_ncar,
  bottlenose_dolphin_sc_ga_fl,
  
  sc_esi_1996,
  sc_esi_2015,
  nc_esi_2016,
  nc_esi_2011,
  
  coral_hapc,
  coast_mig_pelagic,
  dolphin_wahoo_efh_hapc,
  snapper_grouper,
  tilefish,
  
  shrimp,
  dolphin_wahoo_efh,
  spiny_lobster,
  golden_crab,
  deepwater_coral,
  critical_habitat,
  
  shorebird_nests,
  seamap_loggerhead,
  kemp_ridley_seaturtle,
  coastal_critical_habitat,
  highly_migratory_species,
  cetacean_bia,
  audubon_bird_areas,
  protected_areas,
  deepsea_coral_observations,
  
  deepsea_coral_habitat_suitability,
  coastal_barrier,
  
  va_jones_shore,
  va_state_marsh,
  va_submerged_aquatic,
  va_es_submerged,
  va_wma,
  va_priority_conservation,
  veva,
  mid_atl_artificial_reefs,
  
  mallow_bay,
  national_estuarine_researche,
  coastal_wetland,
  
  lautenberg_coral,
  harbor_porpoise,
  cb_esi_2016,
  
  seagrass,
  coral_mounds,
  se_atl_artificial_reef,
  na_right_whale_sighting,
  piping_plover,
  na_right_whale_season,
  
  crabs,
  oyster_sanctuaries,
  
  weakfish,
  sharpnose_shark,
  croaker,
  spot,
  bluefish,
  southern_kingfish,
  
  marmap,
  vermillion_snapper_trap,
  snow_grouper_short_bottom_ll,
  scup_trap,
  scamp_grouper_trap,
  blackbelly_rosefish_short_bottom_ll,
  red_grouper_trap,
  scamp_grouper_short_bottom_ll,
  sand_perch_trap,
  red_porgy_trap,
  red_snapper_trap,
  red_grouper_short_bottom_ll,
  knobbed_porgy_trap,
  white_grunt_trap,
  snow_grouper_long_bottom_ll,
  gag_grouper_trap,
  golden_tilefish_short_bottom_ll,
  bank_sea_bass_trap,
  black_sea_bass_trap,
  grey_triggerfish_trap,
  tomtate_trap,
  moray_eel_trap,
  spottail_pinfish_trap,
  golden_tilefish_long_bottom_ll,
  white_shrimp,
  brown_shrimp,
  blackfish_trap,
  bottom_longline,
  isaacs_kidd,
  kali_pole,
  short_bottom_longline,
  yankee_trawl,
  surficial_sediment_texture,
  
  nc_biodiversity_wildlife,
  va_tier_i_species,
  va_tier_ii_species,
  se_blueprint,
  artificial_reef
)

#####################################
#####################################

parallel::detectCores()

cl <- parallel::makeCluster(spec = parallel::detectCores(), # number of clusters wanting to create
                            type = 'PSOCK')

work <- parallel::parLapply(cl = cl, X = download_list, fun = data_download_function, data_dir = data_dir)

parallel::stopCluster(cl = cl)

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

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
vms_demarcation <- "https://www.fisheries.noaa.gov/s3/2020-04/vms_demarcation_line_20140925.zip?null"

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
                   nc_shrimp)
  
data_download_function(download_list, data_dir)

#####################################
#####################################

# list all files in data directory
list.files(data_dir)

#####################################

# change KMZ to KML data to get integrated with R

## fyke Net
file.rename(from=file.path(data_dir, "fn.kmz"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "fn.zip"))

unzip(zipfile = file.path(data_dir, "fn.zip"),
      # export file to the new data directory
      exdir = data_dir)

file.rename(from=file.path(data_dir, "doc.kml"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "fn.kml"))

## remove original zipped file
file.remove(file.path(data_dir, "fn.zip"))

#####################################

## pound net
file.rename(from=file.path(data_dir, "pn.kmz"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "pn.zip"))

unzip(zipfile = file.path(data_dir, "pn.zip"),
      # export file to the new data directory
      exdir = data_dir)

file.rename(from=file.path(data_dir, "doc.kml"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "pn.kml"))

## remove original zipped file
file.remove(file.path(data_dir, "pn.zip"))

#####################################

## staked gill net
file.rename(from=file.path(data_dir, "sgn.kmz"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "sgn.zip"))

unzip(zipfile = file.path(data_dir, "sgn.zip"),
      # export file to the new data directory
      exdir = data_dir)

file.rename(from=file.path(data_dir, "doc.kml"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "sgn.kml"))

## remove original zipped file
file.remove(file.path(data_dir, "sgn.zip"))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

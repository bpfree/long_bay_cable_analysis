#############################################
### 0. Download Data -- national security ###
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
    
    dir <- file.path(data_dir, new_dir_name)
  }
}

#####################################
#####################################

# set directories
## define data directory (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data"

#####################################
#####################################

# download data
## national security
### military installations
military_installations <- "https://www.acq.osd.mil/eie/Downloads/DISDI/installations_ranges.zip"

### danger zones and restricted areas
danger_zones <- "https://marinecadastre.gov/downloads/data/mc/DangerZoneRestrictedArea.zip"

### formerly used defense sites
fuds <- "https://marinecadastre.gov/downloads/data/mc/FormerlyUsedDefenseSite.zip"

### military submarine transit lines
military_submarine <- "https://marinecadastre.gov/downloads/data/mc/MilitarySubmarineTransitLane.zip"

### military collection
#### ***note: military collection covers:
####          1.) Military Ship Shock Boxes
####          2.) Military Regulated Airspace
####          3.) Military Special Use Airspace
military_collection <- "https://marinecadastre.gov/downloads/data/mc/MilitaryCollection.zip"

### military surface grid areas
military_surface <- "https://marinecadastre.gov/downloads/data/mc/MilitarySurfaceGridArea.zip"

### military operating area boundaries
military_operating <- "https://marinecadastre.gov/downloads/data/mc/MilitaryOperatingAreaBoundary.zip"

### munitions and explosives of concern
munitions_explosvies <- "https://marinecadastre.gov/downloads/data/mc/MunitionsExplosivesConcern.zip"

#####################################
#####################################

# Download list
download_list <- c(
  # military installations
  military_installations,
  
  # danger zones and restricted areas
  danger_zones,
  
  # formerly used defense sites
  fuds,
  
  # military submarine transit lines
  military_submarine,
  
  # military collection
  ## military ship shock boxes
  ## military regulated airspace
  ## military special use airspace
  military_collection,
  
  # military surface grid areas
  military_surface,
  
  # military operating area boundaries
  military_operating,
  
  # munitions and explosives of concern
  munitions_explosvies
)

data_download_function(download_list, data_dir)

#####################################
#####################################

# list all files in data directory
list.files(data_dir)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

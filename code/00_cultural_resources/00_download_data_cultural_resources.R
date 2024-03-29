##############################################
### 0. Download Data -- cultural resources ###
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
      options(timeout=100000)
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
    
    if (grepl("682786", file)){
      
      # grab text before ".zip" and keep only text before that
      new_dir_name <- "nps_historic"
      
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
      
      ## Virginia habitat permit applications (2023)
      file.rename(from=file.path(data_dir, "Habitat23.kmz"),  # Make default download directory flexible
                  # send to the raw data directory
                  to=file.path(data_dir, "va_habitat.zip"))
      
      unzip(zipfile = file.path(data_dir, "va_habitat.zip"),
            # export file to the new data directory
            exdir = data_dir)
      
      file.rename(from=file.path(data_dir, "doc.kml"),  # Make default download directory flexible
                  # send to the raw data directory
                  to=file.path(data_dir, "va_habitat_permit.kml"))
      
      ## remove original zipped file
      file.remove(file.path(data_dir, "va_habitat.zip"))
    }
    
    if (grepl("open", file)){
      
      new_dir_name <- "nps_historic"
      
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
data_dir <- "data/a_raw_data/cultural_resources"

#####################################
#####################################

# download data
## cultural resources
### North Carolina historic preservation
nc_hist <- "https://public-nps.opendata.arcgis.com/datasets/nps::national-register-of-historic-places-points/about"

### North Carolina socio-economic and political data
nc_esi_2016 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2016_GDB.zip"
nc_esi_2011 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/NCarolina_2011_GDB.zip"

### South Carolina socio-economic and political data
sc_esi_2015 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_2015_GDB.zip"
sc_esi_1996 <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/SCarolina_1996_GDB.zip"

### Virginia boating access locations
va_boating <- "https://dwr.virginia.gov/-/gis-data/Boating_Access_Sites.zip"

### Virginia fishing piers
va_fishing <- "https://dwr.virginia.gov/-/gis-data/Fishing_Piers.zip"

### Virginia birding and wildlife trails
va_birding <- "https://dwr.virginia.gov/-/gis-data/VBWT_Sites_&_Loops.zip"

### Virginia habitat permits
va_habitat_permits <- "https://webapps.mrc.virginia.gov/public/maps/kml/Habitat23.kmz"

### recreation activities
#### 1.) long distance sailing
#### 2.) commercial whale watching areas
#### 3.) recreational boater routes
#### 4.) recreational boater density
recreation <- "https://www.northeastoceandata.org/files/metadata/Themes/Recreation.zip"

### shore-based activities
coastal_recreation <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Recreation/CoastalRecSurvey/REG_Shore_PUG_final.zip"

### Chesapeake Bay socio-economic and political data
cb_esi <- "https://response.restoration.noaa.gov/sites/default/files/esimaps/gisdata/ChesapeakeBay_2016_GDB.zip"

### wildlife and sightseeing activities
wildlife_recreation <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Recreation/CoastalRecSurvey/REG_Sightseeing_PUG_final.zip"

#### underwater activities
underwater_recreation <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Recreation/CoastalRecSurvey/REG_Underwater_PUG_final.zip"

### surface water activities
surface_recreation <- "https://portal.midatlanticocean.org/static/data_manager/data-download/Zip_Files/Recreation/CoastalRecSurvey/REG_Surfacewater_PUG_final.zip"

### National Park Service historic places
nps_historic <- "https://irma.nps.gov/DataStore/DownloadFile/682786"

### historical lighthouses
lighthouses <- "https://marinecadastre.gov/downloads/data/mc/HistoricalLighthouse.zip"

### Indian lands
indian_lands <- "https://marinecadastre.gov/downloads/data/mc/IndianLands.zip"

### Southeast blueprint indicator 2023
#### 1.) greenways and trails
#### 2.) south Atlantic low-urban historic landscapes
#### 3.) urban park size
southeast_blueprint <- "https://www.sciencebase.gov/catalog/file/get/64f8da38d34ed30c20546a6a?name=Southeast_Blueprint_2023_Data_Download.zip"

#####################################
#####################################

# Download list
download_list <- c(
  nc_hist,
  
  nc_esi_2016,
  nc_esi_2011,

  sc_esi_2015,
  sc_esi_1996,

  va_boating,
  va_fishing,
  va_birding,
  va_habitat_permits,

  recreation,
  coastal_recreation,

  cb_esi,

  wildlife_recreation,
  underwater_recreation,
  surface_recreation,
  
  nps_historic,

  lighthouses,

  indian_lands,

  southeast_blueprint
)

data_download_function(download_list, data_dir)

#####################################
#####################################

# list all files in data directory
list.files(data_dir)

#####################################
#####################################

file.rename(from = file.path(data_dir, list.files(data_dir,
                                                  # get the Southeast_Blueprint directory
                                                  pattern = "Southeast_Blueprint")),
            to = file.path(data_dir, "Southeast_Blueprint"))

list.files(data_dir)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

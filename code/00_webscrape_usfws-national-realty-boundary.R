############################################################
### 0. Webscrape Data -- USFS National Realty Boundaries ###
############################################################

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

# Set directories
## download directory
download_dir <- "~/downloads"

## output directory
data_dir <- "data/a_raw_data"

#####################################
#####################################

# Webscrape set-up
## Process uses RSelenium package (learn more about basics here: https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html)
### Another helpful tutorial: https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
### Firefox profile (based on this link: https://yizeng.me/2014/05/23/download-pdf-files-automatically-in-firefox-using-selenium-webdriver/)
fprof <- RSelenium::makeFirefoxProfile(list(
  # detail level for download (0 = Desktop, 1 = systems default downloads location, 2 = custom folder.)
  browser.download.folderList = 2,
  # location for data download
  browser.download.dir = data_dir,
  # stores a comma-separated list of MIME types to save to disk without asking what to use to open the file
  browser.helperApps.neverAsk.saveToDisk = "application/pdf",
  # disable PDF javascript so that PDFs are not displayed
  pdfjs.disabled = TRUE,
  # turn off scan and loading of any additionally added plug-ins
  plugin.scan.plid.all = FALSE,
  # high number defined for version of Adobe Acrobat
  plugin.scan.Acrobat = "99.0"))

#####################################

# Launch RSelenium server and driver
rD <- RSelenium::rsDriver(browser="firefox",
                          # set which version of browser
                          version = "latest",
                          # Chrome version (turn off as Firefox will be used)
                          chromever = NULL,
                          # set which version of Gecko to use
                          geckover = "latest",
                          # status messages
                          verbose = TRUE,
                          # populate with the Firefox profile
                          extraCapabilities = fprof)

## Remote driver
remDr <- rD[["client"]]
remDr$open(silent = TRUE)

#####################################
#####################################

# Navigates to National Realty Boundary (source: https://gis-fws.opendata.arcgis.com/datasets/fws::fws-national-realty-boundaries/explore)
# and scrapes the site data for the states and regions of interest

## About FWS NRB data: https://gis-fws.opendata.arcgis.com/datasets/fws::fws-national-realty-boundaries/about
## Metadata: https://www.arcgis.com/sharing/rest/content/items/745ed874c1394da3a9aae50267c9e049/info/metadata/metadata.xml?format=default&output=html

# Base URL
base_url <- "https://gis-fws.opendata.arcgis.com/datasets/fws::fws-national-realty-boundaries/explore"

# Navigate to page
remDr$navigate(base_url)
Sys.sleep(5)

#####################################

# Prepare window
remDr$maxWindowSize()
Sys.sleep(2)

#####################################

# Click "Download" toggle on side panel
download_toggle <-remDr$findElement(using = "css selector",
                                    value = ".hub-toolbar-inner > button:nth-child(3)")
download_toggle$clickElement()
Sys.sleep(3)

#####################################

# Click "Download" for the shapefile to get those data
## data are in a shadow form, so need to use shadowr package
shadow_rd <- shadowr::shadow(remDr)
Sys.sleep(3)

## find the calcite (shadow) buttons
geodatabase_panel_button <- shadowr::find_elements(shadow_rd, 'calcite-button')

### Shapefile are the third dataset (after CSV, KML, shapefile, geojson)
### ***Note: if prefer shapefile, swap out a 3 for the 5
geodatabase_panel_button[[5]]$clickElement()

Sys.sleep(10)

#####################################
#####################################

# Close RSelenium servers
remDr$close()
rD$server$stop()

#####################################
#####################################

# View files in downloads folder to make sure file is there
list.files(download_dir)

# Move data to correct directory
file.rename(from=file.path("~/Downloads", "FWS_National_Realty_Boundaries.gdb.zip"),  # Make default download directory flexible
            # send to the raw data directory
            to=file.path(data_dir, "FWS_National_Realty_Boundaries.gdb.zip"))

#####################################

# Unzip the data
## grab text before ".zip" and keep only text before that
new_dir_name <- sub(".zip", "", "FWS_National_Realty_Boundaries.gdb.zip")

## create new directory for data
new_dir <- file.path(data_dir, new_dir_name)

## unzip the file
unzip(zipfile = file.path(data_dir, "FWS_National_Realty_Boundaries.gdb.zip"),
      # export file to the new data directory
      exdir = data_dir)

list.files(data_dir)

## remove original zipped file
file.remove(file.path(data_dir, "FWS_National_Realty_Boundaries.gdb.zip"))

# Rename geodatabase
file.rename(from = file.path(data_dir,
                             # find original name
                             list.files(data_dir,
                                        # search for pattern that matches dataset
                                        pattern = "53060d2e-c488-4129-b64a-b712b868eccd.gdb")),
            to = file.path(data_dir, "fws_nrb.gdb"))

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

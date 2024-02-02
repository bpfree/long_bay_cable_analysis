##################################################
### 1. Webscrape Data -- GCOOS Regional Assets ###
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
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr,
               tidyverse)

#####################################
#####################################

# Set directories
## output directory
gcoos_gpkg <- "data/a_raw_data/gcoos_sensors.gpkg"

#####################################
#####################################

# Webscrape set-up
## Process uses RSelenium package (learn more about basics here: https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html)
### Another helpful tutorial: https://joshuamccrain.com/tutorials/web_scraping_R_selenium.html
### Firefox profile (based on this link: https://yizeng.me/2014/05/23/download-pdf-files-automatically-in-firefox-using-selenium-webdriver/)
fprof <- RSelenium::makeFirefoxProfile(list(
  # detail level for download (0 = Desktop, 1 = systems default downloads location, 2 = custom folder.)
  browser.download.folderList = 2L,
  # location for data download
  browser.download.dir = outputdir,
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

## Set client (to be used in the zoom function)
client <- rD$client

#####################################
#####################################

# Navigates to GCOOS Inventory of Assets regional page (source: https://data.gcoos.org/inventory.php#tabs-2)
# and scrapes the site data for the states and regions of interest

# Base URL
regional_url <- "https://data.gcoos.org/inventory.php#tabs-2"

# Navigate to page
remDr$navigate(regional_url)

Sys.sleep(5)

regional <- remDr$getPageSource()[[1]]

# Make table from the results of the search
## Read HTML page to create the table
gcoos_regional_table <- rvest::read_html(regional) %>%
  rvest::html_nodes("#tabs-2") %>%
  # obtain the table
  rvest::html_element(css = "table") %>%
  # read the table to get it as a data frame
  rvest::html_table() %>%
  
  # set as a data frame
  as.data.frame() %>%
  # change any blank values to NA
  mutate_all(na_if,"") %>%

  # create sensor field to help spot duplicates across datasets
  dplyr::mutate(sensor = stringr::word(Platform.Station, start = 1, end = 1)) %>%
  # remove colon from sensor name
  dplyr::mutate(sensor = stringr::str_remove(sensor, pattern = "[:]")) %>%
  # remove dash from sensor name
  dplyr::mutate(sensor = stringr::str_remove(sensor, pattern = "[–]")) %>%
  # keep only needed fields
  dplyr::select(Lon, Lat,
                sensor, Status) %>%
  # obtain only active sensors
  dplyr::filter(Status %in% c("Active",
                              "Inactive")) %>%
  # reduce to only places that have NA values
  na.omit() %>%
  # rename "Status" field
  dplyr::rename("status" = "Status") %>%
  
  # convert to simple feature
  sf::st_as_sf(coords = c("Lon", "Lat"),
               # set the coordinate reference system to WGS84
               crs = 4326)

#####################################

### Federal Assets (source: https://data.gcoos.org/inventory.php#tabs-3)
#### ***Note: Copano Bay and Copano Bay East have same coordinates (two different sensors)
#### ***Note: Middle Bay and Magnolia River have same coordinates -- seems like incorrect entry
#### ***Note: If objective is to have single sensor, can use the dplyr::distinct() function to return only unique locations (will need to remove sensor and status fields)

# Base URL
federal_url <- "https://data.gcoos.org/inventory.php#tabs-3"

# Navigate to page
remDr$navigate(federal_url)

federal <- remDr$getPageSource()[[1]]

# Make table from the results of the search
## Read HTML page to create the table
gcoos_federal_table <- rvest::read_html(federal) %>%
  rvest::html_nodes("#tabs-3") %>%
  # obtain the table
  rvest::html_element(css = "table") %>%
  # read the table to get it as a data frame
  rvest::html_table() %>%
  
  # set as a data frame
  as.data.frame() %>%
  # change any blank values to NA
  dplyr::mutate_all(dplyr::na_if, "") %>%
  
  # create sensor field to help spot duplicates across datasets
  dplyr::mutate(sensor = stringr::word(Platform.Station, start = 1, end = 1)) %>%
  # remove colon from sensor name
  dplyr::mutate(sensor = stringr::str_remove(sensor, pattern = "[:]")) %>%
  # keep only needed fields
  dplyr::select(Lon, Lat,
                sensor, Status) %>%
  # obtain only active sensors
  dplyr::filter(Status %in% c("Active",
                              "Inactive")) %>%
  # reduce to only places that have NA values
  na.omit() %>%
  # rename "Status" field
  dplyr::rename("status" = "Status") %>%
  
  # convert to simple feature
  sf::st_as_sf(coords = c("Lon", "Lat"),
               # set the coordinate reference system to WGS84
               crs = 4326)

#####################################
#####################################

# Close RSelenium servers
remDr$close()
rD$server$stop()

#####################################
#####################################

# Export data
sf::st_write(obj = gcoos_regional_table, dsn = gcoos_gpkg, layer = "gcoos_regional_sensor_assets", append = F)
sf::st_write(obj = gcoos_federal_table, dsn = gcoos_gpkg, layer = "gcoos_federal_sensor_assets", append = F)

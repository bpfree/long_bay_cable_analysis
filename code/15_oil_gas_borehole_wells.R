########################################################
### 15. Oil and Gas Boreholes, Test Wells, and Wells ###
########################################################

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
## Define data directory (as this is an R Project, pathnames are simplified)
### Input directories
borehole_dir <- "data/a_raw_data"

### Output directories
#### Analyis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
borehole_gpkg <- "data/b_intermediate_data/borehole.gpkg"

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

#####################################
#####################################

# Load borehole data (source: https://www.data.boem.gov/Well/Borehole/Default.aspx)
## Query Definitions: https://www.data.boem.gov/Well/Borehole/Default.aspx
## Metadata / Field Definitions: https://www.data.boem.gov/Main/HtmlPage.aspx?page=borehole
## Field Values: https://www.data.boem.gov/Main/HtmlPage.aspx?page=boreholeFields
### Borehole Status Code
####   1.) APD -- Application for permit to drill
####   2.) AST -- Approved sidetrack
####   3.) BP -- Bypass
####   4.) CNL -- Borehole is cancelled. The request to drill the well is cancelled after the APD or sundry has been approved. The status date of the borehole was cancelled.
####   5.) COM -- Borehole completed
####   6.) CT -- Core test well
####   7.) DRL -- Drilling active
####   8.) DSI -- Drilling suspended
####   9.) PA -- Permanently abandoned
####   10.) ST -- Borehole side tracked
####   11.) TA -- temporarily abandoned
####   12.) VCW -- Volume chamber well

### Type Code
####   1.) C -- Core test
####   2.) D -- Development
####   3.) E -- Exploratory
####   4.) N -- Non-operation
####   5.) O -- Other
####   6.) R -- Relief
####   7.) S -- Strat test

## Data were up-to-date as of 18 January 2023
borehole <- read.csv(paste(borehole_dir, "borehole.csv", sep = "/")) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("Surface.Longitude", "Surface.Latitude"),
               # According to BOEM, coordinate data are in NAD27 (EPSG:4267)
               crs = 4267) %>% # EPSG:4267 (https://epsg.io/4267)
  # reproject the coordinate reference system to match study area data (EPSG:5070)
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # remove any boreholes that have been side tracked or permanently abandoned
  # return all not status codes CNL, PA and ST
  dplyr::filter(!Status.Code %in% c("CNL", "PA", "ST")) %>%
  # obtain only borehole wells in the study area
  sf::st_intersection(study_area) %>%
  #  add a setback (buffer) distance of 60.96 meters (200 feet) around the boreholes
  sf::st_buffer(dist = 60.96) %>%
  # create field called "layer" and fill with "borehole" for summary
  dplyr::mutate(layer = "borehole") %>%
  # group all features by the "layer" and "value" fields to then have a single feature
  # "value" will get pulled in from the study area layer
  dplyr::group_by(layer,
                  value) %>%
  # summarise data to obtain single feature
  dplyr::summarise()

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = borehole, dsn = analysis_gpkg, "borehole", append = F)

## Borehole geopackage
sf::st_write(obj = borehole, dsn = borehole_gpkg, "boreholes", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
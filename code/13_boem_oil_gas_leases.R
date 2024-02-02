#####################################
### 13. Active Oil and Gas Leases ###
#####################################

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
oilgas_lease_dir <- "data/a_raw_data/ActiveLeasePolygons.gdb/ActiveLeasePolygons.gdb"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directory
oilgas_lease_gpkg <- "data/b_intermediate_data/boem_oilgas_lease.gpkg"

#####################################
#####################################

# View layer names within geodatabase
sf::st_layers(dsn = oilgas_lease_dir,
              do_count = TRUE)

# Obtain layer name by pulling the observation from the first column
layer_date <- sf::st_layers(dsn = oilgas_lease_dir,
                            do_count = TRUE)[[1]]

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

# Load BOEM active oil and gas lease data (source: https://www.data.boem.gov/Main/Mapping.aspx#ascii)
## Geodatabase download link: https://www.data.boem.gov/Mapping/Files/ActiveLeasePolygons.gdb.zip
## Shapefile download link: https://www.data.boem.gov/Mapping/Files/actlease.zip

## Metadata: https://www.data.boem.gov/Mapping/Files/actlease_meta.html
## ***Note: data are updated each month near the first of the month
## These data were updated as of 3 January 2023

### Status codes:
#### 1.) PROD = A lease held by production of a mineral
#### 2.) UNIT = Lease (or portion thereof) included in an approved unit agreement
#### 3.) SOP = Initial term extended due to ordering or appro by Dir of SOP
#### 4.) OPERNS = Initial term extended because of activity on the leased area
#### 5.) DSO = Operations/activities on all or part of lease suspended/temp prohibited on Reg Sup initiative. Lease term extended
#### 6.) PRIMRY = A lease within the initial term of the contract (5, 8, or 10 years)

## On note per Shane Stradley (shane.stradley@boem.gov)
### LEASE_EXPIR_DATE should be null (not expired yet) -- all records are NA (list(unique(oil_gas_lease_areas$LEASE_EXPIR_DATE)))
### LSE_STAT_EFF_DT should not be null (since status should be effective at time of interest)
### DSO, UNIT, PRIMRY, OPERNS, PROD, SOO, SOP are considered active

## Layer name should automatically update to keep up with the new data if more updated than 03 January 2023
oil_gas_lease_areas <- sf::st_read(dsn = oilgas_lease_dir,
                                   # use the layer name from the geodatabase
                                   layer = layer_date) %>%
  # reproject the coordinate reference system to match study area data (EPSG:5070)
  sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
  # obtain only active oil and gas lease blocks in the study area
  sf::st_intersection(study_area) %>%
  # create field called "layer" and fill with "active oil and gas lease" for summary
  dplyr::mutate(layer = "active oil and gas lease") %>%
  ## group all features by the "layer" and "value" fields to then have a single feature
  dplyr::group_by(layer,
                  value) %>%
  # summarise data to obtain single feature
  dplyr::summarise()

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = oil_gas_lease_areas, dsn = analysis_gpkg, "oil_gas_lease_areas", append = F)

## Active oil and gas lease blocks geopackage
sf::st_write(obj = oil_gas_lease_areas, dsn = oilgas_lease_gpkg, "oil_gas_lease_areas", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
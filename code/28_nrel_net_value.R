#################################################
### 28. NREL Net Value Offshore Wind -- 2015  ###
#################################################

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
## Define data directories (as this is an R Project, pathnames are simplified)
### Input directories
input_dir <- "data/a_raw_data"
lease_dir <- "data/a_raw_data/Blocks.gdb/BlockPolygons.gdb"
raster_dir <- "data/d_raster_data"

### Output directories
#### Analysis directory
analysis_gpkg <- "data/c_analysis_data/gom_cable_study.gpkg"

#### Intermediate directories
intermediate_dir <- "data/b_intermediate_data"
osw_value_gpkg <- "data/b_intermediate_data/offshore_wind_potential.gpkg"

# View layer names within geodatabase
sf::st_layers(dsn = lease_dir,
              do_count = TRUE)

#####################################
#####################################

# Load study area (to clip habitats to only that area)
study_area <- sf::st_read(dsn = analysis_gpkg, layer = "gom_study_area_marine")

# Load raster grid
gom_raster <- raster::raster(paste(raster_dir, "gom_study_area_marine_100m_raster.grd", sep = "/"))

#####################################
#####################################

# Load data
## BOEM Lease Blocks (source: https://www.data.boem.gov/Mapping/Files/Blocks.gdb.zip) -- shapefile format is also available
### Metadata: https://www.data.boem.gov/Mapping/Files/blocks_meta.html
lease_blocks <- sf::st_read(dsn = lease_dir, layer = "BlockPolygons") %>%
  # reproject the coordinate reference system
  sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)

## NREL Net Value -- 2015 (source: https://data.nrel.gov/system/files/67/170514_OSW%20cost%20analysis_output%20file%20%281%29.xlsx)
### Data page: https://data.nrel.gov/submissions/67, report: https://www.nrel.gov/docs/fy17osti/67675.pdf
### ***Note: Data come as an Excel spreadsheet. To use data, can delete tabs except 2015 (COD) and save as CSV
### ***Note: The data were renamed as nrel_2015_net_value.xlsx, but any name can be used (and change .xlsx )
nrel_net_value <- readxl::read_xlsx(path = paste(input_dir, "nrel_2015_net_value.xlsx", sep = "/"),
                                    # designate the sheet with the data, in this case 2 = 2015 (COD)
                                    sheet = 2) %>%
  dplyr::rename("net.value" = "Net value") %>%
  # remove any features lacking data (there seem to be 113 features with NA values in entire dataset -- all in Hawaii)
  dplyr::filter(!is.na(net.value)) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               # set the coordinate reference system to WGS84
               crs = 4326) %>% # EPSG 4326 (https://epsg.io/4326)
  # reproject the coordinate reference system
  sf::st_transform("EPSG:5070") # EPSG 5070 (https://epsg.io/5070)

#####################################
#####################################

# Join NREL Net Value data to BOEM lease block data
nrel_net_value_lease_blocks <- lease_blocks %>%
  sf::st_join(y = nrel_net_value,
              # see ?sf::st_join for join options (e.g., st_within, st_covers, st_touches, etc.)
              join = st_contains,
              # use left join, FALSE = inner join
              left = TRUE) %>%
  # obtain sensor data within study area
  sf::st_intersection(study_area) %>%
  # select fields of importance
  dplyr::select(net.value) %>%
  # rename field
  dplyr::rename("value" = "net.value") %>%
  # create layer field
  dplyr::mutate(layer = "nrel net value") %>%
  # reoder fields
  dplyr::relocate(layer, .before = value)

#####################################
#####################################

# Convert data to a raster
nrel_net_value_raster <- nrel_net_value_lease_blocks %>%
  terra::rasterize(x = .,
                   y = gom_raster,
                   field = "value")

#####################################
#####################################

g <- ggplot2::ggplot() + 
  ggplot2::geom_sf(data = study_area, fill = NA, color = "blue", linetype = "dashed") +
  ggplot2::geom_sf(data = nrel_net_value_lease_blocks, color = "orange", fill = NA)
g

#####################################
#####################################

# Export data
## Analysis geopackage
sf::st_write(obj = nrel_net_value_lease_blocks, dsn = analysis_gpkg, "nrel_net_value_lease_blocks", append = F)

## NREL Net Value geopackage
sf::st_write(obj = nrel_net_value_lease_blocks, dsn = osw_value_gpkg, "nrel_net_value_lease_blocks", append = F)

## Intermediate data
terra::writeRaster(nrel_net_value_raster, filename = file.path(intermediate_dir, "nrel_net_value_raster.grd"), overwrite = T)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
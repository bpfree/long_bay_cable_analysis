########################################
### 34. Wind Farm Starting Location  ###
########################################

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
wea_gdb <- "data/a_raw_data/WeaConfiguration4_shp.gdb"

### Output directories
#### Least Cost Path directory
least_cost_gpkg <- "data/e_least_cost_path/least_cost_path_analysis.gpkg"

#### Intermediate directory
wind_start_gpkg <- "data/b_intermediate_data/wind_farm_start_point.gpkg"

# View layer names within geodatabase
sf::st_layers(dsn = wea_gdb,
              do_count = TRUE)

#####################################
#####################################

# Load wind farm data
wind_area <- sf::st_read(dsn = wea_gdb, layer = "WEA_I_configuration_4_dissolved")

g <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = wind_area, color = "blue")
g

#####################################
#####################################

# Create starting point
wind_area_points <- wind_area %>%
  # transform coordinate system to be in decimal degrees for limiting area of interest
  sf::st_transform("EPSG:4326") %>%
  # create wind area as an object composed of points
  sf::st_cast("POINT") %>%
  # create new fields from the geometry 
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  # limit to the boundaries to northeastern area (dd = 28.89 N, -94.5 W) or
  # southwestern area (dd = 28.76 N, -94,825)
  dplyr::filter(lat >= 28.89 & # and
                  lon >= -94.72 | # or
                  # southwestern corner
                  lat <= 28.76 & # and
                  lon <= -94.825) %>%
  # transform coordinate system back to EPSG:5070 to match all other data
  sf::st_transform("EPSG:5070")

#####################################

g <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = wind_area, color = "blue") +
  ggplot2::geom_sf(data = wind_area_points, color = "red")
g

#####################################
#####################################

# Randomly sample 10 points to set as starting points
wind_starting_point <- wind_area_points %>%
  dplyr::sample_n(10)

#####################################

g <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = wind_area, color = "blue") +
  ggplot2::geom_sf(data = wind_area_points, color = "red") +
  ggplot2::geom_sf(data = wind_starting_point, color = "black")
g

#####################################
#####################################

# Export data
## Least cost geopackage
sf::st_write(obj = wind_starting_point, dsn = least_cost_gpkg, "starting_site", append = F)

## Landing sites geopackage
sf::st_write(obj = wind_starting_point, dsn = wind_start_gpkg, "starting_site", append = F)
sf::st_write(obj = wind_area_points, dsn = wind_start_gpkg, "wind_area_points", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate
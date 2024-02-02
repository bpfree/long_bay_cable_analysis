####################################################
### 38. Least Cost Path -- leastcostpath package ###
####################################################

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

# leastcostpath Documentation
## Manual: https://cran.r-project.org/web/packages/leastcostpath/leastcostpath.pdf
## User Guide: https://cran.r-project.org/web/packages/leastcostpath/vignettes/leastcostpath-1.html
## GitHub: https://github.com/josephlewis/leastcostpath

# load most recent version of "leastcostpath"
library(devtools)
install_github("josephlewis/leastcostpath")
library(leastcostpath)

# Inspect the versions of the packages
sessionInfo()

#####################################
#####################################

# Set directories
## Define data directory (as this is an R Project, pathnames are simplified)
### Input directories
least_cost_dir <- "data/e_least_cost_path"
least_cost_gpkg <- "data/e_least_cost_path/least_cost_path_analysis.gpkg"

#####################################

sf::st_layers(dsn = least_cost_gpkg,
              do_count = TRUE)

#####################################
#####################################

# Load data
## Starting point
starting_points <- sf::st_read(dsn = least_cost_gpkg, layer = "starting_site")

## Landing point
landing_points <- sf::st_read(dsn = least_cost_gpkg, layer = "landing_areas")

#### a cost surface with barriers removed from overall data
costs_barriers_extracted <- terra::rast(paste(least_cost_dir, "cost_rm_constraints.grd", sep = "/")) %>%
  # reclassify the values to have values only between 0 and maximum (6.235173)
  terra::classify(., cbind(terra::minmax(.)[1], 0.01, NA))

#####################################
#####################################

# get starting points for northeast
start_ne <- starting_points %>%
  dplyr::filter(lat >= 28.89 &
                lon >= -94.72) %>%
  sample_n(1)

# get starting point for southwest
start_sw <- starting_points %>%
  dplyr::filter(lat <= 28.76 &
                lon <= -94.825) %>%
  sample_n(1)

# combine starting points
start_points <- start_ne %>%
  rbind(start_sw)

#####################################

# landing points (Galveston)
landing_galveston <- landing_points %>%
  filter(endID != 300463)

landing_western <- landing_points %>%
  filter(endID == 300463)

## Swapping values
max <- minmax(costs_barriers_extracted)[2,]

new_value <- max - costs_barriers_extracted

#####################################
#####################################

# create slope cost surface
cs4 <- leastcostpath::create_cs(x = new_value,
                               # neighbors = 4, 8, 16, 32, 48
                               neighbours = 4)

cs8 <- leastcostpath::create_cs(x = new_value,
                               # neighbors = 4, 8, 16, 32, 48
                               neighbours = 8)

cs16 <- leastcostpath::create_cs(x = new_value,
                                # neighbors = 4, 8, 16, 32, 48
                                neighbours = 16)

cs32 <- leastcostpath::create_cs(x = new_value,
                                # neighbors = 4, 8, 16, 32, 48
                                neighbours = 32)

cs48 <- leastcostpath::create_cs(x = new_value,
                                 # neighbors = 4, 8, 16, 32, 48
                                 neighbours = 48)

#####################################
#####################################

# create least cost path
lcp4 <- leastcostpath::create_lcp(x = cs4,
                                  origin = starting_points,
                                  destination = landing_points,
                                  cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 4)

lcp8 <- leastcostpath::create_lcp(x = cs8,
                                  origin = starting_points,
                                  destination = landing_points,
                                  cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 8)

lcp16 <- leastcostpath::create_lcp(x = cs16,
                                   origin = starting_points,
                                   destination = landing_points,
                                   cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 16)

lcp32 <- leastcostpath::create_lcp(x = cs32,
                                   origin = starting_points,
                                   destination = landing_points,
                                   cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 32)

lcp48 <- leastcostpath::create_lcp(x = cs48,
                                   origin = starting_points,
                                   destination = landing_points,
                                   cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 48)

#####################################

# create least cost path (Galveston)
lcp4_galveston <- leastcostpath::create_lcp(x = cs4,
                                            origin = start_ne,
                                            destination = landing_galveston,
                                            cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 4)

lcp8_galveston <- leastcostpath::create_lcp(x = cs8,
                                            origin = start_ne,
                                            destination = landing_galveston,
                                            cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 8)

lcp16_galveston <- leastcostpath::create_lcp(x = cs16,
                                             origin = start_ne,
                                             destination = landing_galveston,
                                             cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 16)

lcp32_galveston <- leastcostpath::create_lcp(x = cs32,
                                             origin = start_ne,
                                             destination = landing_galveston,
                                             cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 32)

lcp48_galveston <- leastcostpath::create_lcp(x = cs48,
                                             origin = start_ne,
                                             destination = landing_galveston,
                                             cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 48)

#####################################

# create least cost path (southwestern)
lcp4_southwestern <- leastcostpath::create_lcp(x = cs4,
                                               origin = start_sw,
                                               destination = landing_western,
                                               cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 4)

lcp8_southwestern <- leastcostpath::create_lcp(x = cs8,
                                               origin = start_sw,
                                               destination = landing_western,
                                               cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 8)

lcp16_southwestern <- leastcostpath::create_lcp(x = cs16,
                                                origin = start_sw,
                                                destination = landing_western,
                                                cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 16)

lcp32_southwestern <- leastcostpath::create_lcp(x = cs32,
                                                origin = start_sw,
                                                destination = landing_western,
                                                cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 32)

lcp48_southwestern <- leastcostpath::create_lcp(x = cs48,
                                                origin = start_sw,
                                                destination = landing_western,
                                                cost_distance = TRUE) %>%
  dplyr::mutate(neighbors = 48)

#####################################
#####################################

tile_raster <- new_value %>%
  as.data.frame(xy=T) %>%
  setNames(c("longitude", "latitude", "cost"))

g <- ggplot() +
  geom_tile(data = tile_raster, aes(x = longitude, y = latitude, fill = cost)) +
  geom_sf(data = starting_points, color = "red", size = 2) +
  geom_sf(data = landing_points, size = 2, color = "black") +
  geom_sf(data = lcp4, color = "yellow") +
  geom_sf(data = lcp8, color = "green") +
  geom_sf(data = lcp16, color = "orange") +
  geom_sf(data = lcp32, color = "purple") +
  geom_sf(data = lcp48, color = "pink")
g

g2 <- ggplot() +
  geom_tile(data = tile_raster, aes(x = longitude, y = latitude, fill = cost)) +
  geom_sf(data = start_ne, color = "red", size = 2) +
  geom_sf(data = landing_galveston, size = 2, color = "black") +
  geom_sf(data = lcp4_galveston, color = "yellow") +
  geom_sf(data = lcp8_galveston, color = "green") +
  geom_sf(data = lcp16_galveston, color = "orange") +
  geom_sf(data = lcp32_galveston, color = "purple") +
  geom_sf(data = lcp48_galveston, color = "pink")
g2

g3 <- ggplot() +
  geom_tile(data = tile_raster, aes(x = longitude, y = latitude, fill = cost)) +
  geom_sf(data = start_sw, color = "red", size = 2) +
  geom_sf(data = landing_western, size = 2, color = "black") +
  geom_sf(data = lcp4_southwestern, color = "yellow") +
  geom_sf(data = lcp8_southwestern, color = "green") +
  geom_sf(data = lcp16_southwestern, color = "orange") +
  geom_sf(data = lcp32_southwestern, color = "purple") +
  geom_sf(data = lcp48_southwestern, color = "pink")
g3

#####################################
#####################################

# Combine lines
## combined all
lcp_lines <- lcp4 %>%
  rbind(lcp8,
        lcp16,
        lcp32,
        lcp48)

#####################################

## Galveston (northeastern)
lcp_lines_galveston <- lcp4_galveston %>%
  rbind(lcp8_galveston,
        lcp16_galveston,
        lcp32_galveston,
        lcp48_galveston)

#####################################

## Southwestern
lcp_lines_southwestern <- lcp4_southwestern %>%
  rbind(lcp8_southwestern,
        lcp16_southwestern,
        lcp32_southwestern,
        lcp48_southwestern)

#####################################
#####################################

# Export data
## Cost paths
st_write(obj = lcp_lines, dsn = least_cost_gpkg, "gom_lcp_lines", append = F)
st_write(obj = lcp_lines_galveston, dsn = least_cost_gpkg, "gom_lcp_lines_galveston", append = F)
st_write(obj = lcp_lines_southwestern, dsn = least_cost_gpkg, "gom_lcp_lines_southwestern", append = F)

st_write(obj = start_ne, dsn = least_cost_gpkg, "wind_area_starting_point_ne", append = F)
st_write(obj = start_sw, dsn = least_cost_gpkg, "wind_area_starting_point_sw", append = F)
st_write(obj = start_points, dsn = least_cost_gpkg, "wind_area_starting_points", append = F)

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

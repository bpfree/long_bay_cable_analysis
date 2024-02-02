#############################
### X. Miscellaneous Code ###
#############################

#####################################
# Code 1
#####################################

# ### ***Note: Alternative option is to use Natural Earth has data for Administration Level 2 (states) (website: https://www.naturalearthdata.com/)
# ### The data from ne_states() pulls in the 10m resolution
# ### rnaturalearth package provides quick access to import the data (https://cran.r-project.org/web/packages/rnaturalearth/README.html)
# texas <- rnaturalearth::ne_states(country = "United States of America", geounit = "United States of America", returnclass = "sf") %>%
#   # select only Texas
#   dplyr::filter(name == "Texas") %>%
#   # reproject the coordinate reference system to match BOEM call areas
#   sf::st_transform("EPSG:5070")

#############################

# ## Create polygon for only coastal / marine area
# aoi_marine <- aoi_poly %>%
#   # remove any of the polygon that falls on land (in Texas)
#   sf::st_difference(texas) %>%
#   dplyr::select(name)

##### ***Note: Alternative option
# rast_temp <- raster(extent(aoi_marine),
#                     # cell resolution is 100 meters
#                     res = 100,
#                     # has the came coordinate reference system as the study area
#                     crs = aoi_marine)

##### ***Note: Alternative option if using raster() function; fasterize does not appear compatiable with terra()
# rast_100m <- fasterize::fasterize(sf = aoi_marine,
#                                   raster = rast_temp,
#                                   field = "value")

#############################

# Alternatively, you can make a grid for the entire study area
# In this case, it will have a boundary box that matches the entire
# maximum extent of the study area. Therefore, it will have a regular
# rectugular shape instead of following the waterline boundary as above.

# aoi_100 <- st_make_grid(aoi_marine,
#                         cellsize = 100, # units are in meter
#                         offset = st_bbox(aoi_marine)[c("xmin", "ymin")],
#                         what = "polygons",
#                         crs = 5070, # NAD83 / Conus Albers
#                         square = TRUE, # keep it as square
#                         flat_topped = TRUE) %>%
#   # convert to sf (simple feature)
#   st_sf() %>%
#   # create value field
#   dplyr::mutate(value = 0)
# 
# ### Grid with 65 meter cell size
# aoi_65 <- st_make_grid(aoi_marine,
#                        cellsize = 65, # units are in meter
#                        offset = st_bbox(aoi_poly)[c("xmin", "ymin")],
#                        what = "polygons",
#                        crs = 5070, # NAD83 / Conus Albers
#                        square = TRUE, # keep it as square
#                        flat_topped = TRUE) %>%
#   # convert to sf (simple feature)
#   st_sf()
# 
#############################
# 
# If so desired, the grid can be made as a hexagonal grid.
# Rasters cannot use a hexagonal grid, so this will be
# less useful in this case.
#
# ## Hexagonal
# ### Grid with 100 meter cell size
# aoi_hex_100 <- st_make_grid(aoi_marine,
#                             cellsize = 100, # units are in meter
#                             offset = st_bbox(aoi_poly)[c("xmin", "ymin")],
#                             what = "polygons",
#                             crs = 5070, # NAD83 / Conus Albers
#                             square = FALSE, # create hexagonal
#                             flat_topped = TRUE) %>%
#   # convert to sf (simple feature)
#   st_sf()

#############################

# Inspect grid
## Calculate longitude resolution
# xmin_100 <- st_bbox(aoi_100)$xmin
# xmax_100 <- st_bbox(aoi_100)$xmax
# 
# ncol_100 <- (xmax_100 - xmin_100) / 100
# 
# ## Calculate latitude resolution
# ymin_100 <- st_bbox(aoi_100)$ymin
# ymax_100 <- st_bbox(aoi_100)$ymax
# 
# nrow_100 <- (ymax_100 - ymin_100) / 100

#############################

# # Convert to raster
# temp_rast <- raster(xmn = xmin_100, xmx = xmax_100,
#                     ymn = ymin_100, ymx = ymax_100,
#                     res = 100,
#                     crs = 5070)
# 
# aoi_100m_raster <- fasterize(sf = aoi_100,
#                              raster = temp_rast,
#                              field = "value",
#                              fun = "last")

#############################

# ***Note: Alternative, but will take longer
# test_area_remove <- aoi_poly %>%
#   rmapshaper::ms_erase(continents) %>%
#   rmapshaper::ms_erase(big_islands) %>%
#   rmapshaper::ms_erase(small_islands) %>%
#   rmapshaper::ms_erase(very_small_islands)

# ***Note: Alternative option by creating single land features and removing individually
## This feature will allow the study area to target only marine / coastal areas
### Remove any continental land
# aoi_continent <- aoi_poly %>%
#   sf::st_difference(continents)
# 
# ### Then remove any big islands land
# aoi_continent_big <- aoi_continent %>%
#   sf::st_difference(big_islands)
# 
# ### Then remove any continental land
# aoi_continent_big_small <- aoi_continent_big %>%
#   sf::st_difference(small_islands)
# 
# ### Then remove any small islands land
# aoi_continent_big_small_very_small <- aoi_continent_big_small %>%
#   sf::st_difference(very_small_islands)

#############################

# # Export data
# ## Raster
# # writeRaster(aoi_100m_raster, filename = file.path(raster_dir, "gom_study_area_marine_100m_raster.grd"), overwrite = T)

# ## Study area geopackage
# ### Study areas
# sf::st_write(aoi_continent, dsn = study_area_gpkg, layer = "gom_study_area_continental", append = F)
# # sf::st_write(aoi_continent_big, dsn = study_area_gpkg, layer = "gom_study_area_continental_big_island", append = F)
# # sf::st_write(aoi_continent_big_small, dsn = study_area_gpkg, layer = "gom_study_area_continental_big_small_islands", append = F)
# # sf::st_write(aoi_continent_big_small_very_small, dsn = study_area_gpkg, layer = "gom_study_area_continental_big_small__vsmall_islands", append = F)
# # sf::st_write(aoi_marine, dsn = study_area_gpkg, layer = "gom_study_area_marine", append = F)

#####################################
# Code 4
#####################################

# ## Alternative way to add and inspect the bathymetry data
# 
# gom_bathymetry <- ncdf4::nc_open(paste(bathymetry_dir, "crm_vol5.nc", sep = "/"))
# 
# # Inspect bathymetry data
# print(gom_bathymetry)
# 
# # Get variable values
# lon <- ncdf4::ncvar_get(gom_bathymetry, "x")
# lat <- ncdf4::ncvar_get(gom_bathymetry, "y")
# depth <- ncdf4::ncvar_get(gom_bathymetry, "z")
# 
# # Verify depth units are meters
# depth_units <- ncdf4::ncatt_get(gom_bathymetry, "z", "units")
# depth_units$value # yes, the values are in meters
# 
# # Dimensions of variables
# nlon <- dim(lon)
# nlat <- dim(lat)
# 
# # Change missing values and fill values to be "NA"
# gom_bathymetry[["var"]][["z"]][["_FillValue"]] <- NA
# gom_bathymetry[["var"]][["z"]][["missval"]] <- NA

#############################

### Old method:
# tx_bath_mask <- gom_bath %>%
#   # crop to the study area (will be for the extent)
#   terra::crop(study_area) %>%
#   # mask to the study area (show data within the extent)
#   terra::mask(study_area)

#####################################
# Code 18
#####################################

# Alternative Dataset / Method

# bsee_platform_dir <- "data/a_raw_data"
# # Load BSEE drilling platform data (source: https://www.data.bsee.gov/Main/Platform.aspx)
# ## Query: https://www.data.bsee.gov/Platform/PlatformStructures/Default.aspx
# ## Metadata information: https://www.data.bsee.gov/Main/Platform.aspx
# ### ***Note: These data came from generated CSV for all data within the query database
# bsee_platforms <- read.csv(file = paste(bsee_platform_dir, "PlatStruc.csv", sep = "/")) %>%
#   # remove any features that do not have longitude data
#   dplyr::filter(!is.na(Longitude)) %>%
#   # convert to simple feature
#   sf::st_as_sf(coords = c("Longitude", "Latitude"),
#                crs = 4267) %>%
#   # reproject the coordinate reference system to match study area data (EPSG:5070)
#   sf::st_transform("EPSG:5070") %>% # EPSG 5070 (https://epsg.io/5070)
#   # obtain only platforms in study area
#   sf::st_intersection(study_area) %>%
#   # Filter for platforms that have been installed but not yet removed
#   dplyr::filter(Install.Date != "" & # platforms that have an install date (so not blank)
#                   Removal.Date == "") %>% # platforms that lack a removal date (so are blank)
#   #  add a setback (buffer) distance of 152.4 meters (500 feet) around each drilling platform
#   sf::st_buffer(dist = 152.4) %>%
#   # create field called "layer" and fill with "drilling platform" for summary
#   dplyr::mutate(layer = "drilling platform") %>%
#   # group all features by the "layer" and "value" fields to then have a single feature
#   # "value" will get pulled in from the study area layer
#   dplyr::group_by(layer,
#                   value) %>%
#   # summarise data to obtain single feature
#   dplyr::summarise()

#####################################
# Code 32
#####################################

# smf_function_raster <- function(raster){
#   # calculate minimum value
#   min <- raster::minValue(raster)
#   
#   # calculate maximum value
#   max <- raster::maxValue(raster)
#   
#   # calculate s-scores (more desired values get score of 0 while less desired will increase till 1)
#   s_value <- ifelse(raster[] == min, 0, # if value is equal to minimum, score as 0
#                     # if value is larger than minimum but lower than mid-value, calculate based on reduction equation
#                     ifelse(raster[] > min & raster[] < (min + max) / 2, 2*((raster[] - min) / (max - min))**2,
#                            # if value is larger than mid-value but lower than maximum, calculate based on equation
#                            ifelse(raster[] >= (min + max) / 2 & raster[] < max, 1 - 2*((raster[] - max) / (max - min))**2,
#                                   # if value is equal to maximum, score as 1; otherwise give NA
#                                   ifelse(raster[] == max, 1, NA))))
#   
#   # set values back to the original raster
#   pelagic_svalues <- raster::setValues(raster, s_value)
#   
#   # return the raster
#   return(pelagic_svalues)
# }
#   
# pelagic_bird <- raster::raster(paste(intermediate_dir, "pelagic_bird5070.grd", sep = "/")) %>%
#   # reproject so resolution is 100 meters
#   raster::projectRaster(crs = 5070,
#                         res = 100) %>% # resolution should be put in meters as EPSG:5070 is in meters, no longer degrees
#   # crop to the study area (will be for the extent)
#   raster::crop(study_area) %>%
#   # mask to study area
#   raster::mask(study_area)

#############################

# # Create normalized pelagic data
# pelagic_bird_normalize <- pelagic_bird %>%
#   smf_function_raster()
# 
# extent(pelagic_bird_normalize) <- terra::ext(gom_raster)
# nrow(pelagic_bird_normalize) <- nrow(gom_raster)
# 
# # Inspect 
# raster::maxValue(pelagic_bird_normalize) # maximum value = 1
# raster::minValue(pelagic_bird_normalize) # minimum value = 0
# res(pelagic_bird_normalize) # 100 x 100
# hist(pelagic_bird_normalize) # show histogram of values (though mostly values near 1)
# freq(pelagic_bird_normalize) # show frequency of values (though will round to 0 and 1)

#############################

# Export data
## Raster data
#terra::writeRaster(pelagic_bird_normalize, filename = file.path(raster_dir, "pelagic_bird_normalize.grd"), overwrite = T)

## Intermediate data
#terra::writeRaster(pelagic_bird_normalize, filename = file.path(intermediate_dir, "pelagic_bird_normalize.grd"), overwrite = T)

#####################################
# Code 26
#####################################

# not_carbon_capture <- study_area %>%
#   # obtain data outside carbon capture within study area (erase carbon capture area from study area)
#   rmapshaper::ms_erase(carbon_capture)

#####################################
# Code 35
#####################################

### Carbon Capture Lease Blocks
#### ***Areas are located with the active lease blocks
# not_carbon_capture <- sf::st_read(dsn = data_dir, layer = "carbon_capture_lease_blocks") %>%
#   # add cost value of 0.5
#   dplyr::mutate(value = 0.5) %>%
#   # rasterize data
#   terra::rasterize(y = gom_raster,
#                    field = "value")

# temp_r = raster(matrix(sample(2:1000, 10000, replace = TRUE), 100, 100))
# min <- cellStats(temp_r, "min")
# max <- cellStats(temp_r, "max")
# temp_z_values <- zmf_function(temp_r, min, max)
# max(temp_z_values, na.rm = T)
# hist(temp_z_values)
# temp_new <- setValues(temp_r, temp_z_values)
# hist(temp_new)
# freq(temp_new, by = 0.1)
# max(temp_new, na.rm = T)




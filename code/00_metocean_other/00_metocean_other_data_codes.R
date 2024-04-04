############################################
### 0. Data codes -- metocean and others ###
############################################

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

# set parameters
region <- "vcar"
submodel <- "mo"

#####################################
#####################################

# set directories
## define data directories (as this is an R Project, pathnames are simplified)
data_dir <- "data/a_raw_data/metocean_other"

list.files(data_dir)
list.dirs(data_dir, recursive = TRUE)

## national security submodel geopackage
metocean_other_geopackage <- "data/a_raw_data/metocean_other/metocean_other.gpkg"

#####################################
#####################################

# load metocean and other datasets
## South Carolina general geology
sc_geology <- sf::st_read(dsn = file.path(data_dir, "sc_geology.gpkg"),
                          layer = sf::st_layers(dsn = file.path(data_dir, "sc_geology.gpkg"))[[1]][1])

## South Carolina storm tracks
sc_storms <- sf::st_read(dsn = file.path(data_dir, "SC_Hurricanes_Public/SC_Hurricanes_Public.shp"))

## South Carolina approved TMDL sites
tmdl_sites <- sf::st_read(dsn = file.path(data_dir, "tmdl_sites/Approved_TMDL_Sites.shp"))

## section 319 grant projects
section319 <- sf::st_read(dsn = file.path(data_dir, "section319_projects/Section_319_Grant_Projects.shp"))

## North Carolina NPDES stormwater permits
nc_npdes <- sf::st_read(dsn = file.path(data_dir, "nc_npdes_permits/NPDES_Stormwater_Permits.shp"))

## North Carolina state stormwater permits
nc_state_stormwater <- sf::st_read(dsn = file.path(data_dir, "nc_state_permits/STATE_Stormwater_Permits.shp"))

## North Carolina no exposure certifications
nc_no_exposure <- sf::st_read(dsn = file.path(data_dir, "no_exposure_certs/NO_EXPOSURE_Certifications.shp"))

## North Carolina NPDES wastewater discharge permits
nc_wastewater <- sf::st_read(dsn = file.path(data_dir, "nc_wastewater_permits.gpkg"),
                             layer = sf::st_layers(dsn = file.path(data_dir, "nc_wastewater_permits.gpkg"))[[1]][1])

## North Carolina IHA environmental concern
nc_iha <- sf::st_read(dsn = file.path(data_dir, "iha/iha.shp"))

## North Carolina oceanfront erosion rates
### 2020
nc_ocean_erosion_2020 <- sf::st_read(dsn = file.path(data_dir, "er_2020/er_2020.shp"))

### 20213
nc_ocean_erosion_2013 <- sf::st_read(dsn = file.path(data_dir, "er_2013/er_2013.shp"))

## North Carolina oceanfront shorelines (1848 - 2016)
nc_dcm_ocean <- sf::st_read(dsn = file.path(data_dir, "dcm_oceanfront_shorelines/dcm_oceanfront_shorelines.shp"))

## North Carolina ESMP structures (2010 - 2012)
### ***warning: have to manually get within Esri products from ArcGIS Online
nc_esmp <- sf::st_read(dsn = file.path(data_dir, "esmp_structures/esmp_structures.shp"))

## North Carolina estuarine shoreline (2010)
nc_estuarine_shoreline_2010 <- sf::st_read(dsn = file.path(data_dir, "ESMP_shoreline_2007/ESMP_shoreline_2007.shp"))

## North Carolina estuarine shoreline (2012)
nc_estuarine_shoreline_2012 <- sf::st_read(dsn = file.path(data_dir, "shoreline_estuarine_2012/shoreline_estuarine_2012.shp"))

## Southeast bathymetry
southeast_bath <- sf::st_read(dsn = file.path(data_dir, "southeast_bathymetry_meters/southeast_bathymetry_meters.shp"))

## South Carolina ESI -- natural hazard (2015)
sc_esi_2015 <- sf::st_read(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"),
                           layer = sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]][grep(pattern = "NAT_HAZARD_POLY",
                                                                                                                                               x = sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]])])

## North Carolina ESI -- natural hazard (2016)
nc_esi_2016 <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
                           layer = sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]][grep(pattern = "NAT_HAZARD_POLY",
                                                                                                                                       x = sf::st_layers(dsn = file.path(dsn = data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]])])

## tropical cyclone wind exposure -- North Atlantic
na_tropical_wind <- sf::st_read(dsn = file.path(data_dir, "TropicalCycloneWindExposure/TropicalCycloneWindExposure.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "TropicalCycloneWindExposure/TropicalCycloneWindExposure.gdb"))[[1]][grep(pattern = "NorthAtlantic",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "TropicalCycloneWindExposure/TropicalCycloneWindExposure.gdb"))[[1]])])

## sea surface height
sea_surface_height <- sf::st_read(dsn = file.path(data_dir, "SeaSurfaceHeight/SeaSurfaceHeight.gpkg"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SeaSurfaceHeight/SeaSurfaceHeight.gpkg"))[[1]][1])

## bathymetric contours
bathymetry_contours <- sf::st_read(dsn = file.path(data_dir, "BathymetricContour/BathymetryContours.gdb"),
                                   layer = sf::st_layers(dsn = file.path(data_dir, "BathymetricContour/BathymetryContours.gdb"))[[1]][1])


## offshore wind resource potential -- Atlantic
### point
offshore_wind_potential_pt <- sf::st_read(dsn = file.path(data_dir, "nrel_wind/NREL_HourlyWind_Atlantic_polysandpoints.gdb"),
                                          layer = sf::st_layers(dsn = file.path(data_dir, "nrel_wind/NREL_HourlyWind_Atlantic_polysandpoints.gdb"))[[1]][grep(pattern = "annual_atlantic",
                                                                                                                                                              x = sf::st_layers(dsn = file.path(data_dir, "nrel_wind/NREL_HourlyWind_Atlantic_polysandpoints.gdb"))[[1]])])


### polygon
offshore_wind_potential_poly <- sf::st_read(dsn = file.path(data_dir, "nrel_wind/NREL_HourlyWind_Atlantic_polysandpoints.gdb"),
                                            layer = sf::st_layers(dsn = file.path(data_dir, "nrel_wind/NREL_HourlyWind_Atlantic_polysandpoints.gdb"))[[1]][grep(pattern = "Wind_Speed_2017_Ann",
                                                                                                                                                                x = sf::st_layers(dsn = file.path(data_dir, "nrel_wind/NREL_HourlyWind_Atlantic_polysandpoints.gdb"))[[1]])])

## ENOW (2015)
enow_code <- sf::st_read(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"),
                         layer = sf::st_layers(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"))[[1]][grep(pattern = "Coastal",
                                                                                                             x = sf::st_layers(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"))[[1]])]) %>%
  # ***Warning: Pennsylvania appears twice but only one is a good polygon feature (the other basically comes across as lines)
  #             so this removes the bad feature
  dplyr::filter(!row_number() == 19)

### state statistics
enow_state_statistics <- enow_code %>%
  dplyr::inner_join(x = .,
                    y = sf::st_read(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"))[[1]][grep(pattern = "Statistics",
                                                                                                                        x = sf::st_layers(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"))[[1]])]),
                    by = "GEOID")

### state percentages
enow_state_percentages <- enow_code %>%
  dplyr::inner_join(x = .,
                    y = sf::st_read(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"))[[1]][grep(pattern = "Percent",
                                                                                                                        x = sf::st_layers(dsn = file.path(data_dir, "ENOW2015/ENOW2015.gdb"))[[1]])]),
                    by = "GEOID")

## undersea feature names
undersea_features <- sf::st_read(dsn = file.path(data_dir, "undersea_features/undersea_features.shp"))

## mid-Atlantic benthic habitats
benthic_habitat <- sf::st_read(dsn = file.path(data_dir, "Benthic_Habitats/MARCO_data/Marine_Life/Benthic_Habitats/benhab_mab.shp"))

## submarine canyons
submarine_canyons <- sf::st_read(dsn = file.path(data_dir, "Major_Canyons_Update/major_canyons_update/major_canyons.shp"))

## middle Virginia (sea level rise)
### 0 foot
va_middle_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 1 foot
va_middle_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 2 foot
va_middle_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 3 foot
va_middle_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 4 foot
va_middle_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 5 foot
va_middle_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 6 foot
va_middle_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 7 foot
va_middle_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 8 foot
va_middle_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 9 foot
va_middle_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

### 10 foot
va_middle_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                     x = sf::st_layers(dsn = file.path(data_dir, "VA_Middle_slr_data_dist/VA_Middle_slr_final_dist.gdb"))[[1]])])

## southern Virginia (sea level rise)
### 0 foot
va_south_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 1 foot
va_south_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 2 foot
va_south_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 3 foot
va_south_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 4 foot
va_south_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 5 foot
va_south_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 6 foot
va_south_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 7 foot
va_south_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 8 foot
va_south_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 9 foot
va_south_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

### 10 foot
va_south_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                        x = sf::st_layers(dsn = file.path(data_dir, "VA_Southern_slr_data_dist/VA_Southern_slr_final_dist.gdb"))[[1]])])

## Virginia SVI (2010)
va_svi <- sf::st_read(dsn = file.path(data_dir, "SoVI_2010_VA/SoVI2010_VA/SoVI0610_VA.shp"))

## North Carolina AST incidents (petroleum spills)
ast_incidents <- sf::st_read(dsn = file.path(data_dir, "ast_incidents/AST_Incidents.shp"))

## North Carolina UST incidents (petroleum spills)
ust_incidents <- sf::st_read(dsn = file.path(data_dir, "ust.gpkg"),
                             layer = sf::st_layers(dsn = file.path(data_dir, "ust.gpkg"))[[1]][1])

## North Carolina hazardous waste sites
hazardous_waste_sites <- sf::st_read(dsn = file.path(data_dir, "hazardous_waste_sites.gpkg"),
                                     layer = sf::st_layers(dsn = file.path(data_dir, "hazardous_waste_sites.gpkg"))[[1]][1])

## North Carolina notice land use restrictions view
notice_land_use <- sf::st_read(dsn = file.path(data_dir, "notice_land_use.gpkg"),
                               layer = sf::st_layers(dsn = file.path(data_dir, "notice_land_use.gpkg"))[[1]][1])

## North Carolina pre-regulatory landfill sites
pre_reg_landfill <- sf::st_read(dsn = file.path(data_dir, "prereg_landfill_sites.gpkg"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "prereg_landfill_sites.gpkg"))[[1]][1])

## North Carolina dry cleaning historical boiler inspections
dry_cleaning <- sf::st_read(dsn = file.path(data_dir, "dry_cleaning.gpkg"),
                            layer = sf::st_layers(dsn = file.path(data_dir, "dry_cleaning.gpkg"))[[1]][1])

## acidification monitoring locations
acidification <- sf::st_read(dsn = file.path(data_dir, "AcidificationMonitoringMidA_Ver202310/AcidificationMonitoring_MidA_ver202310/AcidificationMonitoringMidA_ver202310.gdb"),
                             layer = sf::st_layers(file.path(data_dir, "AcidificationMonitoringMidA_Ver202310/AcidificationMonitoring_MidA_ver202310/AcidificationMonitoringMidA_ver202310.gdb"))[[1]][1])

## climate change vulnerabilities (compile)
ccv_compile <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                           layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "Compile",
                                                                                                                                                                                             x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (employment)
ccv_employment <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                              layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "Employment",
                                                                                                                                                                                                x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (fisheries)
ccv_fisheries <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                             layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "Fish",
                                                                                                                                                                                               x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (flooding summary)
ccv_flooding <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                            layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "Flood",
                                                                                                                                                                                              x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (high)
ccv_high <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                        layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "High",
                                                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (ranking)
ccv_ranking <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                           layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "Merge_Rank",
                                                                                                                                                                                             x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (ocean employment)
ccv_ocean_employment <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "Ocean_Emp",
                                                                                                                                                                                                      x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (population and housing ranking)
ccv_pop_house_rank <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "House_Rank",
                                                                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (population and housing summary)
ccv_pop_house_summary <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                                     layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "Pop_Housing",
                                                                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (major road and highways)
ccv_road_highways <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "Roads_Rails",
                                                                                                                                                                                                   x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## climate change vulnerabilities (social vulnerability summary)
ccv_social_summary <- sf::st_read(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]][grep(pattern = "SOVI",
                                                                                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb"))[[1]])])

## intrinsic seabed vulnerabilities (bottom trawl, median fishing effort)
bottom_trawl_median <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Median/ISHV_BottomTrawl_Median.shp"))

## intrinsic seabed vulnerabilities (gillnet, median fishing effort)
gill_net_median <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Median/ISHV_Gillnet_Median.shp"))

## intrinsic seabed vulnerabilities (hydraulic clam dredge, median fishing effort)
hyrdaulic_median <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Median/ISHV_Hydraulic_Median.shp"))

## intrinsic seabed vulnerabilities (longline, median fishing effort)
longline_median <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Median/ISHV_Longline_Median.shp"))

## intrinsic seabed vulnerabilities (scallop dredge, median fishing effort)
scallop_median <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Median/ISHV_Scallop_Median.shp"))

## intrinsic seabed vulnerabilities (trap, median fishing effort)
trap_median <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Fishing_Effects_Intrinsic_Seabed_Habitat_Vulnerability/Median/ISHV_Trap_Median.shp"))

## subcatchments
subcatchments <- sf::st_read(dsn = file.path(data_dir, "Subcatchments/Subcatchments.shp"))

## VPDES permit outfalls
vpdes_permit_outfalls <- sf::st_read(dsn = file.path(data_dir, "vpdes_permit_outfalls/VPDES_Permit_Outfalls.shp"))

## VWP (general permits)
vwp_general_permits <- sf::st_read(dsn = file.path(data_dir, "vwp_general_permits/VWP_General_Permits_(GP).shp"))

## VWP (individual permits)
vwp_individual_permits <- sf::st_read(dsn = file.path(data_dir, "vwp_individual_permits/VWP_Individual_Permits_(IP).shp"))

## VWP (non-permits)
vwp_non_permits <- sf::st_read(dsn = file.path(data_dir, "vwp_non_permits/VWP_Non-Permit_(NP).shp"))

## WQA monitoring stations (2018)
wqa_stations <- sf::st_read(dsn = file.path(data_dir, "wqa_stations/2018_Final_WQA_Monitoring_Stations.shp"))

## water quality monitoring plan stations (current)
water_monitoring_stations <- sf::st_read(dsn = file.path(data_dir, "water_quality_monitoring_stations/Water_Quality_Monitoring_Plan_Stations_(Current).shp"))

## Chesapeake Bay ESI
### hydro -- line
cb_hydro_ln <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                           layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "HYDROL",
                                                                                                                                           x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])
### hydro -- polygon
cb_hydro_poly <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                             layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "HYDROP",
                                                                                                                                             x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

### natural hazards
cb_natural_hazards <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "NAT_HAZARD",
                                                                                                                                                  x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

### political
cb_political_pt <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                               layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "POLITICAL_POINT",
                                                                                                                                               x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

cb_political_ln <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                               layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "POLITICAL_LINE",
                                                                                                                                               x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

cb_political_poly <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "POLITICAL_POLY",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

### management
cb_management_pt <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "MANAGED_POINT",
                                                                                                                                                x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

cb_management_poly <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "MANAGED_POLY",
                                                                                                                                                  x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

## middle Atlantic bight
### bottom shear stress (median)
mab_bottom_shear_stress_median <- sf::st_read(dsn = file.path(data_dir, "MAB_median/MAB_median/MAB_median.shp"))

### bottom shear stress (half interpercentile)
mab_bottom_shear_stress_hipr <- sf::st_read(dsn = file.path(data_dir, "MAB_hIPR/MAB_hIPR/MAB_hIPR.shp"))

### bottom shear stress (95th percentile)
mab_bottom_shear_stress_95th <- sf::st_read(dsn = file.path(data_dir, "MAB_95th_perc/MAB_95th_perc/MAB_95th_perc.shp"))

### seabed mobility percentage
mab_seabed_mobility_pct <- sf::st_read(dsn = file.path(data_dir, "MAB_mobile_perc/MAB_mobile_perc/MAB_mobile_perc.shp"))

### seabed mobility recurrence interval
mab_seabed_mobility_recurrence <- sf::st_read(dsn = file.path(data_dir, "MAB_mobile_freq_v1_1/MAB_mobile_freq_v1_1/MAB_mobile_freq_v1_1.shp"))

## south Atlantic bight
### bottom shear stress (median)
sab_bottom_shear_stress_median <- sf::st_read(dsn = file.path(data_dir, "SAB_median/SAB_median/SAB_median.shp"))

### bottom shear stress (half interpercentile)
sab_bottom_shear_stress_hipr <- sf::st_read(dsn = file.path(data_dir, "SAB_hIPR/SAB_hIPR/SAB_hIPR.shp"))

### bottom shear stress (95th percentile)
sab_bottom_shear_stress_95th <- sf::st_read(dsn = file.path(data_dir, "SAB_95th_perc/SAB_95th_perc/SAB_95th_perc.shp"))

### seabed mobility percentage
sab_seabed_mobility_pct <- sf::st_read(dsn = file.path(data_dir, "SAB_mobile_perc/SAB_mobile_perc/SAB_mobile_perc.shp"))

### seabed mobility recurrence interval
sab_seabed_mobility_recurrence <- sf::st_read(dsn = file.path(data_dir, "SAB_mobile_freq_v1_1/SAB_mobile_freq_v1_1/SAB_mobile_freq_v1_1.shp"))

## fishing effects model
### boulder
boulder <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Sediment/Fishing_Effects_Sediment/FishingEffectsSediment.shp")) %>%
  dplyr::select(GridID, Boulder)

### cobble
cobble <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Sediment/Fishing_Effects_Sediment/FishingEffectsSediment.shp")) %>%
  dplyr::select(GridID, Cobble)

### granule and pebble
granule_pebble <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Sediment/Fishing_Effects_Sediment/FishingEffectsSediment.shp")) %>%
  dplyr::select(GridID, GrPe)

### mud
mud <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Sediment/Fishing_Effects_Sediment/FishingEffectsSediment.shp")) %>%
  dplyr::select(GridID, Mud)

### sand
sand <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Sediment/Fishing_Effects_Sediment/FishingEffectsSediment.shp")) %>%
  dplyr::select(GridID, Sand)

### steep / deep
steep_deep <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Sediment/Fishing_Effects_Sediment/FishingEffectsSediment.shp")) %>%
  dplyr::select(GridID, StDeep)

### density
sediment_density <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Sediment/Fishing_Effects_Sediment/FishingEffectsSediment.shp")) %>%
  dplyr::select(GridID, Density)

### sediment diversity
sediment_diversity <- sf::st_read(dsn = file.path(data_dir, "Fishing_Effects_Sediment/Fishing_Effects_Sediment/FishingEffectsSediment.shp")) %>%
  dplyr::select(GridID, Diversity)

## northern North Carolina (sea level rise)
### 0 foot
nc_north_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 1 foot
nc_north_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 2 foot
nc_north_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 3 foot
nc_north_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 4 foot
nc_north_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 5 foot
nc_north_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 6 foot
nc_north_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 7 foot
nc_north_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 8 foot
nc_north_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 9 foot
nc_north_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

### 10 foot
nc_north_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                        x = sf::st_layers(dsn = file.path(data_dir, "NC_Northern_slr_data_dist/NC_Northern_slr_final_dist.gdb"))[[1]])])

## middle 1 North Carolina (sea level rise)
### 0 foot
nc_middle1_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 1 foot
nc_middle1_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 2 foot
nc_middle1_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 3 foot
nc_middle1_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 4 foot
nc_middle1_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 5 foot
nc_middle1_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 6 foot
nc_middle1_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 7 foot
nc_middle1_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 8 foot
nc_middle1_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 9 foot
nc_middle1_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

### 10 foot
nc_middle1_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"),
                                   layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                        x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle1_slr_data_dist/NC_Middle1_slr_final_dist.gdb"))[[1]])])

## middle 2 North Carolina (sea level rise)
### 0 foot
nc_middle2_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 1 foot
nc_middle2_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 2 foot
nc_middle2_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 3 foot
nc_middle2_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 4 foot
nc_middle2_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 5 foot
nc_middle2_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 6 foot
nc_middle2_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 7 foot
nc_middle2_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 8 foot
nc_middle2_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 9 foot
nc_middle2_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

### 10 foot
nc_middle2_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"),
                                   layer = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                        x = sf::st_layers(dsn = file.path(data_dir, "NC_Middle2_slr_data_dist/NC_Middle2_slr_final_dist.gdb"))[[1]])])

## southern 1 North Carolina (sea level rise)
### 0 foot
nc_south1_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 1 foot
nc_south1_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 2 foot
nc_south1_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 3 foot
nc_south1_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 4 foot
nc_south1_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 5 foot
nc_south1_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 6 foot
nc_south1_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 7 foot
nc_south1_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 8 foot
nc_south1_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 9 foot
nc_south1_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

### 10 foot
nc_south1_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                           x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern1_slr_data_dist/NC_Southern1_slr_final_dist.gdb"))[[1]])])

## southern 2 North Carolina (sea level rise)
### 0 foot
nc_south2_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 1 foot
nc_south2_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 2 foot
nc_south2_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 3 foot
nc_south2_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 4 foot
nc_south2_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 5 foot
nc_south2_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 6 foot
nc_south2_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 7 foot
nc_south2_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 8 foot
nc_south2_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 9 foot
nc_south2_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

### 10 foot
nc_south2_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                           x = sf::st_layers(dsn = file.path(data_dir, "NC_Southern2_slr_data_dist/NC_Southern2_slr_final_dist.gdb"))[[1]])])

## northern South Carolina (sea level rise)
### 0 foot
sc_north_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 1 foot
sc_north_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 2 foot
sc_north_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 3 foot
sc_north_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 4 foot
sc_north_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 5 foot
sc_north_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 6 foot
sc_north_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 7 foot
sc_north_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 8 foot
sc_north_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 9 foot
sc_north_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

### 10 foot
sc_north_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                  x = sf::st_layers(dsn = file.path(data_dir, "SC_North_slr_data_dist/SC_North_slr_final_dist.gdb"))[[1]])])

## central South Carolina (sea level rise)
sc_central_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 1 foot
sc_central_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 2 foot
sc_central_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 3 foot
sc_central_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 4 foot
sc_central_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 5 foot
sc_central_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 6 foot
sc_central_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 7 foot
sc_central_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 8 foot
sc_central_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 9 foot
sc_central_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                       x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

### 10 foot
sc_central_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"),
                                   layer = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                        x = sf::st_layers(dsn = file.path(data_dir, "SC_Central_slr_data_dist/SC_Central_slr_final_dist.gdb"))[[1]])])

## southern South Carolina (sea level rise)
sc_south_slr_0ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_0ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 1 foot
sc_south_slr_1ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_1ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 2 foot
sc_south_slr_2ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_2ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 3 foot
sc_south_slr_3ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_3ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 4 foot
sc_south_slr_4ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_4ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 5 foot
sc_south_slr_5ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_5ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 6 foot
sc_south_slr_6ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_6ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 7 foot
sc_south_slr_7ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_7ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 8 foot
sc_south_slr_8ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_8ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 9 foot
sc_south_slr_9ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_9ft",
                                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

### 10 foot
sc_south_slr_10ft <- sf::st_read(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"),
                                 layer = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]][grep(pattern = "slr_10ft",
                                                                                                                                                  x = sf::st_layers(dsn = file.path(data_dir, "SC_South_slr_data_dist/SC_South_slr_final_dist.gdb"))[[1]])])

## South Carolina beachfront jurisdictional lines (setback)
sc_beachfront_setback <- sf::st_read(dsn = file.path(data_dir, "BaselineSetbackline2018/BaselineSetbackline2018/Setback2018_Final.shp"))

## South Carolina beachfront jurisdictional lines (baseline)
sc_beachfront_baseline <- sf::st_read(dsn = file.path(data_dir, "BaselineSetbackline2018/BaselineSetbackline2018/Baseline2018_Final.shp"))

## North Carolina no-wake zones
nc_no_wake <- sf::st_read(dsn = file.path(data_dir, "no_wake_zones/No-Wake_Zones.shp"))

## North Carolina DWR regional office service areas
nc_dwr_office_areas <- sf::st_read(dsn = file.path(data_dir, "nc_dwr_office_service_areas.gpkg"),
                                   layer = sf::st_layers(dsn = file.path(data_dir, "nc_dwr_office_service_areas.gpkg"))[[1]][1])

## North Carolina DBCJIW rule 2022 (Coastal, join, inland waters)
nc_dbcjiw_rule <- sf::st_read(dsn = file.path(data_dir, "dbcjiw_rule/dbcjiw_rule.shp"))

## North Carolina ITP management areas
nc_itp_management <- sf::st_read(dsn = file.path(data_dir, "itp_management/mgmt_areas.shp"))

## limit of OCSLA 8(g) zone
ocsla_8g_zone <- sf::st_read(dsn = file.path(data_dir, "limit_of_ocsla_8g_zone/limit_of_ocsla_8g_zone.shp"))

## Submerged Land Acts boundary (3nm)
submerged_land_acts_boundary <- sf::st_read(dsn = file.path(data_dir, "submerged_land_acts_boundary_3_nm/submerged_land_acts_boundary_3_nm.shp"))

## South Carolina ESI (1996) managed lands for human uses
sc_esi_1996_managed <- sf::st_read(dsn = file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"),
                                   layer = sf::st_layers(dsn = file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"))[[1]][grep(pattern = "mgt_polygon",
                                                                                                                                                            sf::st_layers(dsn = file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"))[[1]])])

## South Carolina ESI (2015) parks and managed areas
sc_esi_2015_managed_pt <- sf::st_read(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"),
                                      layer = sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]][grep(pattern = "MANAGED_POINT",
                                                                                                                                                          sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]])])

sc_esi_2015_managed_poly <- sf::st_read(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"),
                                        layer = sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]][grep(pattern = "MANAGED_POINT",
                                                                                                                                                            sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]])])

## North Carolina ESI (2016) parks and managed areas
nc_esi_2016_managed_pt <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
                                      layer = sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]][grep(pattern = "MANAGED_POINT",
                                                                                                                                                  sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]])])

nc_esi_2016_managed_poly <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
                                        layer = sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]][grep(pattern = "MANAGED_POLY",
                                                                                                                                                    sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]])])

## North Carolina (2011) management areas
nc_esi_2011_managed <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"),
                                   layer = sf::st_layers(dsn = file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"))[[1]][grep(pattern = "mgt_poly",
                                                                                                                                                            sf::st_layers(dsn = file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"))[[1]])])

## marine ecoregions of the world
meow <- sf::st_read(dsn = file.path(data_dir, "7gger8mmke_MEOW_Final/MEOW/meow_ecos.shp"))

## deepwater marine protected areas
deepwater_mpa <- sf::st_read(dsn = file.path(data_dir, "MPA_update/MPA_update.shp"))

## wind stipulation areas
wind_stipulation <- sf::st_read(dsn = file.path(data_dir, "dod_wind/dod_wind.shp"))

## coastal populated places
coastal_populated_places <- sf::st_read(dsn = file.path(data_dir, "CoastalPopulatedPlace/CoastalPopulatedPlace.gpkg"),
                                        layer = sf::st_layers(dsn = file.path(data_dir, "CoastalPopulatedPlace/CoastalPopulatedPlace.gpkg"))[[1]][1])

## federal and state waters
federal_state_waters <- sf::st_read(dsn = file.path(data_dir, "FederalStateWaters/FederalAndStateWaters.gdb"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "FederalStateWaters/FederalAndStateWaters.gdb"))[[1]][1])

## coastal states
coastal_state <- sf::st_read(dsn = file.path(data_dir, "CoastalState/CoastalState.gpkg"),
                             layer = sf::st_layers(dsn = file.path(data_dir, "CoastalState/CoastalState.gpkg"))[[1]][1])

## coastal counties
coastal_county <- sf::st_read(dsn = file.path(data_dir, "CoastalCounty/CoastalCounty.gpkg"),
                              layer = sf::st_layers(dsn = file.path(data_dir, "CoastalCounty/CoastalCounty.gpkg"))[[1]][1])

## federal statutes
federal_statutes <- sf::st_read(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"),
                                layer = sf::st_layers(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"))[[1]][grep(pattern = "FederalStatute_",
                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"))[[1]])]) %>%
  dplyr::inner_join(x = .,
                    y = sf::st_read(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"))[[1]][grep(pattern = "LookupTable",
                                                                                                                                     x = sf::st_layers(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"))[[1]])]),
                    by = "footprintID") %>%
  dplyr::inner_join(x = .,
                    y = sf::st_read(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"))[[1]][grep(pattern = "StatuteTable",
                                                                                                                                     x = sf::st_layers(dsn = file.path(data_dir, "FederalStatute/FederalStatute.gpkg"))[[1]])]),
                    by = "statuteID")

## state census statistics
state_census <- sf::st_read(dsn = file.path(data_dir, "CensusStatistics/CensusStatistics.gdb"),
                            layer = sf::st_layers(dsn = file.path(data_dir, "CensusStatistics/CensusStatistics.gdb"))[[1]][grep(pattern = "State",
                                                                                                                                x = sf::st_layers(dsn = file.path(data_dir, "CensusStatistics/CensusStatistics.gdb"))[[1]])])

## county census statistics
county_census <- sf::st_read(dsn = file.path(data_dir, "CensusStatistics/CensusStatistics.gdb"),
                             layer = sf::st_layers(dsn = file.path(data_dir, "CensusStatistics/CensusStatistics.gdb"))[[1]][grep(pattern = "County",
                                                                                                                                 x = sf::st_layers(dsn = file.path(data_dir, "CensusStatistics/CensusStatistics.gdb"))[[1]])])

## Coastal Zone Management Act boundary
coastal_zone <- sf::st_read(dsn = file.path(data_dir, "CoastalZoneManagementAct/CoastalZoneManagementAct.gpkg"),
                            layer = sf::st_layers(dsn = file.path(data_dir, "CoastalZoneManagementAct/CoastalZoneManagementAct.gpkg"))[[1]][1])

## National Marine Fisheries Service regions
nmfs_regions <- sf::st_read(dsn = file.path(data_dir, "NationalMarineFisheriesServiceRegions/NationalMarineFisheriesServiceRegions.gdb"),
                            layer = sf::st_layers(dsn = file.path(data_dir, "NationalMarineFisheriesServiceRegions/NationalMarineFisheriesServiceRegions.gdb"))[[1]][1])

## Atlantic administrative boundaries
atlantic_admin <- sf::st_read(dsn = file.path(data_dir, "Atl_admn/atl_admn.shp"))

## BOEM outer continental self areas withdrawn from leasing
boem_ocs_withdrawn_leasing <- sf::st_read(dsn = file.path(data_dir, "boemocsareas-withdrawn-leasing-2021/Withdraw2021.gdb"),
                                          layer = sf::st_layers(dsn = file.path(data_dir, "boemocsareas-withdrawn-leasing-2021/Withdraw2021.gdb"))[[1]][1]) %>%
  sf::st_zm(x = .)

## National Park Service boundaries
nps_boundaries <- sf::st_read(dsn = file.path(data_dir, "nps_boundaries/Administrative_Boundaries_of_National_Park_System_Units.gdb"),
                              layer = sf::st_layers(dsn = file.path(data_dir, "nps_boundaries/Administrative_Boundaries_of_National_Park_System_Units.gdb"))[[1]][1])

## Native American reservation and land trusts
native_american_land <- sf::st_read(dsn = file.path(data_dir, "tl_2018_us_aiannh/tl_2018_us_aiannh.shp"))

## United States census tracts (MARCO coastal zone and vicinity)
us_census <- sf::st_read(dsn = file.path(data_dir, "census_tracts_coastal_counties/census_tracts_coastal_counties_wm.shp"))

## federal consistency geographic location descriptions
federal_consistency <- sf::st_read(dsn = file.path(data_dir, "FederalConsistencyGeographicLocationDescription/FederalConsistencyGeographicLocationDescription.gpkg"),
                                   layer = sf::st_layers(dsn = file.path(data_dir, "FederalConsistencyGeographicLocationDescription/FederalConsistencyGeographicLocationDescription.gpkg"))[[1]][1])

## United States maritime limits boundaries
us_maritime <- sf::st_read(dsn = file.path(data_dir, "USMaritimeLimitsAndBoundariesSHP/USMaritimeLimitsNBoundaries.shp"))

## ShellBase (southeast shellfish water quality database for South Carolina coastal waters -- 1980s - 2020)
sc_shellbase <- sf::st_read(dsn = file.path(data_dir, "ShellBase-SC/SECOORA-ShellBase-SC.shp"))

## ShellBase (southeast shellfish water quality database for North Carolina -- 1980s - 2020)
nc_shellbase <- sf::st_read(dsn = file.path(data_dir, "ShellBase-NC/SECOORA-ShellBase-NC.shp"))

## current magnitude and direction
current_magnitude_direction <- sf::st_read(dsn = file.path(data_dir, "CurrentMagnitudeDirection/CurrentMagnitudeDirection.gpkg"),
                                           layer = sf::st_layers(dsn = file.path(data_dir, "CurrentMagnitudeDirection/CurrentMagnitudeDirection.gpkg"))[[1]][1])

## significant wave height
significant_wave_hgt <- sf::st_read(dsn = file.path(data_dir, "SignificantWaveHeightDirection/SignificantWaveHeightDirection.gpkg"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "SignificantWaveHeightDirection/SignificantWaveHeightDirection.gpkg"))[[1]][1]) %>%
  dplyr::select(significantWaveHeightAnnual)

## wave direction
significant_wave_dir <- sf::st_read(dsn = file.path(data_dir, "SignificantWaveHeightDirection/SignificantWaveHeightDirection.gpkg"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "SignificantWaveHeightDirection/SignificantWaveHeightDirection.gpkg"))[[1]][1]) %>%
  dplyr::select(waveHindcastDirectionAnnual)

## wave energy period
significant_wave_energy <- sf::st_read(dsn = file.path(data_dir, "SignificantWaveHeightDirection/SignificantWaveHeightDirection.gpkg"),
                                       layer = sf::st_layers(dsn = file.path(data_dir, "SignificantWaveHeightDirection/SignificantWaveHeightDirection.gpkg"))[[1]][1]) %>%
  dplyr::select(waveEnergyPeriodAnnual)

## wind
### speed
wind_spd <- sf::st_read(dsn = file.path(data_dir, "WindSpeedDirection/Wind_Speed_Dir_Combined.gdb"),
                        layer = sf::st_layers(dsn = file.path(data_dir, "WindSpeedDirection/Wind_Speed_Dir_Combined.gdb"))[[1]][1]) %>%
  dplyr::select(contains("spd"))

### direction
wind_dir <- sf::st_read(dsn = file.path(data_dir, "WindSpeedDirection/Wind_Speed_Dir_Combined.gdb"),
                        layer = sf::st_layers(dsn = file.path(data_dir, "WindSpeedDirection/Wind_Speed_Dir_Combined.gdb"))[[1]][1]) %>%
  dplyr::select(contains("dir"))

## EMU
### water temperature
emu_water_temp <- sf::st_read(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"),
                              layer = sf::st_layers(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"))[[1]][grep(pattern = "Water",
                                                                                                                                  x = sf::st_layers(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"))[[1]])])
### salinity
emu_water_salinity <- sf::st_read(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"),
                                  layer = sf::st_layers(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"))[[1]][grep(pattern = "Salinity",
                                                                                                                                      x = sf::st_layers(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"))[[1]])])

### dissolved oxygen
emu_water_do <- sf::st_read(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"),
                            layer = sf::st_layers(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"))[[1]][grep(pattern = "Oxygen",
                                                                                                                                x = sf::st_layers(dsn = file.path(data_dir, "EMUWaterQuality/EMU_Water_Quality.gdb"))[[1]])])

## EMU
### nitrates
emu_nutrient_nitrate <- sf::st_read(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"),
                                    layer = sf::st_layers(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"))[[1]][grep(pattern = "Nitrate",
                                                                                                                                    x = sf::st_layers(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"))[[1]])]) %>%
  sf::st_zm()

### phosphates
emu_nutrient_phosphate <- sf::st_read(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"),
                                      layer = sf::st_layers(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"))[[1]][grep(pattern = "Phosphate",
                                                                                                                                      x = sf::st_layers(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"))[[1]])]) %>%
  sf::st_zm()

### silicates
emu_nutrient_silicate <- sf::st_read(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"),
                                     layer = sf::st_layers(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"))[[1]][grep(pattern = "Silicate",
                                                                                                                                     x = sf::st_layers(dsn = file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb"))[[1]])]) %>%
  sf::st_zm()

#### ***warning: these data will not get exported to the geopackage for their are too many fields
## current speed and direction
current_speed_direction <- sf::st_read(dsn = file.path(data_dir, "CurrentSpeedDirection/Current_U_V_M.gdb"),
                                       layer = sf::st_layers(dsn = file.path(data_dir, "CurrentSpeedDirection/Current_U_V_M.gdb"))[[1]][1])

#####################################
#####################################

# create list of the datasets
data <- list(
  sc_geology,
  sc_storms,
  tmdl_sites,
  section319,
  nc_npdes,
  nc_state_stormwater,
  nc_no_exposure,
  nc_wastewater,
  nc_iha,
  nc_ocean_erosion_2020,
  nc_ocean_erosion_2013,
  nc_dcm_ocean,
  nc_esmp,
  nc_estuarine_shoreline_2010,
  nc_estuarine_shoreline_2012,
  southeast_bath,
  sc_esi_2015,
  nc_esi_2016,
  na_tropical_wind,
  sea_surface_height,
  bathymetry_contours,
  offshore_wind_potential_pt,
  offshore_wind_potential_poly,
  enow_state_statistics,
  enow_state_percentages,
  undersea_features,
  benthic_habitat,
  submarine_canyons,
  va_middle_slr_0ft,
  va_middle_slr_1ft,
  va_middle_slr_2ft,
  va_middle_slr_3ft,
  va_middle_slr_4ft,
  va_middle_slr_5ft,
  va_middle_slr_6ft,
  va_middle_slr_8ft,
  va_middle_slr_8ft,
  va_middle_slr_9ft,
  va_middle_slr_10ft,
  va_south_slr_0ft,
  va_south_slr_1ft,
  va_south_slr_2ft,
  va_south_slr_3ft,
  va_south_slr_4ft,
  va_south_slr_5ft,
  va_south_slr_6ft,
  va_south_slr_7ft,
  va_south_slr_8ft,
  va_south_slr_9ft,
  va_south_slr_10ft,
  va_svi,
  ast_incidents,
  ust_incidents,
  hazardous_waste_sites,
  notice_land_use,
  pre_reg_landfill,
  dry_cleaning,
  acidification,
  ccv_compile,
  ccv_employment,
  ccv_fisheries,
  ccv_flooding,
  ccv_high,
  ccv_ranking,
  ccv_ocean_employment,
  ccv_pop_house_rank,
  ccv_pop_house_summary,
  ccv_road_highways,
  ccv_social_summary,
  bottom_trawl_median,
  gill_net_median,
  hyrdaulic_median,
  longline_median,
  scallop_median,
  trap_median,
  subcatchments,
  vpdes_permit_outfalls,
  vwp_general_permits,
  vwp_individual_permits,
  vwp_non_permits,
  wqa_stations,
  water_monitoring_stations,
  cb_hydro_ln,
  cb_hydro_poly,
  cb_natural_hazards,
  cb_political_pt,
  cb_political_ln,
  cb_political_poly,
  cb_management_pt,
  cb_management_poly,
  mab_bottom_shear_stress_median,
  mab_bottom_shear_stress_hipr,
  mab_bottom_shear_stress_95th,
  mab_seabed_mobility_pct,
  mab_seabed_mobility_recurrence,
  sab_bottom_shear_stress_median,
  sab_bottom_shear_stress_hipr,
  sab_bottom_shear_stress_95th,
  sab_seabed_mobility_pct,
  sab_seabed_mobility_recurrence,
  boulder,
  cobble,
  granule_pebble,
  mud,
  sand,
  steep_deep,
  sediment_density,
  sediment_diversity,
  nc_north_slr_0ft,
  nc_north_slr_1ft,
  nc_north_slr_2ft,
  nc_north_slr_3ft,
  nc_north_slr_4ft,
  nc_north_slr_5ft,
  nc_north_slr_6ft,
  nc_north_slr_7ft,
  nc_north_slr_8ft,
  nc_north_slr_9ft,
  nc_north_slr_10ft,
  nc_middle1_slr_0ft,
  nc_middle1_slr_1ft,
  nc_middle1_slr_2ft,
  nc_middle1_slr_3ft,
  nc_middle1_slr_4ft,
  nc_middle1_slr_5ft,
  nc_middle1_slr_6ft,
  nc_middle1_slr_7ft,
  nc_middle1_slr_8ft,
  nc_middle1_slr_9ft,
  nc_middle1_slr_10ft,
  nc_middle2_slr_0ft,
  nc_middle2_slr_1ft,
  nc_middle2_slr_2ft,
  nc_middle2_slr_3ft,
  nc_middle2_slr_4ft,
  nc_middle2_slr_5ft,
  nc_middle2_slr_6ft,
  nc_middle2_slr_7ft,
  nc_middle2_slr_8ft,
  nc_middle2_slr_9ft,
  nc_middle2_slr_10ft,
  nc_south1_slr_0ft,
  nc_south1_slr_1ft,
  nc_south1_slr_2ft,
  nc_south1_slr_3ft,
  nc_south1_slr_4ft,
  nc_south1_slr_5ft,
  nc_south1_slr_6ft,
  nc_south1_slr_7ft,
  nc_south1_slr_8ft,
  nc_south1_slr_9ft,
  nc_south1_slr_10ft,
  nc_south2_slr_0ft,
  nc_south2_slr_1ft,
  nc_south2_slr_2ft,
  nc_south2_slr_3ft,
  nc_south2_slr_4ft,
  nc_south2_slr_5ft,
  nc_south2_slr_6ft,
  nc_south2_slr_7ft,
  nc_south2_slr_8ft,
  nc_south2_slr_9ft,
  nc_south2_slr_10ft,
  sc_north_slr_0ft,
  sc_north_slr_1ft,
  sc_north_slr_2ft,
  sc_north_slr_3ft,
  sc_north_slr_4ft,
  sc_north_slr_5ft,
  sc_north_slr_6ft,
  sc_north_slr_7ft,
  sc_north_slr_8ft,
  sc_north_slr_9ft,
  sc_north_slr_10ft,
  sc_central_slr_0ft,
  sc_central_slr_1ft,
  sc_central_slr_2ft,
  sc_central_slr_3ft,
  sc_central_slr_4ft,
  sc_central_slr_5ft,
  sc_central_slr_6ft,
  sc_central_slr_7ft,
  sc_central_slr_8ft,
  sc_central_slr_9ft,
  sc_central_slr_10ft,
  sc_south_slr_0ft,
  sc_south_slr_1ft,
  sc_south_slr_2ft,
  sc_south_slr_3ft,
  sc_south_slr_4ft,
  sc_south_slr_5ft,
  sc_south_slr_6ft,
  sc_south_slr_7ft,
  sc_south_slr_8ft,
  sc_south_slr_9ft,
  sc_south_slr_10ft,
  sc_beachfront_setback,
  sc_beachfront_baseline,
  nc_no_wake,
  nc_dwr_office_areas,
  nc_dbcjiw_rule,
  nc_itp_management,
  ocsla_8g_zone,
  submerged_land_acts_boundary,
  sc_esi_1996_managed,
  sc_esi_2015_managed_pt,
  sc_esi_2015_managed_poly,
  nc_esi_2016_managed_pt,
  nc_esi_2016_managed_poly,
  nc_esi_2011_managed,
  meow,
  deepwater_mpa,
  wind_stipulation,
  coastal_populated_places,
  federal_state_waters,
  coastal_state,
  coastal_county,
  federal_statutes,
  state_census,
  county_census,
  coastal_zone,
  nmfs_regions,
  atlantic_admin,
  boem_ocs_withdrawn_leasing,
  nps_boundaries,
  native_american_land,
  us_census,
  federal_consistency,
  us_maritime,
  sc_shellbase,
  nc_shellbase,
  current_magnitude_direction,
  significant_wave_hgt,
  significant_wave_dir,
  significant_wave_energy,
  wind_spd,
  wind_dir,
  emu_water_temp,
  emu_water_salinity,
  emu_water_do,
  emu_nutrient_nitrate,
  emu_nutrient_phosphate,
  emu_nutrient_silicate
  # not included in exporting to geopackage as data can only handle 998 columns
  # current_speed_direction
)

#####################################

# create a sequence starting from 1 to the length of the number of the datasets by an increment of 1
data_order <- seq(from = 1,
                  to = length(data),
                  by = 1)

## add extra "0" when needed to make all numbers three digits
for(i in 1:length(data_order)){
  data_order[i] <- ifelse(nchar(data_order[i]) < 2, paste0("00", data_order[i]),
                          ifelse(nchar(data_order[i]) == 2, paste0("0", data_order[i]), data_order[i]))
}

#####################################
#####################################

# create the new codes for the datasets
## create an empty vector for the data codes
data_code <- c()

## loop through the length of datasets and add code index to the region and submodel to have total name
for(i in 1:length(data)){
  data_code[i] <- c(paste(region, submodel, data_order[i], sep = "_"))
}

#####################################
#####################################

# export all datasets with new codes
for(i in seq_along(data)){
  # grab the dataset
  dataset <- data[[i]]
  
  # export the dataset
  sf::st_write(obj = dataset, dsn = metocean_other_geopackage, layer = data_code[i], append = F)
}

for(i in 231:length(data)){
  # grab the dataset
  dataset <- data[[i]]
  
  # export the dataset
  sf::st_write(obj = dataset, dsn = metocean_other_geopackage, layer = data_code[i], append = F)
}

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

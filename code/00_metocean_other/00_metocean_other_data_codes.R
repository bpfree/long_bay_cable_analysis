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
nc_esi_2015 <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
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
offshore_wind_potential <- sf::st_read(dsn = file.path(data_dir, "nrel_wind/NREL_HourlyWind_Atlantic_polysandpoints.gdb"),
                                       layer = sf::st_layers(dsn = file.path(data_dir, "nrel_wind/NREL_HourlyWind_Atlantic_polysandpoints.gdb"))[[1]][1])

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


## North Carolina dry cleaning historical boiler inspections
cleaning_gpkg <- file.path(data_dir, "dry_cleaning.gpkg")

## acidification monitoring locations
acidification <- sf::st_read(dsn = file.path(data_dir, "AcidificationMonitoringMidA_Ver202310/AcidificationMonitoring_MidA_ver202310/AcidificationMonitoringMidA_ver202310.gdb"),
                             layer = sf::st_layers(file.path(data_dir, "AcidificationMonitoringMidA_Ver202310/AcidificationMonitoring_MidA_ver202310/AcidificationMonitoringMidA_ver202310.gdb"))[[1]][1])

## climate change vulnerabilities (compile)
ccv_gdb <- file.path(data_dir, "CCV_CoastalMidAtlantic_Counties/CC_CoastalMidAtlantic_Counties/CCV_CoastalMidAtlantic_Counties.gdb")

## climate change vulnerabilities (employment)

## climate change vulnerabilities (fisheries)

## climate change vulnerabilities (flooding summary)

## climate change vulnerabilities (high)

## climate change vulnerabilities (ranking)

## climate change vulnerabilities (ocean employment)

## climate change vulnerabilities (population and housing ranking)

## climate change vulnerabilities (population and housing summary)

## climate change vulnerabilities (major road and highways)

## climate change vulnerabilities (social vulnerability summary)

## intrinsic seabed vulnerabilities (bottom trawl, median fishing effort)

## intrinsic seabed vulnerabilities (gillnet, median fishing effort)

## intrinsic seabed vulnerabilities (hydraulic clam dredge, median fishing effort)

## intrinsic seabed vulnerabilities (longline, median fishing effort)

## intrinsic seabed vulnerabilities (scallop dredge, median fishing effort)

## intrinsic seabed vulnerabilities (trap, median fishing effort)

## subcatchments

## VPDES permit outfalls

## VWP (general permits)

## VWP (individual permits)

## VWP (non-permits)

## WQA monitoring stations (2018)

## water quality monitoring plan stations (current)

## Chesapeake Bay ESI
### hydro -- line
cb_hydro_l <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                          layer = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "HYDROL",
                                                                                                                                          x = sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])
### hydro -- polygon

### natural hazards

### political

### management

## middle Atlantic bight
### bottom shear stress (median)

### bottom shear stress (half interpercentile)

### bottom shear stress (95th percentile)

### seabed mobility percentage

### seabed mobility recurrence interval

## south Atlantic bight
### bottom shear stress (median)

### bottom shear stress (half interpercentile)

### bottom shear stress (95th percentile)

### seabed mobility percentage

### seabed mobility recurrence interval

## fishing effects model
### boulder

### cobbler

### granule and pebble

### mud

### sand

### steep / deep

### sediment data density

### sediment diversity

## northern North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## middle 1 North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## middle 2 North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## southern 1 North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## southern 2 North Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## northern South Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## central South Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## southern South Carolina (sea level rise)
### 0 foot

### 1 foot

### 2 foot

### 3 foot

### 4 foot

### 5 foot

### 6 foot

### 7 foot

### 8 foot

### 9 foot

### 10 foot

## South Carolina beachfront jurisdictional lines (setback)

## South Carolina beachfront jurisdictional lines (baseline)

## North Carolina no-wake zones

## North Carolina DWR regional office service areas

## North Carolina DBCJIW rule 2022(Coastal, join, inland waters)

## North Carolina ITP management areas

## limit of OCSLA 8(g) zone

## Submerged Land Acts boundary (3nm)

## South Carolina ESI (1996) managed lands for human uses

## South Carolina ESI (2015) parks and managed areas

## North Carolina ESI (2016) parks and managed areas

## North Carolina (2011) management areas

## marine ecoregions of the world

## deepwater marine protected areas

## wind stipulation areas

## coastal populated places
populated_gpkg <- file.path(data_dir, "CoastalPopulatedPlace/CoastalPopulatedPlace.gpkg")

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


## state census statistics
census_gdb <- file.path(data_dir, "CensusStatistics.gdb")

## county census statistics
census_gdb <- file.path(data_dir, "CensusStatistics.gdb")

## Coastal Zone Management Act boundary
management_gpkg <- file.path(data_dir, "CoastalZoneManagementAct/CoastalZoneManagementAct.gpkg")

## National Marine Fisheries Service regions

## Atlantic administrative boundaries

## BOEM outer continental self areas withdrawn from leasing
withdraw_gdb <- file.path(data_dir, "boemoscareas-withdrawn-leasing-2021/Withdraw2021.gdb")

## National Park Service boundaries

## Native American reservation and land trusts

## United States census tracts (MARCO coastal zone and vicinity)


## federal consistency geographic location descriptions
federal_gpkg <- file.path(data_dir, "FederalConsistencyGeographicLocationDescription/FederalConsistencyGeographicLocationDescription.gpkg")

## United States maritime limits boundaries

## ShellBase (southeast shellfish water quality database for South Carolina coastal waters -- 1980s - 2020)

## ShellBase (southeast shellfish water quality database for North Carolina -- 1980s - 2020)

## current speed / direction

## significant wave height

## wave direction

## wave energy period

## wind speed and direction

## current speed and direction
current_speed_direction <- sf::st_read(dsn = file.path(data_dir, "CurrentSpeedDirection/Current_U_V_M.gdb"),
                                       layer = sf::st_layers(dsn = file.path(data_dir, "CurrentSpeedDirection/Current_U_V_M.gdb"))[[1]][1])

## significant wave height

## EMU -- water temperature, salinity, dissolved oxygen
emu_water_gdb <- file.path(data_dir, "EMUWaterQuality/EMU_WaterQuality.gdb")

## EMU -- nitrates, phosphates, silicates
emu_nutrient_gdb <- file.path(data_dir, "EMUNutrient/EMU_Nutrient_Data.gdb")


#####################################
#####################################

# create list of the datasets
data <- list()

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
  sf::st_write(obj = dataset, dsn = national_security_geopackage, layer = data_code[i], append = F)
}

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

###########################################
### 0. Data Codes -- cultural resources ###
###########################################

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
               RSQLite,
               sf,
               sp,
               stringr,
               terra, # is replacing the raster package
               tidyr)

#####################################
#####################################

# set parameters
region <- "vcar"
submodel <- "cr"

#####################################
#####################################

# set directories
## submodel raw data directory
data_dir <- "data/a_raw_data/cultural_resources"

list.files(data_dir)

## cultural resources submodel geopackage
cultural_resources_geopackage <- "data/a_raw_data/cultural_resources/cultural_resources.gpkg"

#####################################
#####################################

# load cultural resources datasets
## North Carolina boating access areas
baa <- sf::st_read(dsn = file.path(data_dir, "BAA/BAA.shp"))

## North Carolina coastal plain paddle trails
paddle_trails <- sf::st_read(dsn = file.path(data_dir, "paddle_trails/paddle_trails.shp"))

## North Carolina fish community assessment locations
nc_fish_community <- sf::st_read(dsn = file.path(data_dir, "DWR_Fish_Community_Assessment_Locations/DWR_Fish_Community_Assessment_Locations.shp"))

## North Carolina clean marinas
nc_marinas <- sf::st_read(dsn = file.path(data_dir, "nc_clean_marinas.gpkg"),
                          layer = sf::st_layers(file.path(data_dir, "nc_clean_marinas.gpkg"))[[1]][1])

## North Carolina beach and waterfront access
nc_beach_waterfront <- sf::st_read(dsn = file.path(data_dir, "nc_beach_waterfront.gpkg"),
                                   layer = sf::st_layers(file.path(data_dir, "nc_beach_waterfront.gpkg"))[[1]][1])

## North Carolina RCCP communities
nc_rccp <- sf::st_read(dsn = file.path(data_dir, "rccp_communities/RCCP_Communities_-_All_Phases.shp"))

## North Carolina potentially underserved block groups (2019)
nc_underserved <- sf::st_read(dsn = file.path(data_dir, "underserved_blocks/NC_DEQ_s_Potentially_Underserved_Block_Groups_2019.shp"))

## North Carolina pumpout locations
nc_pumpouts <- sf::st_read(dsn = file.path(data_dir, "nc_pumpouts.gpkg"),
                                   layer = sf::st_layers(file.path(data_dir, "nc_pumpouts.gpkg"))[[1]][1])

## North Carolina historic preservation office local district boundaries
### local district boundaries
nc_hpo_local <- sf::st_read(dsn = file.path(data_dir, "NCHPO_GISdata/Local_District_Boundaries.shp"))

### national register and study list and determination of eligibility boundaries
nc_hpo_nr_sl_doe <- sf::st_read(dsn = file.path(data_dir, "NCHPO_GISdata/NCHPO_NR_SL_DOE_Boundaries.shp"))
  
### points
nc_hpo_points <- sf::st_read(dsn = file.path(data_dir, "NCHPO_GISdata/NCHPOpoints.shp"))

## North Carolina Environmental Sensitivity Index (2016)
### socio-economic point
nc_esi_2016_socecon_pt <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
                                 layer = sf::st_layers(file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]][grep(pattern = "SOCECON_POINT",
                                                                                                                                       sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]])])

### socio-economic line
nc_esi_2016_socecon_ln <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
                                 layer = sf::st_layers(file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]][grep(pattern = "SOCECON_LINE",
                                                                                                                                       sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]])])

### socio-economic polygon
nc_esi_2016_socecon_poly <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
                                 layer = sf::st_layers(file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]][grep(pattern = "SOCECON_POLY",
                                                                                                                                       sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]])])

### political line
nc_esi_poli_ln <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
                                 layer = sf::st_layers(file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]][grep(pattern = "POLITICAL_LINE",
                                                                                                                                       sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]])])

### political polygon
nc_esi_poli_poly <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"),
                                   layer = sf::st_layers(file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]][grep(pattern = "POLITICAL_POLY",
                                                                                                                                         sf::st_layers(dsn = file.path(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb"))[[1]])])

## North Carolina Environmental Sensitivity Index (2011)
### socio-economic arc
nc_esi_2011_socecon_arc <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"),
                                      layer = sf::st_layers(file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"))[[1]][grep(pattern = "socecon_arc",
                                                                                                                                            sf::st_layers(dsn = file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"))[[1]])])

### socio-economic polygon
nc_esi_2011_socecon_lab <- sf::st_read(dsn = file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"),
                                        layer = sf::st_layers(file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"))[[1]][grep(pattern = "socecon_lab",
                                                                                                                                              sf::st_layers(dsn = file.path(data_dir, "NCarolina_2011_GDB/NCarolina_2011_GDB/NorthCarolinaESI.gdb"))[[1]])])

## South Carolina marinas with pumpout stations
sc_marina_pumpouts <- sf::st_read(dsn = file.path(data_dir, "sc_pumpouts.gpkg"),
                                  layer = sf::st_layers(file.path(data_dir, "sc_pumpouts.gpkg"))[[1]][1])

## South Carolina beaches
beaches <- sf::st_read(dsn = file.path(data_dir, "beaches/BEACHES.shp"))

## South Carolina beach access points
beach_access <- sf::st_read(dsn = file.path(data_dir, "Beach_Access_Points/Beach_Access_Points.shp"))

## South Carolina public water access
water_access <- sf::st_read(dsn = file.path(data_dir, "sc_water_access.gpkg"),
                                  layer = sf::st_layers(file.path(data_dir, "sc_water_access.gpkg"))[[1]][1])

## South Carolina Environmental Sensitivity Index (2015)
### political point
sc_esi_poli_pt <- sf::st_read(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"),
                              layer = sf::st_layers(file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]][grep(pattern = "POLITICAL_POINT",
                                                                                                                                    sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]])])

### political line
sc_esi_poli_poly <- sf::st_read(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"),
                                layer = sf::st_layers(file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]][grep(pattern = "POLITICAL_POLY",
                                                                                                                                      sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]])])

### socio-economic point
sc_esi_2015_socecon_pt <- sf::st_read(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"),
                                      layer = sf::st_layers(file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]][grep(pattern = "SOCECON_POINT",
                                                                                                                                            sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]])])

### socio-economic line
sc_esi_2015_socecon_ln <- sf::st_read(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"),
                                      layer = sf::st_layers(file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]][grep(pattern = "SOCECON_LINE",
                                                                                                                                            sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]])])

### socio-economic polygon
sc_esi_2015_socecon_poly <- sf::st_read(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"),
                                        layer = sf::st_layers(file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]][grep(pattern = "SOCECON_POLY",
                                                                                                                                              sf::st_layers(dsn = file.path(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb"))[[1]])])

## South Carolina Environmental Sensitivity Index (1996)
### socio-economic arc
sc_esi_1996_socecon_arc <- sf::st_read(dsn = file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"),
                                      layer = sf::st_layers(file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"))[[1]][grep(pattern = "socecon_arc",
                                                                                                                                                    sf::st_layers(dsn = file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"))[[1]])])

### socio-economic point
sc_esi_1996_socecon_pt <- sf::st_read(dsn = file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"),
                                      layer = sf::st_layers(file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"))[[1]][grep(pattern = "socecon_point",
                                                                                                                                                    sf::st_layers(dsn = file.path(data_dir, "SCarolina_1996_GDB/SCarolina_1996_GDB/SouthCarolinaESI.gdb"))[[1]])])

## Virginia boating access sites
va_boating <- sf::st_read(dsn = file.path(data_dir, "Boating_Access_Sites/Boating_Access_Sites.shp"))

## Virginia fishing piers
va_fishing <- sf::st_read(dsn = file.path(data_dir, "Fishing_Piers/Fishing_Piers.shp"))

## Virginia birding and wildlife trails
va_birding <- sf::st_read(dsn = file.path(data_dir, "VBWT_Sites_&_Loops/Birding_Wildlife_Trail_Sites.shp"))

## Virginia habitat permit applications (2023)
va_habitat_permits <- sf::st_read(dsn = file.path(data_dir, "va_habitat_permit.kml"))

## long distance sailing races
sailing <- sf::st_read(dsn = file.path(data_dir, "Recreation/Recreation.gdb"),
                       layer = sf::st_layers(file.path(data_dir, "Recreation/Recreation.gdb"))[[1]][grep(pattern = "Sailing",
                                                                                                         sf::st_layers(dsn = file.path(data_dir, "Recreation/Recreation.gdb"))[[1]])])

## shore-based activities
shore_activities <- sf::st_read(dsn = file.path(data_dir, "REG_Shore_PUG_final/REG_Shore_PUG_final.shp"))

## county fiscal stress (2021)
county_fiscal <- sf::st_read(dsn = file.path(data_dir, "County_Fiscal_Stress_2019/County_Fiscal_Stress_2019.shp"))

## Virginia beach bikeways and trails
va_bike <- sf::st_read(dsn = file.path(data_dir, "va_beach_bikeways_trails.gpkg"),
                       layer = sf::st_layers(file.path(data_dir, "va_beach_bikeways_trails.gpkg"))[[1]][1])

## Chesapeake Bay Environmental Sensitivity Index (2016)
### socio-economic point
cb_esi_socecon_pt <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                         layer = sf::st_layers(file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "SOCECON_POINT",
                                                                                                                                   sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

### socio-economic line
cb_esi_socecon_ln <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                                 layer = sf::st_layers(file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "SOCECON_LINE",
                                                                                                                                           sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

### socio-economic line
cb_esi_socecon_poly <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                                    layer = sf::st_layers(file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "SOCECON_POLY",
                                                                                                                                              sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])
### political point
cb_esi_poli_pt <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                                 layer = sf::st_layers(file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "POLITICAL_LINE",
                                                                                                                                           sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

### political line
cb_esi_poli_ln <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                              layer = sf::st_layers(file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "POLITICAL_POLY",
                                                                                                                                        sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

### political polygon
cb_esi_poli_poly <- sf::st_read(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"),
                              layer = sf::st_layers(file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]][grep(pattern = "POLITICAL_POINT",
                                                                                                                                        sf::st_layers(dsn = file.path(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb"))[[1]])])

## commercial whale watching areas
commercial_whale <- sf::st_read(dsn = file.path(data_dir, "Recreation/Recreation.gdb"),
                       layer = sf::st_layers(file.path(data_dir, "Recreation/Recreation.gdb"))[[1]][grep(pattern = "CommercialWhaleWatchingAreas",
                                                                                                         sf::st_layers(dsn = file.path(data_dir, "Recreation/Recreation.gdb"))[[1]])])

## recreation boater routes
boater_routes <- sf::st_read(dsn = file.path(data_dir, "Recreation/Recreation.gdb"),
                                layer = sf::st_layers(file.path(data_dir, "Recreation/Recreation.gdb"))[[1]][grep(pattern = "BoaterRoutes",
                                                                                                                  sf::st_layers(dsn = file.path(data_dir, "Recreation/Recreation.gdb"))[[1]])])

## wildlife and sightseeing activities
wildlife_activities <- sf::st_read(dsn = file.path(data_dir, "REG_Sightseeing_PUG_final/REG_Sightseeing_PUG_final.shp"))

## underwater activities
underwater_activities <- sf::st_read(dsn = file.path(data_dir, "REG_Underwater_PUG_final/REG_Underwater_PUG_final.shp"))

## surface water activities
surfacewater_activities <- sf::st_read(dsn = file.path(data_dir, "REG_Surfacewater_PUG_final/REG_Surfacewater_PUG_final.shp"))

## NPS National Register of Historic Places (points)
nps_nrhp_points <- sf::st_read(dsn = file.path(data_dir, "nps_nrhp_points/NPS_-_National_Register_of_Historic_Places_Locations.shp"))

## NPS National Register of Historic Places (polygons)
nps_nrhp_polygons <- sf::st_read(dsn = file.path(data_dir, "nps_nrhp_polygons/NPS_-_National_Register_of_Historic_Places_Locations.shp"))

## NPS National Register of Historic Places
### building point
nps_register_bldg_pt <- sf::st_read(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"),
                            layer = sf::st_layers(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]][grep(pattern = "bldg_pt",
                                                                                                                              sf::st_layers(dsn = file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]])])

### site point
nps_register_site_pt <- sf::st_read(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"),
                                    layer = sf::st_layers(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]][grep(pattern = "site_pt",
                                                                                                                                      sf::st_layers(dsn = file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]])])

### object point
nps_register_obj_pt <- sf::st_read(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"),
                                    layer = sf::st_layers(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]][grep(pattern = "obj_pt",
                                                                                                                                      sf::st_layers(dsn = file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]])])

### structure point
nps_register_struct_pt <- sf::st_read(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"),
                                    layer = sf::st_layers(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]][grep(pattern = "stru_pt",
                                                                                                                                      sf::st_layers(dsn = file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]])])

### building polygon
nps_register_bldg_poly <- sf::st_read(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"),
                                    layer = sf::st_layers(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]][grep(pattern = "bldg_py",
                                                                                                                                      sf::st_layers(dsn = file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]])])

### site polygon
nps_register_site_poly <- sf::st_read(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"),
                                    layer = sf::st_layers(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]][grep(pattern = "site_py",
                                                                                                                                      sf::st_layers(dsn = file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]])])

### object polygon
nps_register_obj_poly <- sf::st_read(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"),
                                   layer = sf::st_layers(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]][grep(pattern = "obj_py",
                                                                                                                                     sf::st_layers(dsn = file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]])])

### structure polygon
nps_register_struct_poly <- sf::st_read(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"),
                                      layer = sf::st_layers(file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]][grep(pattern = "stru_py",
                                                                                                                                        sf::st_layers(dsn = file.path(data_dir, "nps_historic/NRIS_CR_Standards_Public.gdb"))[[1]])])

## historical lighthouses
lightouse <- sf::st_read(dsn = file.path(data_dir, "HistoricalLighthouse/HistoricLighthouses.gdb"),
                         layer = sf::st_layers(file.path(data_dir, "HistoricalLighthouse/HistoricLighthouses.gdb"))[[1]][1])

## Indian lands
indian_lands <- sf::st_read(dsn = file.path(data_dir, "IndianLands/IndianLands.gdb"),
                         layer = sf::st_layers(file.path(data_dir, "IndianLands/IndianLands.gdb"))[[1]][1])


#####################################
#####################################

# create list of the datasets
data <- list(baa,
             paddle_trails,
             
             nc_fish_community,
             nc_marinas,
             nc_beach_waterfront,
             nc_rccp,
             nc_underserved,
             nc_pumpouts,
             nc_hpo_local,
             nc_hpo_nr_sl_doe,
             nc_hpo_points,
             
             nc_esi_2016_socecon_pt,
             nc_esi_2016_socecon_ln,
             nc_esi_2016_socecon_poly,
             nc_esi_poli_ln,
             nc_esi_poli_poly,
             nc_esi_2011_socecon_arc,
             nc_esi_2011_socecon_lab,
             
             sc_marina_pumpouts,
             beaches,
             beach_access,
             water_access,
             
             sc_esi_poli_pt,
             sc_esi_poli_poly,
             sc_esi_2015_socecon_pt,
             sc_esi_2015_socecon_ln,
             sc_esi_2015_socecon_poly,
             sc_esi_1996_socecon_arc,
             sc_esi_1996_socecon_pt,
             
             va_boating,
             va_fishing,
             va_birding,
             va_habitat_permits,
             sailing,
             shore_activities,
             county_fiscal,
             va_bike,
             
             cb_esi_socecon_pt,
             cb_esi_socecon_ln,
             cb_esi_socecon_poly,
             cb_esi_poli_pt,
             cb_esi_poli_ln,
             cb_esi_poli_poly,
             
             commercial_whale,
             boater_routes,
             wildlife_activities,
             underwater_activities,
             surfacewater_activities,
             
             nps_nrhp_points,
             nps_nrhp_polygons,
             
             nps_register_bldg_pt,
             nps_register_site_pt,
             nps_register_obj_pt,
             nps_register_struct_pt,
             nps_register_bldg_poly,
             nps_register_site_poly,
             nps_register_obj_poly,
             nps_register_struct_poly,
             
             lightouse,
             
             indian_lands)

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
  sf::st_write(obj = dataset, dsn = cultural_resources_geopackage, layer = data_code[i], append = F)
}

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

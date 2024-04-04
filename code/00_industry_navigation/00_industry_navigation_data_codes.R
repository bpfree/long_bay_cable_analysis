################################################
### 0. Data Codes -- industry and navigation ###
################################################

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
submodel <- "in"

#####################################
#####################################

# set directories
## submodel raw data directory
data_dir <- "data/a_raw_data/industry_navigation"

list.files(data_dir)
list.dirs(data_dir, recursive = TRUE)

## industry and navigation submodel geopackage
industry_navigation_geopackage <- "data/a_raw_data/industry_navigation/industry_navigation.gpkg"

#####################################
#####################################

# load industry and navigation datasets
## 2019-2024 OCS Oil and Gas Leasing Draft Proposed Program - All Areas 
oil_gas_lease <- sf::st_read(dsn = paste(data_dir, "atl-5yr-2019-2024/2019-2024DPPAreas_Atlantic.shp", sep = "/"))

## airport
airport_points <- sf::st_read(dsn = paste(data_dir, "airport_points/Airport Points.shp", sep = "/"))

airport_pavements <- sf::st_read(dsn = paste(data_dir, "airport_pavement/airport_pavement.shp", sep = "/"))

federal_navigation <- sf::st_read(dsn = paste(data_dir, "federal_navigation_channel/Federal_Navigation_Channel.shp", sep = "/"))

## ferry data
ferry_terminals <- sf::st_read(dsn = paste(data_dir, "ferry_terminals.gdb", sep = "/"),
                               layer = sf::st_layers(paste(data_dir, "ferry_terminals.gdb", sep = "/"))[[1]][1])
ferry_routes <- sf::st_read(dsn = paste(data_dir, "ferry_routes.gdb", sep = "/"),
                            layer = sf::st_layers(paste(data_dir, "ferry_routes.gdb", sep = "/"))[[1]][1])

navigable_waterway <- sf::st_read(dsn = paste(data_dir, "Navigable_Waterway_Network_Lines.gdb/Navigable_Waterway_Network_Lines.gdb", sep = "/"),
                                  layer = sf::st_layers(paste(data_dir, "Navigable_Waterway_Network_Lines.gdb/Navigable_Waterway_Network_Lines.gdb", sep = "/"))[[1]][1]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.) %>%
  # make all geometries "MULTILINESTRING"
  sf::st_cast("MULTILINESTRING")

## ESI -- South Carolina and North Carolina
sc_esi_nav_marine_poly <- sf::st_read(dsn = paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"),
                                      layer = sf::st_layers(paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"))[[1]][grep(pattern = "NAV_MARINE_POLY",
                                                                                                                                                           sf::st_layers(dsn = paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"))[[1]])])
sc_esi_nav_marine_ln <- sf::st_read(dsn = paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"),
                                    layer = sf::st_layers(paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"))[[1]][grep(pattern = "NAV_MARINE_LINE",
                                                                                                                                                         sf::st_layers(dsn = paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"))[[1]])])
sc_esi_nav_marine_pt <- sf::st_read(dsn = paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"),
                                    layer = sf::st_layers(paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"))[[1]][grep(pattern = "NAV_MARINE_POINT",
                                                                                                                                                         sf::st_layers(dsn = paste(data_dir, "SCarolina_2015_GDB/SCarolina_2015_GDB/SC_ESI_2015.gdb", sep = "/"))[[1]])])
nc_esi_nav_marine_poly <- sf::st_read(dsn = paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"),
                                      layer = sf::st_layers(paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"))[[1]][grep(pattern = "NAV_MARINE_POLY",
                                                                                                                                                   sf::st_layers(dsn = paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"))[[1]])])
nc_esi_nav_marine_pt <- sf::st_read(dsn = paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"),
                                    layer = sf::st_layers(paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"))[[1]][grep(pattern = "NAV_MARINE_POINT",
                                                                                                                                                 sf::st_layers(dsn = paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"))[[1]])])
nc_esi_nav_marine_ln <- sf::st_read(dsn = paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"),
                                    layer = sf::st_layers(paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"))[[1]][grep(pattern = "NAV_MARINE_LINE",
                                                                                                                                                 sf::st_layers(dsn = paste(data_dir, "NCarolina_2016_GDB/NorthCarolina_2016_ESI.gdb", sep = "/"))[[1]])])

anchorage_areas <- sf::st_read(dsn = paste(data_dir, "Anchorage/Anchorage.gpkg", sep = "/"),
                               layer = sf::st_layers(paste(data_dir, "Anchorage/Anchorage.gpkg", sep = "/"))[[1]][[1]])

## pilot
pilot_boarding_areas <- sf::st_read(dsn = paste(data_dir, "PilotBoarding/PilotBoarding.gdb", sep = "/"),
                                    layer = sf::st_layers(paste(data_dir, "PilotBoarding/PilotBoarding.gdb", sep = "/"))[[1]][grep(pattern = "Areas",
                                                                                                                                   sf::st_layers(dsn = paste(data_dir, "PilotBoarding/PilotBoarding.gdb", sep = "/"))[[1]])])
pilot_boarding_stations <- sf::st_read(dsn = paste(data_dir, "PilotBoarding/PilotBoarding.gdb", sep = "/"),
                                       layer = sf::st_layers(paste(data_dir, "PilotBoarding/PilotBoarding.gdb", sep = "/"))[[1]][grep(pattern = "Stations",
                                                                                                                                      sf::st_layers(dsn = paste(data_dir, "PilotBoarding/PilotBoarding.gdb", sep = "/"))[[1]])])


principal_ports <- sf::st_read(dsn = paste(data_dir, "PrincipalPort/PrincipalPort.gpkg", sep = "/"),
                               layer = sf::st_layers(paste(data_dir, "PrincipalPort/PrincipalPort.gpkg", sep = "/"))[[1]][1])

navigation_channels <- sf::st_read(dsn = paste(data_dir, "NavigationChannel/NavigationChannel.gpkg", sep = "/"),
                                   layer = sf::st_layers(paste(data_dir, "NavigationChannel/NavigationChannel.gpkg", sep = "/"))[[1]][1])

wrecks_observations <- sf::st_read(dsn = paste(data_dir, "WreckObstruction/WreckObstruction.gpkg", sep = "/"),
                                   layer = sf::st_layers(paste(data_dir, "WreckObstruction/WreckObstruction.gpkg", sep = "/"))[[1]][1])

## pipelines
pipelines <- sf::st_read(dsn = paste(data_dir, "Pipeline/Pipelines.gdb", sep = "/"),
                         layer = sf::st_layers(paste(data_dir, "Pipeline/Pipelines.gdb", sep = "/"))[[1]][1])
pipeline_areas <- sf::st_read(dsn = paste(data_dir, "PipelineArea/PipelineArea.gpkg", sep = "/"),
                              layer = sf::st_layers(paste(data_dir, "PipelineArea/PipelineArea.gpkg", sep = "/"))[[1]][1])

## submarine cables
submarine_cables <- sf::st_read(dsn = paste(data_dir, "SubmarineCable/NOAAChartedSubmarineCables.gdb", sep = "/"),
                                layer = sf::st_layers(paste(data_dir, "SubmarineCable/NOAAChartedSubmarineCables.gdb", sep = "/"))[[1]][1])

submarine_cable_areas <- sf::st_read(dsn = paste(data_dir, "SubmarineCableArea/SubmarineCableArea.gpkg", sep = "/"),
                                     layer = sf::st_layers(paste(data_dir, "SubmarineCableArea/SubmarineCableArea.gpkg", sep = "/"))[[1]][1])

## wastewater
wastewater_outfalls_facility <- sf::st_read(dsn = paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"),
                                            layer = sf::st_layers(paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"))[[1]][grep(pattern = "Facility",
                                                                                                                                                    sf::st_layers(dsn = paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"))[[1]])])
wastewater_outfalls <- sf::st_read(dsn = paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"),
                                   layer = sf::st_layers(paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"))[[1]][grep(pattern = "Outfall",
                                                                                                                                           sf::st_layers(dsn = paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"))[[1]])])
wastewater_outfalls_pipe <- sf::st_read(dsn = paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"),
                                        layer = sf::st_layers(paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"))[[1]][grep(pattern = "Pipe",
                                                                                                                                                sf::st_layers(dsn = paste(data_dir, "WastewaterOutfall/WastewaterOutfall.gpkg", sep = "/"))[[1]])])

lightering_zones <- sf::st_read(dsn = paste(data_dir, "LighteringZone/LighteringZone.gpkg", sep = "/"),
                                layer = sf::st_layers(paste(data_dir, "LighteringZone/LighteringZone.gpkg", sep = "/"))[[1]][1])

aids_navigation <- sf::st_read(dsn = paste(data_dir, "AtoN/AtoN.gpkg", sep = "/"),
                               layer = sf::st_layers(paste(data_dir, "AtoN/AtoN.gpkg", sep = "/"))[[1]][1])

# wind areas
planning_areas <- sf::st_read(dsn = paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"),
                              layer = sf::st_layers(paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"))[[1]][grep(pattern = "Area_Outlines",
                                                                                                                                                            sf::st_layers(dsn = paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"))[[1]])])
planning_blocks <- sf::st_read(dsn = paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"),
                               layer = sf::st_layers(paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"))[[1]][grep(pattern = "Planning_Areas",
                                                                                                                                                             sf::st_layers(dsn = paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"))[[1]])])
lease_areas <- sf::st_read(dsn = paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"),
                           layer = sf::st_layers(paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"))[[1]][grep(pattern = "Lease_Outlines",
                                                                                                                                                         sf::st_layers(dsn = paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"))[[1]])])
lease_blocks <- sf::st_read(dsn = paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"),
                            layer = sf::st_layers(paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"))[[1]][grep(pattern = "Wind_Leases",
                                                                                                                                                          sf::st_layers(dsn = paste(data_dir, "BOEM-Renewable-Energy-Geodatabase/BOEMWindLayers_4Download.gdb", sep = "/"))[[1]])])

oil_gas_potential <- sf::st_read(dsn = paste(data_dir, "OffshoreOilGasResourcePotential.gpkg", sep = "/"),
                                 layer = sf::st_layers(paste(data_dir, "OffshoreOilGasResourcePotential.gpkg", sep = "/"))[[1]][1])

energy_facilities <- sf::st_read(dsn = paste(data_dir, "CoastalEnergyFacility/CoastalEnergyFacilities.gdb", sep = "/"),
                                 layer = sf::st_layers(paste(data_dir, "CoastalEnergyFacility/CoastalEnergyFacilities.gdb", sep = "/"))[[1]][1])

power_substations <- sf::st_read(dsn = paste(data_dir, "ElectricPowerSubstation/Substations.gdb", sep = "/"),
                                 layer = sf::st_layers(paste(data_dir, "ElectricPowerSubstation/Substations.gdb", sep = "/"))[[1]][1])

sand_blocks <- sf::st_read(dsn = paste(data_dir, "ATLSandAliquots_shp/ATLSandAliquots_20240227.shp", sep = "/"))

beach_nourishment <- sf::st_read(dsn = paste(data_dir, "BeachNourishment/BeachNourishment.gdb", sep = "/"),
                                 layer = sf::st_layers(paste(data_dir, "BeachNourishment/BeachNourishment.gdb", sep = "/"))[[1]][1])

ocean_disposal <- sf::st_read(dsn = paste(data_dir, "OceanDisposalSite/OceanDisposalSite.gpkg", sep = "/"),
                              layer = sf::st_layers(paste(data_dir, "OceanDisposalSite/OceanDisposalSite.gpkg", sep = "/"))[[1]][1])

electric_transmission <- sf::st_read(dsn = paste(data_dir, "US_Electric_Power_Transmission_Lines.gpkg", sep = "/"),
                                     layer = sf::st_layers(paste(data_dir, "US_Electric_Power_Transmission_Lines.gpkg", sep = "/"))[[1]][1])

freight_facilities <- sf::st_read(dsn = paste(data_dir, "intermodal_freight_facilities.gdb", sep = "/"),
                                  layer = sf::st_layers(paste(data_dir, "intermodal_freight_facilities.gdb", sep = "/"))[[1]][1])

marine_highways <- sf::st_read(dsn = paste(data_dir, "marine_highways.gdb", sep = "/"),
                               layer = sf::st_layers(paste(data_dir, "marine_highways.gdb", sep = "/"))[[1]][1]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

shipping_lanes <- sf::st_read(dsn = paste(data_dir, "shippinglanes/shippinglanes.shp", sep = "/"))

wind_interarray <- sf::st_read(dsn = paste(data_dir, "offshore_wind_interarray_cables.gpkg", sep = "/"),
                               layer = sf::st_layers(paste(data_dir, "offshore_wind_interarray_cables.gpkg", sep = "/"))[[1]][1])

channel_markers <- sf::st_read(dsn = paste(data_dir, "Channel_Markers_and_Navigation_Aids/Channel_Markers_and_Navigation_Aids.shp", sep = "/"))

exploration_exclusion <- sf::st_read(dsn = paste(data_dir, "Exploration_ExclusionZones.kml", sep = "/")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

export_cable <- sf::st_read(dsn = paste(data_dir, "offshore_wind_export_cable.gpkg", sep = "/"),
                            layer = sf::st_layers(paste(data_dir, "offshore_wind_export_cable.gpkg", sep = "/"))[[1]][1])

## passive acoustic
passive_acoustic_nc_line <- sf::st_read(dsn = paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"),
                                        layer = sf::st_layers(paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"))[[1]][13]) %>%
  sf::st_transform(x =.,
                   crs = "EPSG:4326")
passive_acoustic_nc_point <- sf::st_read(dsn = paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"),
                                         layer = sf::st_layers(paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"))[[1]][14]) %>%
  sf::st_transform(x =.,
                   crs = "EPSG:4326")

passive_acoustic_sc_line <- sf::st_read(dsn = paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"),
                                        layer = sf::st_layers(paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"))[[1]][15]) %>%
  sf::st_transform(x =.,
                   crs = "EPSG:4326")
passive_acoustic_sc_point <- sf::st_read(dsn = paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"),
                                         layer = sf::st_layers(paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"))[[1]][16]) %>%
  sf::st_transform(x =.,
                   crs = "EPSG:4326")

passive_acoustic_va_line <- sf::st_read(dsn = paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"),
                                        layer = sf::st_layers(paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"))[[1]][11]) %>%
  sf::st_transform(x =.,
                   crs = "EPSG:4326")
passive_acoustic_va_point <- sf::st_read(dsn = paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"),
                                         layer = sf::st_layers(paste(data_dir, "ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork/ProposedPassiveAcousticNetwork.gdb", sep = "/"))[[1]][12]) %>%
  sf::st_transform(x =.,
                   crs = "EPSG:4326")

ais_tracks <- sf::st_read(dsn = file.path(data_dir, "ais_tracks_2022.gdb"),
                          layer = sf::st_layers(file.path(data_dir, "ais_tracks_2022.gdb"))[[1]][1])

right_whale <- sf::st_read(dsn = paste(data_dir, "Proposed-Right-Whale-Seasonal-Speed-Zones/Proposed_Right_Whale_Seasonal_Speed_Zones.shp", sep = "/"))

storage_tank <- sf::st_read(dsn = paste(data_dir, "ust_active_facilities.gpkg", sep = "/"),
                            layer = sf::st_layers(paste(data_dir, "ust_active_facilities.gpkg", sep = "/"))[[1]][1])

planned_ports <- sf::st_read(dsn = paste(data_dir, "OffshoreWindPlannedPorts2023_MidAtl/OffshoreWindPlannedPorts2023_MidAtl/OffshoreWindPlannedPorts2023.gdb", sep = "/"),
                             layer = sf::st_layers(paste(data_dir, "OffshoreWindPlannedPorts2023_MidAtl/OffshoreWindPlannedPorts2023_MidAtl/OffshoreWindPlannedPorts2023.gdb", sep = "/"))[[1]][1])

## petroleum
petroleum_tank <- sf::st_read(dsn = paste(data_dir, "Registered_Petroleum_Tank_Facilities/Registered_Petroleum_Tank_Facilities.shp", sep = "/"))
petroleum_release <- sf::st_read(dsn = paste(data_dir, "Petroleum_Release_sites/Petroleum_Release_Sites.shp", sep = "/"))

## Chesapeake Bay ESI
cb_esi_ln <- sf::st_read(dsn = paste(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb", sep = "/"),
                         layer = sf::st_layers(paste(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb", sep = "/"))[[1]][8])

cb_esi_poly <- sf::st_read(dsn = paste(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb", sep = "/"),
                           layer = sf::st_layers(paste(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb", sep = "/"))[[1]][14])

cb_esi_pt <- sf::st_read(dsn = paste(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb", sep = "/"),
                         layer = sf::st_layers(paste(data_dir, "ChesapeakeBay_2016_GDB/ChesapeakeBay_2016_ESI.gdb", sep = "/"))[[1]][20])

## NDBC sensors / buoys
ndbc_intl <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                         layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "International",
                                                                                                                sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

ndbc_ioos <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                         layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "IOOS",
                                                                                                                sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

ndbc_metar <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                          layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "METAR",
                                                                                                                 sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

ndbc_ocean <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                          layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "Ocean",
                                                                                                                 sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

ndbc_nerrs <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                          layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "NERRS",
                                                                                                                 sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

ndbc_nos <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                        layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "NOS",
                                                                                                               sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

ndbc_ships <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                          layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "Ships",
                                                                                                                 sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

ndbc_tao <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                        layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "TAO",
                                                                                                               sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

ndbc_tsunami <- sf::st_read(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"),
                            layer = sf::st_layers(paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]][grep(pattern = "Tsunami",
                                                                                                                   sf::st_layers(dsn = paste(data_dir, "stations_by_program.kml", sep = "/"))[[1]])]) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

mebd <- read.csv(file = paste(data_dir, "mebd.csv", sep = "/")) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to WGS84
               ## according to Joseph Anderson (joseph.p.anderson@noaa.gov) -- the data are put into WGS84
               crs = 4326) # EPSG 4326 (https://epsg.io/4326)

sand_gravel <- sf::st_read(dsn = paste(data_dir, "LeaseAreas_shp/LeaseAreas_20240209.shp", sep = "/"),
                           layer = sf::st_layers(paste(data_dir, "LeaseAreas_shp/LeaseAreas_20240209.shp", sep = "/"))[[1]][1])

raw_incident  <- read.csv(file = paste(data_dir, "incidents.csv", sep = "/")) %>%
  # convert to simple feature
  sf::st_as_sf(coords = c("lon", "lat"),
               # set the coordinate reference system to WGS84
               ## according to Michael Greer (michael.greer@noaa.gov) -- the data are put into WGS84
               crs = 4326) # EPSG 4326 (https://epsg.io/4326)

#####################################
#####################################

# create list of the datasets
data <- list(oil_gas_lease,
             
             airport_points,
             airport_pavements,
             federal_navigation,
             
             ferry_terminals,
             ferry_routes,
             
             navigable_waterway,
             
             sc_esi_nav_marine_poly,
             sc_esi_nav_marine_ln,
             sc_esi_nav_marine_pt,
             
             nc_esi_nav_marine_poly,
             nc_esi_nav_marine_pt,
             nc_esi_nav_marine_ln,
             
             anchorage_areas,
             
             pilot_boarding_areas,
             pilot_boarding_stations,
             
             principal_ports,
             
             navigation_channels,
             
             wrecks_observations,
             
             pipelines,
             pipeline_areas,
             
             submarine_cables,
             submarine_cable_areas,
             
             wastewater_outfalls_facility,
             wastewater_outfalls,
             wastewater_outfalls_pipe,
             
             lightering_zones,
             
             aids_navigation,
             
             planning_areas,
             planning_blocks,
             lease_areas,
             lease_blocks,
             
             oil_gas_potential,
             
             energy_facilities,
             
             power_substations,
             
             sand_blocks,
             
             beach_nourishment,
             
             ocean_disposal,
             
             electric_transmission,
             
             freight_facilities,
             
             marine_highways,
             
             shipping_lanes,
             
             wind_interarray,
             
             channel_markers,
             
             exploration_exclusion,
             
             export_cable,
             
             passive_acoustic_nc_line,
             passive_acoustic_nc_point,
             passive_acoustic_sc_line,
             passive_acoustic_sc_point,
             passive_acoustic_va_line,
             passive_acoustic_va_point,
             
             ais_tracks,
             
             right_whale,
             
             storage_tank,
             
             planned_ports,
             
             petroleum_tank,
             
             petroleum_release,
             
             cb_esi_ln,
             cb_esi_poly,
             cb_esi_pt,
             
             ndbc_intl,
             ndbc_ioos,
             ndbc_metar,
             ndbc_ocean,
             ndbc_nerrs,
             ndbc_nos,
             ndbc_ships,
             ndbc_tao,
             ndbc_tsunami,
             
             mebd,
             
             sand_gravel,
             raw_incident)

#####################################

# create a sequence starting from 1 to the length of the number of the datasets by an increment of 1
data_order <- seq(from = 1,
                  to = length(data),
                  by = 1)

## add extra "0" when needed to make all numbers three digits
for(i in 1:length(data_order)){
  data_order[i] <- ifelse(nchar(data_order[i]) < 2, paste0("00",data_order[i]),
                          ifelse(nchar(data_order[i]) == 2, paste0("0",data_order[i]), data_order[i]))
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
  sf::st_write(obj = dataset, dsn = industry_navigation_geopackage, layer = data_code[i], append = F)
}

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

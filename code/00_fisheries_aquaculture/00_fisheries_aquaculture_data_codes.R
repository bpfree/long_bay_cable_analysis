##################################################
### 0. Data Codes -- fisheries and aquaculture ###
##################################################

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
submodel <- "fa"

#####################################
#####################################

# set directories
## submodel raw data directory
data_dir <- "data/a_raw_data/fisheries_aquaculture"

list.files(data_dir)
list.dirs(data_dir, recursive = TRUE)

## fisheries and aquaculture submodel geopackage
fisheries_aquaculture_geopackage <- "data/a_raw_data/fisheries_aquaculture/fisheries_aquaculture.gpkg"

#####################################
#####################################

# load fisheries and aquaculture datasets
## South Carolina trawl area boundaries
sc_trawl <- sf::st_read(dsn = file.path(data_dir, "SC_Official_Trawl_Area_Boundaries/SC_Official_Trawl_Area_Boundaries.shp"))

## North Carolina public fishing areas
nc_pfa <- sf::st_read(dsn = file.path(data_dir, "pfa/PFA.shp"))

## Virginia clamming grounds
clams <- sf::st_read(dsn = file.path(data_dir, "Clams.kml")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

## Virginia Baylor grounds
baylor <- sf::st_read(dsn = file.path(data_dir, "Baylor.kml")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

## North Carolina crab trawl line
nc_crab <- sf::st_read(dsn = file.path(data_dir, "crab_trawl_line/Crab_trawl_line.shp"))

## rock shrimp VMS
shrimp_vms <- sf::st_read(dsn = file.path(data_dir, "shrimp_vms/shrimp_vms.shp"))

## South Carolina shrimp landings
sc_shrimp_landings <- sf::st_read(dsn = file.path(data_dir, "sc_shrimp/shrimp_landings_south_carolina.shp"))

## aquaculture
aquaculture <- sf::st_read(dsn = file.path(data_dir, "aquaculture/aquaculture.gpkg"),
                           layer = sf::st_layers(file.path(data_dir, "aquaculture/aquaculture.gpkg"))[[1]][1])

## commercial fish landing summary
commercial_fish <- sf::st_read(dsn = file.path(data_dir, "CommercialFishLandingSummary/CommercialFishLandings.gdb"),
                           layer = sf::st_layers(file.path(data_dir, "CommercialFishLandingSummary/CommercialFishLandings.gdb"))[[1]][1])

## ACCSP fish landings (area code)
fish_landings_area_code <- sf::st_read(dsn = file.path(data_dir, "fish_landings/fish_landings.shp"))

## ACCSP fish landings (latitude and species)
fish_landings_species <- sf::st_read(dsn = file.path(data_dir, "fish_landings_lat_spp/accsp_fish_landings_by_latitude_and_species.shp"))

## North Carolina shrimp landings
nc_shrimp <- sf::st_read(dsn = file.path(data_dir, "nc_shrimp/north_carolina_shrimp_landings.shp"))

## fishing nets
### fyke nets
fyke_nets_points <- sf::st_read(dsn = file.path(data_dir, "fn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "POINT"))

fyke_nets_lines <- sf::st_read(dsn = file.path(data_dir, "fn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "LINESTRING")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

### pound nets
pound_nets_points <- sf::st_read(dsn = file.path(data_dir, "pn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "POINT"))

pound_nets_lines <- sf::st_read(dsn = file.path(data_dir, "pn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "LINESTRING")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

### staked gill nets
staked_gill_nets_points <- sf::st_read(dsn = file.path(data_dir, "sgn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "POINT"))

staked_gill_nets_lines <- sf::st_read(dsn = file.path(data_dir, "sgn.kml")) %>%
  dplyr::filter(sf::st_is(x = ., type = "LINESTRING")) %>%
  # drop dimensions to make geometry valid
  sf::st_zm(.)

## VMS demarcation line
vms_demarcation <- sf::st_read(dsn = file.path(data_dir, "vms_demarcation_line_20140925?null/VMS_Demarcation_Line/VMS_Demarcation_Line.shp"))

## clam amendment
clam_amendment <- sf::st_read(dsn = file.path(data_dir, "ClamAmendmentAreas/ClamAmendmentAreas.shp"))

## Atlantic Large Whale Take Reduction Plan Regulated and Exempted Waters

## coral amendment
coral_amendment <- sf::st_read(dsn = file.path(data_dir, "CoralAmendmentAreas/CoralAmendmentAreas.shp"))

## fishery management area
fishery_management_area <- sf::st_read(dsn = file.path(data_dir, "FisheryManagementAreasSample/FisheryManagementAreasSample.shp"))

## North Carolina shellfish growing areas current classifications
nc_sga_classifications <- sf::st_read(dsn = file.path(data_dir, "SGA_Current_Classifications/SGA_Current_Classifications.shp"))

## South Carolina shellfish harvest classifications
sc_shellfish_harvest_classifications <- sf::st_read(dsn = file.path(data_dir, "Shellfish_Harvest_Classifications/Shellfish_Harvest_Classifications.shp"))

## North Carolina cultch
nc_cultch_1981_2002 <- sf::st_read(dsn = file.path(data_dir, "nc_cultch_1981_2002/CultchApp2022.shp"))
nc_cultch_2003_present <- sf::st_read(dsn = file.path(data_dir, "nc_cultch_2003_present/CultchApp2022.shp"))

## Virginia open harvest
open_harvest <- sf::st_read(dsn = file.path(data_dir, "OpenHarvest.kml")) %>%
  sf::st_make_valid() %>%
  sf::st_zm()

## Virginia oyster gardening
oyster_garden <- sf::st_read(dsn = file.path(data_dir, "OyGarden.kml"))

## Virginia oyster ground applications
lease_applications <- sf::st_read(dsn = file.path(data_dir, "LeaseApplications.kml")) %>%
  sf::st_make_valid() %>%
  sf::st_zm()

## Virginia oyster private oyster ground leases
private_leases <- sf::st_read(dsn = file.path(data_dir, "PrivateLeases.kml")) %>%
  sf::st_zm()

## NC Designated Pot Area (DPA)
dpa <- sf::st_read(dsn = file.path(data_dir, "dpa/dpa.shp"))

## NC Attended Gillnet Areas
agna_combine <- sf::st_read(dsn = file.path(data_dir, "agna_combine/agna_combine.shp"))

## NC Mechanical Clam Harvest Areas 2016 (proposed)
mcha <- sf::st_read(dsn = file.path(data_dir, "pmcha/proposed_mechanical_clam_harvest_area.shp"))

## South Carolina shellfish permit data
sc_shellfish_permit <- sf::st_read(dsn = file.path(data_dir, "MariculturePermitData_Public_View/MariculturePermitData_Public_View.shp"))

## South Carolina monitoring stations (shellfish)
sc_monitoring_stations <- sf::st_read(dsn = file.path(data_dir, "monitoring_stations_shellfish/Monitoring_Stations_-_Shellfish.shp"))

## lobster gear
lobster_gear <- sf::st_read(dsn = file.path(data_dir, "lobster_gear_areas_20160501/Lobster_Gear_Areas/Lobster_Gear_Areas.shp"))

## illex fishery mesh exemption
illex_fishery <- sf::st_read(dsn = file.path(data_dir, "illex_fishery_mesh_exemption_area_20140501/Illex_Fishery_Mesh_Exemption_Area/Illex_Fishery_Mesh_Exemption_Area.shp"))

## Atlantic red drum fishery harvest or possession prohibition area
altantic_red_drum <- sf::st_read(dsn = file.path(data_dir, "atlantic-red-drum-fishery-harvest-or-possession-prohibition-area-20140915-noaa-garfo/Atlantic_Red_Drum_Fishery_Harvest_or_Possession_Prohibition_Area/Atlantic_Red_Drum_Fishery_Harvest_or_Possession_Prohibition_Area.shp"))
sne_dogfish <- sf::st_read(dsn = file.path(data_dir, "sne-dogfish-gillnet-exemption-area-20150315-noaa-garfo/SNE_Dogfish_Gillnet_Exemption_Area/SNE_Dogfish_Gillnet_Exemption_Area.shp"))
sne_regulated <- sf::st_read(dsn = file.path(data_dir, "sne-regulated-mesh-area-20150315-noaa-garfo/SNE_Regulated_Mesh_Area/SNE_Regulated_Mesh_Area.shp"))
sne_exemption_area <- sf::st_read(dsn = file.path(data_dir, "sne_exemption_area_20150315/SNE_Exemption_Area/SNE_Exemption_Area.shp"))
scup_sea <- sf::st_read(dsn = file.path(data_dir, "scup-transfer-at-sea-20140501-noaa-garfo/Scup_Transfer-at-Sea/Scup_Transfer-at-Sea.shp"))
sne_monkfish_skate_trawl <- sf::st_read(dsn = file.path(data_dir, "sne_monkfish_and_skate_trawl_exemption_area_20150315/SNE_Monkfish_and_Skate_Trawl_Exemption_Area/SNE_Monkfish_and_Skate_Trawl_Exemption_Area.shp"))
sne_monkdish_skate_gillnet <- sf::st_read(dsn = file.path(data_dir, "sne_monkfish_and_skate_gillnet_exemption_area_20150315/SNE_Monkfish_and_Skate_Gillnet_Exemption_Area/SNE_Monkfish_and_Skate_Gillnet_Exemption_Area.shp"))
atl_sea_scallop <- sf::st_read(dsn = file.path(data_dir, "Scallop-Rotational-Areas-20230419/Scallop_Rotational_Areas_20230419/Scallop_Rotational_Areas_20230411.shp"))
va_pound_net <- sf::st_read(dsn = file.path(data_dir, "Virginia_Pound_Net_Regulated_Areas_2022523/Virginia_Pound_Net_Regulated_Areas_2022523/Virginia_Pound_Net_Regulated_Areas.shp"))
cape_lookout_hapc_fishery_fma <- sf::st_read(dsn = file.path(data_dir, "shapefile-cape-lookout-orig-ext/CapeLookout_po.shp"))
cape_lookout_hapc_ext_fishery_fma <- sf::st_read(dsn = file.path(data_dir, "shapefile-cape-lookout-orig-ext/Cape_Lookout_ext_po.shp"))
cape_fear_hapc_fma <- sf::st_read(dsn = file.path(data_dir, "capefear/CapeFear_po.shp"))
stetson_miami_hapc_fma <- sf::st_read(dsn = file.path(data_dir, "stetson_miami/StetsonMiami_po.shp"))
blake_ridge <- sf::st_read(dsn = file.path(data_dir, "blakeridge/BlakeRidge_po.shp"))
mpas <- sf::st_read(dsn = file.path(data_dir, "mpas/MPAs_po.shp"))
sa_atlantic_fma <- sf::st_read(dsn = file.path(data_dir, "sa_eez_off_states/SA_EEZ_off_states.shp"))
comm_vessel_snapper_grouper <- sf::st_read(dsn = file.path(data_dir, "comm_permits_sa_sg/comm_permits_SA_SG.shp"))
sea_bass_fma <- sf::st_read(dsn = file.path(data_dir, "seabass_potid/SeaBass_potid.shp"))
smz_fma <- sf::st_read(dsn = file.path(data_dir, "smz/SMZ_po.shp"))
longline_pa_fma <- sf::st_read(dsn = file.path(data_dir, "ll_prohibareas_n_s/LL_prohibareas_N_S_po.shp"))
spawing_smz <- sf::st_read(dsn = file.path(data_dir, "spawning_smzs/Spawning_SMZs_po.shp"))

bsb_pot_nov_apr_fma <- sf::st_read(dsn = file.path(data_dir, "bsb_pot_nov_apr/BSB_Pot_Nov_Apr_po.shp"))
bsb_pot_dec_mar_fma <- sf::st_read(dsn = file.path(data_dir, "bsb_pot_dec_mar/BSB_Pot_Dec_Mar_po.shp")) %>%
  sf::st_make_valid()
sa_shrimp_cold_fma <- sf::st_read(dsn = file.path(data_dir, "sa_shrimp_cold_weather/SA_shrimp_cold_weather_po.shp"))
octocoral_closed_fma <- sf::st_read(dsn = file.path(data_dir, "octocoral/octocoral.shp"))
golden_crab_fma <- sf::st_read(dsn = file.path(data_dir, "goldencrab/GoldenCrab_po.shp"))
charleston_bump_fma <- sf::st_read(dsn = file.path(data_dir, "pelagicll_charleston/PelagicLL_Charleston.shp"))
pelagic_sargassum_fma <- sf::st_read(dsn = file.path(data_dir, "pelagic_sargassum/pelagic_sargassum.shp"))
king_mackerel_fma <- sf::st_read(dsn = file.path(data_dir, "king_mackerel/king_mackerel_po.shp"))
spanish_mackerel_fma <- sf::st_read(dsn = file.path(data_dir, "spanish_mackerel_/Spanish_mackerel_po.shp"))
cobia_fma <- sf::st_read(dsn = file.path(data_dir, "cobia/cobia_new_po.shp"))

## NC Designated Seed Oyster Management Area
dsoma <- sf::st_read(dsn = file.path(data_dir, "dsoma/designated_seed_oyster_management_area.shp"))

## NC River Herring Management Area
rhma <- sf::st_read(dsn = file.path(data_dir, "rhma/river_herring_management_area.shp"))

## NC Striped Bass Management Areas
sbma <- sf::st_read(dsn = file.path(data_dir, "sbma/striped_bass_management_area.shp"))

## South Carolina shellfish management areas
sc_shellfish_management_area <- sf::st_read(dsn = file.path(data_dir, "Shellfish_Management_Areas/Shellfish_Management_Areas.shp"))

## NC CRAB HARVEST MANAGEMENT AREAS
chma_line <- sf::st_read(dsn = file.path(data_dir, "chma_line/chma_line.shp"))

monkfish_fma <- sf::st_read(dsn = file.path(data_dir, "MonkfishFisheryManagementAreasShapefile/Monkfish closed areas/Monkfish_Fishery_Management_Areas.shp"))
skate_management_unit <- sf::st_read(dsn = file.path(data_dir, "skate-management-unit-20140501-noaa-garfo/Skate_Management_Unit/Skate_Management_Unit.shp"))
sf_scup_bsb_management_units <- sf::st_read(dsn = file.path(data_dir, "management-units-sf-scup-bsb-20140501-noaa-garfo/Management_Units_SF-Scup-BSB/Management_Units_SF-Scup-BSB.shp"))

## NC Oyster Sanctuaries
nc_oyster_sanctuaries <- sf::st_read(dsn = file.path(data_dir, "nc_oyster_sanctuaries.gpkg"),
                                     layer = sf::st_layers(dsn = file.path(data_dir, "nc_oyster_sanctuaries.gpkg"))[[1]][1])

## Virginia shellfish condemnation zones
va_shellfish_condemnation <- sf::st_read(dsn = file.path(data_dir, "CondemnationZones_sim.kml")) %>%
  sf::st_zm()

bottom_longlines_restrictions <- sf::st_read(dsn = file.path(data_dir, "bottom_longlines_restrictions/bottom_longlines_restrictions.shp"))
fish_trap_restrictions <- sf::st_read(dsn = file.path(data_dir, "fish_traps_restrictions/fish_traps_restrictions.shp"))
octocoral_gear_restrictions <- sf::st_read(dsn = file.path(data_dir, "octocoral_gear_restrictions/octocoral_gear_restrictions.shp"))
roller_rig_trawls_restrictions <- sf::st_read(dsn = file.path(data_dir, "roller_rig_trawls_restrictions/roller_rig_trawls_restrictions.shp"))
sargassum_restrictions <- sf::st_read(dsn = file.path(data_dir, "sargassum_restrictions/sargassum_restrictions.shp"))

## North Carolina mechanical methods prohibited
mechanical_methods_prohibited <- sf::st_read(dsn = file.path(data_dir, "mechanical_methods_prohibited/mechanical_methods_prohibited.shp"))

## North Carolina shrimp trawl prohibited area
shrimp_trawl_prohibited <- sf::st_read(dsn = file.path(data_dir, "shrimp_trawl_net_prohibited/shrimp_trawl_net_prohibited.shp"))

## North Carolina pound net prohibited area
pound_net_prohibited <- sf::st_read(dsn = file.path(data_dir, "pound_net_prohibited/pound_net_prohibited.shp"))

## North Carolina trawl net prohibited areas
tnp <- sf::st_read(dsn = file.path(data_dir, "tnp/trawl_nets_prohibited.shp"))

## scup gear restricted areas
scup_gear_restricted_areas <- sf::st_read(dsn = file.path(data_dir, "scup-gear-restricted-areas-20161114-noaa-garfo/Scup_Gear_Restricted_Areas/Scup_Gear_Restricted_Areas.shp"))

# SAFMC restrictions
safmc_restrictions <- sf::st_read(dsn = file.path(data_dir, "SAFMC_regulations/SAFMC_Regulations.shp"))

## recreational fishing seasons and closures
recreational_fishing_season_closure <- sf::st_read(dsn = file.path(data_dir, "recreational_fishing_seasons_and_closures/recreational_fishing_seasons_and_closures.shp"))

# VMS All 2015 - 2019
# VMS Declared Out of Fishery 2015 - 2019
# VMS Monkfish May 2015 - April 2019
# VMS Multispecies May 2015 - April 2019
# VMS Pelagics 2015 - 2016 Less than 4 knots
# VMS Scallops April 2015 - March 2019
# VMS Squid 2015 - 2016 Less than 4 knots
# VMS Squid/Mackerel/Butterfish 2015 - 2019
# Communities at Sea - Total Bottom Trawl > 65 ft Activity 2011 - 2015
# Communities at Sea - Total Dredge Activity 2011 - 2015
# Communities at Sea - Total Gillnet Activity 2011 - 2015
# Communities at Sea - Total Longline Activity 2011 - 2015
# Communities at Sea - Total Pots and Traps Activity 2011 - 2015
# Communities at Sea - Total Bottom Trawl < 65 ft Activity 2011 - 2015
# Communities at Sea - Total Lobster Activity 2011 - 2015
# Communities at Sea - Total Party/Charter Activity 2011 - 2015
# Communities at Sea - Total Shrimp Activity 2011 - 2015

#####################################
#####################################

# create list of the datasets
data <- list(sc_trawl,
             nc_pfa,
             clams,
             baylor,
             nc_crab,
             shrimp_vms,
             sc_shrimp_landings,
             aquaculture,
             commercial_fish,
             fish_landings_area_code,
             fish_landings_species,
             nc_shrimp,
             fyke_nets_points,
             fyke_nets_lines,
             pound_nets_points,
             pound_nets_lines,
             staked_gill_nets_points,
             staked_gill_nets_lines,
             vms_demarcation,
             clam_amendment,
             coral_amendment,
             fishery_management_area,
             nc_sga_classifications,
             sc_shellfish_harvest_classifications,
             nc_cultch_1981_2002,
             nc_cultch_2003_present,
             open_harvest,
             oyster_garden,
             lease_applications,
             private_leases,
             dpa,
             agna_combine,
             mcha,
             sc_shellfish_permit,
             sc_monitoring_stations,
             lobster_gear,
             illex_fishery,
             altantic_red_drum,
             sne_dogfish,
             sne_regulated,
             sne_exemption_area,
             scup_sea,
             sne_monkfish_skate_trawl,
             sne_monkdish_skate_gillnet,
             atl_sea_scallop,
             va_pound_net,
             cape_lookout_hapc_fishery_fma,
             cape_lookout_hapc_ext_fishery_fma,
             cape_fear_hapc_fma,
             stetson_miami_hapc_fma,
             blake_ridge,
             mpas,
             sa_atlantic_fma,
             comm_vessel_snapper_grouper,
             sea_bass_fma,
             smz_fma,
             longline_pa_fma,
             spawing_smz,
             bsb_pot_nov_apr_fma,
             bsb_pot_dec_mar_fma,
             sa_shrimp_cold_fma,
             octocoral_closed_fma,
             golden_crab_fma,
             charleston_bump_fma,
             pelagic_sargassum_fma,
             king_mackerel_fma,
             spanish_mackerel_fma,
             cobia_fma,
             dsoma,
             rhma,
             sbma,
             sc_shellfish_management_area,
             chma_line,
             monkfish_fma,
             skate_management_unit,
             sf_scup_bsb_management_units,
             nc_oyster_sanctuaries,
             va_shellfish_condemnation,
             bottom_longlines_restrictions,
             fish_trap_restrictions,
             octocoral_gear_restrictions,
             roller_rig_trawls_restrictions,
             sargassum_restrictions,
             mechanical_methods_prohibited,
             shrimp_trawl_prohibited,
             pound_net_prohibited,
             tnp,
             scup_gear_restricted_areas,
             safmc_restrictions,
             recreational_fishing_season_closure
             ## Atlantic Large Whale Take Reduction Plan Regulated and Exempted Waters
             
             # VMS All 2015 - 2019
             # VMS Declared Out of Fishery 2015 - 2019
             # VMS Monkfish May 2015 - April 2019
             # VMS Multispecies May 2015 - April 2019
             # VMS Pelagics 2015 - 2016 Less than 4 knots
             # VMS Scallops April 2015 - March 2019
             # VMS Squid 2015 - 2016 Less than 4 knots
             # VMS Squid/Mackerel/Butterfish 2015 - 2019
             # Communities at Sea - Total Bottom Trawl > 65 ft Activity 2011 - 2015
             # Communities at Sea - Total Dredge Activity 2011 - 2015
             # Communities at Sea - Total Gillnet Activity 2011 - 2015
             # Communities at Sea - Total Longline Activity 2011 - 2015
             # Communities at Sea - Total Pots and Traps Activity 2011 - 2015
             # Communities at Sea - Total Bottom Trawl < 65 ft Activity 2011 - 2015
             # Communities at Sea - Total Lobster Activity 2011 - 2015
             # Communities at Sea - Total Party/Charter Activity 2011 - 2015
             # Communities at Sea - Total Shrimp Activity 2011 - 2015
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
  sf::st_write(obj = dataset, dsn = fisheries_aquaculture_geopackage, layer = data_code[i], append = F)
}

#####################################
#####################################

# calculate end time and print time difference
print(Sys.time() - start) # print how long it takes to calculate

# Amy Keister
# November 2022
# Imports data from openstreetmap for the Southeast Blueprint area, pulls out highway types that we think are trails/greenways. Want to remove trails marked with a tag of "access"=>"private" from the other_tags column, but haven't gotten that part to work yet.

#install.packages("broom", type="binary")


#import packages
require(raster)
require(tidyverse)
require(sf)
require(mapview)
require(rgdal)
require(osmdata)
require(osmextract)

#set working directory
setwd("D:/SE_Blueprint_2023/4_Indicators/CarGreenwaysAndTrails/OpenStreetMap20230624")
wd<-"D:/SE_Blueprint_2023/4_Indicators/CarGreenwaysAndTrails/OpenStreetMap20230624"

## I tried using oe_get, which should do all the steps above more quickly, but kept hitting problems. need to change the default download location and don't know how, need to find the magic word to call up the right files, etc.

## this checks the current default directory used by oe_get. It isn't going to a place where I can easily use it. I haven't figured out how to change the default directory
#oe_download_directory()

#oe_get(
#   "us/puerto-rico", 
#   layer = "lines",
#   vectortranslate_options = my_vectortranslate
# )
# 
# oe_get(
#   "us/us_virgin-islands", 
#   layer = "lines",
#   vectortranslate_options = my_vectortranslate
# )
# 
# oe_get(
#  "us/missouri", 
#  layer = "lines",
#  vectortranslate_options = my_vectortranslate
# )

## tried to use this to pull the access info from the openstreetmap, but having issues with oe_get
# colnames(oe_get(
#   "us/missouri", 
#   layer = "lines",
#   query = "SELECT *, hstore_get_value(other_tags, 'access=>private') AS 'access=>private' FROM lines",
#   vectortranslate_options = my_vectortranslate
# ))

## tried to list keys but having problems accessing the default download location on my machine
#oe_get_keys("us/missouri")

# download all the open street files for my area individually. There are search functions, but I wasn't guessing the magic words well. Easier right now to just give it the exact links I need. These 4 downloads should cover the whole SE Blueprint area.

oe_download(
  file_url = "https://download.geofabrik.de/north-america/us/puerto-rico-latest.osm.pbf", 
  provider = "geofabrik",
  download_directory = wd
)

oe_download(
  file_url = "https://download.geofabrik.de/north-america/us/us-virgin-islands-latest.osm.pbf", 
  provider = "geofabrik",
  download_directory = wd
)

# oe_download(
#   file_url = "https://download.geofabrik.de/north-america/us/missouri-latest.osm.pbf", 
#   provider = "geofabrik",
#   download_directory = "E:/WORKING/SE_Blueprint_2023/Indicators/CaribbeanGreenwaysAndTrails/OpenStreetMap20230213"
# )
# 
# oe_download(
#   file_url = "https://download.geofabrik.de/north-america/us-south-latest.osm.pbf", 
#   provider = "geofabrik",
#   download_directory = "E:/WORKING/SE_Blueprint_2023/Indicators/CaribbeanGreenwaysAndTrails/OpenStreetMap20230213"
# )


# make a list of the file path and name for these downloaded files

files<- dir(pattern = "latest.osm.pbf$")

print(files)

# this writes the query used in oe_vectortranslate below, I couldn't figure out the syntax to get it to work without pulling it out separate
my_vectortranslate = c(
  # SQL-like query where we select only the following fields
  "-select", "osm_id, name, highway, other_tags",
  # SQL-like query where we filter only the features where highway is equal to footway, cycleway, bridalway, or path
  # for Virgin Islands and Puerto Rico, we played around with looking at track types for a while
  # "-where", "highway IN ('footway', 'cycleway', 'bridleway', 'path','track_grade4','track_grade3','track')"
  "-where", "highway IN ('footway', 'cycleway', 'bridleway', 'path')"
)

# # this writes the query used in oe_vectortranslate below, I couldn't figure out the syntax to get it to work without pulling it out separate
# my_vectortranslate = c(
#   # SQL-like query where we select only the following fields
#   "-select", "osm_id, name, highway, other_tags", 
#   # SQL-like query where we filter only the features where highway is equal to footway, cycleway, bridalway, or path
#   # for Virgin Islands and Puerto Rico, I'm having to add in more types
#   "-where", "highway IN ('footway', 'cycleway', 'bridleway', 'path')"
# )

#Loop through the 4 downloaded files, pullout out only lines, apply queries to pull out specific fields and query attributes for lines that seem like trails, convert to geopackage
for(i in 1:length(files)){
  # Get the current file
  dat<- oe_vectortranslate(
    files[i],
    layer = "lines",
    vectortranslate_options = my_vectortranslate
  )
}
   
#remake the files list to list the geopackages   
files<- dir(pattern = "latest.gpkg$")

print(files)

# loop through geopackage files and merge them, I kept the filter and select to remember how to do a bunch of stuff at once, even though we don't need that right now
for(i in 1:length(files)){
  # Get the current file
  dat<- st_read(files[i], stringsAsFactors = FALSE) #%>%
    # this narrows down the selection to not have any tracks
    #filter(highway %in% c("bridleway","cycleway","footway","path")) %>%
    #dplyr::select(osm_id, highway, other_tags, name, geometry)
  # Test if on first iteration
  if(i == 1){
    AllTrails <- dat
  } else {
    AllTrails <- rbind(AllTrails, dat)
  }
}

# make a datset with only trails marked as private (when I tried do return all except private, it also removed the rows with NA in other tags, so i'm doing it a bit backwards)
PrivateTrails <- AllTrails%>%
  dplyr::filter(str_detect(other_tags,'"access"=>"private"'))

# make a datset with only trails marked as a sidewalk
Sidewalks <- AllTrails%>%
  dplyr::filter(str_detect(other_tags,'"footway"=>"sidewalk"'))

# # loop through geopackage files and merge them, apply another filter to pull out marked as private
# for(i in 1:length(files)){
#   # Get the current file
#   dat<- st_read(files[i], stringsAsFactors = FALSE) %>%
#     # this selects trails marked as private
#     dplyr::filter(str_detect(other_tags,'"access"=>"private"'))
#   # Test if on first iteration
#   if(i == 1){
#     trails <- dat
#   } else {
#     PrivateTrails <- rbind(trails, dat)
#   }
# }

# # loop through geopackage files and merge them, apply another filter to try to remove lines marked as private
# for(i in 1:length(files)){
#   # Get the current file
#   dat<- st_read(files[i], stringsAsFactors = FALSE) %>%
#     # this isn't working quite right, i'm trying to delete all features that have "access"=>"private" in the other_tags column, but it is also deleting the files that have NULL in the other_tags
#     dplyr::filter(!str_detect(other_tags,'"access"=>"private"'))
#   # Test if on first iteration
#   if(i == 1){
#     trails <- dat
#   } else {
#     NotPrivateTrails <- rbind(trails, dat)
#   }
# }

# # loop through geopackage files and merge them, apply a filter to keep those marked as sidewalk
# for(i in 1:length(files)){
#   # Get the current file
#   dat<- st_read(files[i], stringsAsFactors = FALSE) %>%
#     # Select only trails marked as sidewalks
#     dplyr::filter(str_detect(other_tags,'"footway"=>"sidewalk"'))
#   # Test if on first iteration
#   if(i == 1){
#     trails <- dat
#   } else {
#     Sidewalks <- rbind(trails, dat)
#   }
# }

NotPrivateTrails <- AllTrails[!AllTrails$osm_id %in% PrivateTrails$osm_id,]

NotSidewalks <- AllTrails[!AllTrails$osm_id %in% Sidewalks$osm_id,]

# export merged trails as a geopackage
st_write(AllTrails, dsn = "./Greenways.gpkg", layer = "AllTrails", driver = "GPKG", delete_layer = TRUE, append = TRUE)

# export merged trails as a geopackage
st_write(PrivateTrails, dsn = "./Greenways.gpkg", layer = "PrivateTrails", driver = "GPKG", delete_layer = TRUE, append = TRUE)

# export merged trails as a geopackage
st_write(NotPrivateTrails, dsn = "./Greenways.gpkg", layer = "NotPrivateTrails", driver = "GPKG", delete_layer = TRUE, append = TRUE)

# export merged trails as a geopackage
st_write(Sidewalks, dsn = "./Greenways.gpkg", layer = "Sidewalks", driver = "GPKG", delete_layer = TRUE, append = TRUE)

# export merged trails as a geopackage
st_write(NotSidewalks, dsn = "./Greenways.gpkg", layer = "NotSidewalks", driver = "GPKG", delete_layer = TRUE, append = TRUE)
  
# Export trails as shapefile
#st_write(trails, dsn = ".", layer = "trails" driver = "ESRI Shapefile", delete_layer = TRUE, update = TRUE)








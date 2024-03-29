# Amy Keister
# November 2022
# Imports data from openstreetmap for the VIPR area


#import packages
require(raster)
require(tidyverse)
require(sf)
require(mapview)
require(rgdal)
require(osmdata)
require(osmextract)

#set working directory
setwd("F:/GIS_DATA/Cultural_Resources/OpenStreetMap20230314")




# download all the open street files for my area individually. There are search functions, but I wasn't guessing the magic words well. Easier right now to just give it the exact links I need. These 4 downloads should cover the whole SE Blueprint area.

oe_download(
  file_url = "https://download.geofabrik.de/north-america/us/puerto-rico-latest.osm.pbf", 
  provider = "geofabrik",
  download_directory = "F:/GIS_DATA/Cultural_Resources/OpenStreetMap20230314"
)

oe_download(
  file_url = "https://download.geofabrik.de/north-america/us/us-virgin-islands-latest.osm.pbf", 
  provider = "geofabrik",
  download_directory = "F:/GIS_DATA/Cultural_Resources/OpenStreetMap20230314"
)

# oe_download(
#   file_url = "https://download.geofabrik.de/north-america/us/missouri-latest.osm.pbf",
#   provider = "geofabrik",
#   download_directory = "F:/GIS_DATA/Cultural_Resources//OpenStreetMap20230314"
# )
# 
# oe_download(
#   file_url = "https://download.geofabrik.de/north-america/us-south-latest.osm.pbf",
#   provider = "geofabrik",
#   download_directory = "F:/GIS_DATA/Cultural_Resources/OpenStreetMap20230314"
# )

# make a list of the file path and name for these downloaded files

files<- dir(pattern = "latest.osm.pbf$")

print(files)

#this writes the query used in oe_vectortranslate below, I couldn't figure out the syntax to get it to work without pulling it out separate
my_vectortranslate = c(
  # SQL-like query where we select only the following fields
  "-select", "osm_id, name, historic, other_tags",
  # SQL-like query where we filter only the features where highway is equal to footway, cycleway, bridalway, or path
  # for Virgin Islands and Puerto Rico, we played around with looking at track types for a while
  # "-where", "historic IN ('footway', 'cycleway', 'bridleway', 'path','track_grade4','track_grade3','track')"
  "-where", "historic IS NOT NULL"
)

#Loop through the 4 downloaded files, convert polygons to geopackage
for(i in 1:length(files)){
  # Get the current file
  dat<- oe_vectortranslate(
    files[i],
    layer = "multipolygons",
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
  # this selects trails marked as private
  #filter(highway %in% c("bridleway","cycleway","footway","path","track_grade4","track_grade3","track")) %>%
  #dplyr::select(osm_id, highway, other_tags, name, geometry)
  #dplyr::filter(str_detect(other_tags,'"access"=>"private"'))
  # Test if on first iteration
  if(i == 1){
    Historic <- dat
  } else {
    Historic <- rbind(Historic, dat)
  }
}





# export merged polygons as a geopackage
st_write(Historic, dsn = "./Historic.gpkg", layer = "Historic", driver = "GPKG", delete_layer = TRUE, append = TRUE)











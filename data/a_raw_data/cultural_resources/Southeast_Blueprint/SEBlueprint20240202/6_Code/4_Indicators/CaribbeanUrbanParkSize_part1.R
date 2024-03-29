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
setwd("E:/projects/BP/BP23/OpenStreetMap20230221")




# download all the open street files for my area individually. There are search functions, but I wasn't guessing the magic words well. Easier right now to just give it the exact links I need. These 4 downloads should cover the whole SE Blueprint area.

oe_download(
  file_url = "https://download.geofabrik.de/north-america/us/puerto-rico-latest.osm.pbf", 
  provider = "geofabrik",
  download_directory = "E:/projects/BP/BP23/OpenStreetMap20230221"
)

oe_download(
  file_url = "https://download.geofabrik.de/north-america/us/us-virgin-islands-latest.osm.pbf", 
  provider = "geofabrik",
  download_directory = "E:/projects/BP/BP23/OpenStreetMap20230221"
)


# make a list of the file path and name for these downloaded files

files<- dir(pattern = "latest.osm.pbf$")

print(files)

# this writes the query used in oe_vectortranslate below, I couldn't figure out the 
#syntax to get it to work without pulling it out separate

# I started looking for tags we would be interested in but I didn't follow through. 
#this is what you would use if you want to only pull out certain leisure tags, 
#need to explore further here https://wiki.openstreetmap.org/wiki/Key:leisure

my_vectortranslate = c(
  # SQL-like query where we select only the following fields
  "-select", "osm_id, name, leisure, other_tags",
  # SQL-like query where we filter only the features where highway is equal to footway, cycleway, bridalway, or path
  # for Virgin Islands and Puerto Rico, we played around with looking at track types for a while
  # "-where", "highway IN ('footway', 'cycleway', 'bridleway', 'path','track_grade4','track_grade3','track')"
#  "-where", "leisure IN ('beach_resort','common','fishing','Nature_reserve','park','picnic_table', 'playground', 'swimming area', 'wildlife_hide')" # TURN THIS ON TO SAVE OTHER PARKS
#  "-where", "leisure IN ('Nature_reserve','park')" # TURN THIS ON TO SAVE OTHER PARKS
"-where", "natural IN ('beach')" #TURN THIS ONE TO SAVE BEACHES
)

#Loop through the 4 downloaded files, pullout out only lines, apply queries to pull out specific fields and query attributes for lines that seem like trails, convert to geopackage
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
    # this narrows down the selection to not have any tracks
    #filter(highway %in% c("bridleway","cycleway","footway","path")) %>%
    #dplyr::select(osm_id, highway, other_tags, name, geometry)
  # Test if on first iteration
  if(i == 1){
    Leisure <- dat
  } else {
    Leisure <- rbind(Leisure, dat)
  }
}



# export merged roads as a SHAPEFILE - geopackage does strange things
#st_write(Leisure, dsn = "./beach.gpkg", layer = "beach", driver = "GPKG", delete_layer = TRUE, append = TRUE)
#st_write(Leisure, dsn = "./Leisure.gpkg", layer = "Leisure", driver = "GPKG", delete_layer = TRUE, append = TRUE)
#st_write(Leisure, dsn = "Leisure.shp", layer = "Leisure.shp", driver = "ESRI Shapefile")
st_write(Leisure, dsn = "beach20230314.shp", layer = "beach.shp", driver = "ESRI Shapefile")








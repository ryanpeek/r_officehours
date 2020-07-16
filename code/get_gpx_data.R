library(sf)
library(dplyr)
library(lubridate)
library(mapview)

# https://geocompr.github.io/geocompkg/articles/gps-tracks.html

# the gpx file
file1 <- "~/Downloads/NFA.GPX"
st_layers(file1)

# read just the tracks:
trx <- st_read(file1, layer = "tracks")
trx$name

# pull a single track
trx1 <- trx %>% filter(name=="2017-05-05-NFA")

mapview(trx1)

# get points
pts <- st_read(file1, layer="waypoints") %>% 
	filter(!grepl("NFARRAV", x = name)) %>% 
	mutate(YYYY = year(time))

mapview(pts, zcol="YYYY") + mapview(trx1, color="orange")

# write out to a geopackage (saves the spatial components and data together)
# st_write(pts, dsn = "data_output/rabo_eggs_timing.gpkg", layer="nfa_eggmasses")
# st_write(trx, dsn="data_output/rabo_eggs_timing.gpkg", layer="nfa_egg_surveys")
# st_layers("data_output/rabo_eggs_timing.gpkg")

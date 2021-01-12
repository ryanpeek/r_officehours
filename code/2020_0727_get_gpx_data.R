# Working with .gpx files in SF


# Packages We Need --------------------------------------------------------

# this requires the following packages

library(sf)
library(dplyr)
library(lubridate)
library(mapview)


# Read in our File! -------------------------------------------------------

# can download locally from here: https://raw.githubusercontent.com/ryanpeek/r_officehours/master/data/NFA.GPX

# the gpx file we'll use: here a locally downloaded file
file1 <- "~/Downloads/NFA.GPX"

# or read straight from the interwebs
file1 <- "https://raw.githubusercontent.com/ryanpeek/r_officehours/master/data/NFA.GPX"

# read just the tracks:
trx <- st_read(file1, layer = "tracks")

# check the names of the tracks
trx$name

# these are dates from a GPS for each day the track was taken


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

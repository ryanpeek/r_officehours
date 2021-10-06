# R help 2021-10-06


# Mapping and Getting Flowlines -------------------------------------------

# mapping and getting flowlines packages
library(sf)
library(tidyverse)
library(mapview)
library(dataRetrieval)

# download data based on a pt or comid
df <- findNLDI(comid = 101, nav = c("UM", "DM"),
               find = c("basin", "flowlines"),
               distance_km = 100) 

# extract riverlines
um_flowlines <- df$UM_flowlines

# preview 
mapview(um_flowlines) + mapview(df$basin)

# get lengths for each comid
um_flowlines<- um_flowlines %>% 
  mutate(comid_length_m = purrr::map_dbl(st_length(um_flowlines),~sum(.x)))

# or this
df_lengths <- tapply(st_length(um_flowlines), um_flowlines$nhdplus_comid, sum) %>% as.data.frame()

# add map with measurements! zoom in to see finer scale
mapview(um_flowlines, zcol="comid_length_m")@map %>% leaflet.extras::addMeasurePathToolbar()


# another option?
mapview(um_flowlines, zcol="nhdplus_comid")@map %>% leaflet::addMeasure()

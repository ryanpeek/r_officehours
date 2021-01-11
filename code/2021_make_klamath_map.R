# make klamath map

library(sf)
library(tidyverse)
library(mapview)
mapviewOptions(fgb = FALSE)
library(USAboundaries)

# Get Data ----------------------------------------------------------------

st_layers("data_output/klam_watershed_full.gpkg")

h6 <- read_rds("data_output/klam_h12.rds")

states <- USAboundaries::us_boundaries(type="state",states=c("ca","or"))

klam_riv_poly <- st_read("data_output/klam_watershed.gpkg", layer="klam_river_poly")
klam_wb <- st_read("data_output/klam_watershed_full.gpkg", layer="NHDWaterbody")
rivs <- st_read("data_output/klam_watershed_full.gpkg", layer="NHDFLowline_Network")

ecoreg <- st_read("data_output/klam_watershed_full.gpkg", layer="ecoreg_ca_or")

# read in mainstem
klam_main <- read_rds(file = "data_output/klam_flowline_mainstem.rds")


# Dissolve Boundaries -----------------------------------------------------

h6_bound <- h6 %>% group_by(HUC_6) %>% 
  summarize() # or use st_union

# crop ecoregions
mapview(h6_bound, col.regions="transparent", alpha.regions=0, color="orange") + mapview(klam_riv_poly, col.regions="steelblue") + mapview(ecoreg, zcol="US_L3NAME") + 
  mapview(klam_rivs, color="steelblue", lwd=0.4)

ca <- states %>% filter(stusps=="CA")


# Mapshaper ---------------------------------------------------------------
# using mapshapr
# library(rmapshaper)
# h8s <- rmapshaper::ms_simplify(h8_bound, keep = 0.1) 
# mapview(h8s) + mapview(h8_bound, col.regions=NA, alpha.regions=0.2)

# # pull out huc
# h8_ca <- h8 %>% filter(states=="CA") %>% 
#   mutate(huc6=str_sub(huc8, 1, 6)) %>% 
#   rename(geometry=Shape)
# 
# # dissolve
# h6 <- h8_ca %>% 
#   rmapshaper::ms_dissolve(., field = "huc6") #%>% 



# Make a TMAP -------------------------------------------------------------

library(tmap)
library(tmaptools)

# make larger bounding box
klam_bb <- h6_bound %>% st_transform(3310) %>% 
  st_bbox() %>% st_as_sfc() %>% st_buffer(1000)

# crop ecoreg by this:
eco_crop <- st_intersection(ecoreg, st_transform(klam_bb, 4326))

# map
klam_base <- tmaptools::read_osm(klam_bb, type="esri-topo", zoom = 8,raster=TRUE)

# basemap
(map1 <- tm_shape(klam_base) + tm_rgb())

# full map
(map2 <- map1 + 
    tm_shape(ecoreg)+
    tm_fill(col = "L3_KEY", alpha=0.2, legend.show = FALSE) +
    tm_shape(h6_bound) + 
    tm_polygons(border.col = "slateblue4", alpha = 0, border.alpha = 0.5, lwd=2) +
    #tm_shape(klam_wb) + 
    #tm_polygons(border.col="steelblue", alpha=0.5, lwd=0.5) +
    tm_shape(rivs) + 
    tm_lines(col="steelblue", alpha=0.5, lwd=0.5) +
    tm_shape(klam_main) + 
    tm_lines(col="steelblue", alpha=0.9, lwd=1.2) +
    tm_shape(klam_riv_poly) + 
    tm_polygons(col="dodgerblue", alpha=0.7, border.col = "steelblue", border.alpha = 0.8) +
    tm_compass(size = 3, type="8star", position=c("left", "bottom"), show.labels = 2, color.light = "transparent") +
    tm_scale_bar(position=c("left", "bottom")))

tmap_save(map2, filename = "figures/klam_basemap_ecoregs.png", width = 8.5, height = 11, units = "in", dpi = 300)


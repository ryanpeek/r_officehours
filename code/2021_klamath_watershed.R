# watershed boundary maps

library(sf)
library(tidyverse)
library(nhdplusTools)
library(mapview)
mapviewOptions(fgb = FALSE)
library(USAboundaries)

# Get Data ----------------------------------------------------------------

states <- USAboundaries::us_boundaries(type="state",states=c("ca","or"))

# Get HUCs ----------------------------------------------------------------

st_layers("data/WBD_dataset.gdb")

hucs <- read_sf("data/WBD_dataset.gdb/", "WBDSnapshot_National")

hucKlam <- hucs %>% dplyr::filter(HUC_8 == "18010209")
                                  #Trinity | HUC_10 == "1801021112")

# klamath outlet
klam_out <- st_sfc(st_point(c(-124.07942, 41.54315))) %>% 
                     st_set_crs(4326) %>% st_sf()

mapview(hucKlam) + mapview(klam_out, col.regions="yellow")

# save out
write_rds(hucKlam, file = "data_output/klam_h8.rds")


# nhdTools ----------------------------------------------------------------

start_comid <- discover_nhdplus_id(klam_out)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 300)

subset <- subset_nhdplus(comids = flowline$nhdplus_comid,
                         output_file = "data_output/klam_watershed.gpkg",
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE)



# Get EcoRegions & Load Data ----------------------------------------------------------

#ecoreg <- read_sf("data/us_eco_l3.shp") %>% st_transform(4326)
# trim to just ca,or
ecoreg_l <- ecoreg[states,]
#st_write(ecoreg_l, "data_output/klam_watershed.gpkg", layer = "ecoreg_ca_or")

nhdarea <- st_read("data_output/klam_watershed.gpkg", layer = "NHDArea") %>% st_transform(4326)

ca <- states %>% filter(stusps=="CA")

# get HUC
hucKlam <- read_rds("data_output/klam_h8.rds")
huc_diss <- st_union(hucKlam) # dissolve 

# river poly
klam_poly <- st_intersection(nhdarea, ca)
klam_poly <- klam_poly %>% filter(id=="nhdarea.16155")

#st_write(klam_poly, "data_output/klam_watershed.gpkg", layer = "klam_river_poly")

st_layers("data_output/klam_watershed.gpkg")

# catchments
catch <- st_read("data_output/klam_watershed.gpkg", layer="CatchmentSP")

# flowlines
klam_flowline <- st_read("data_output/klam_watershed.gpkg", layer="NHDFlowline_Network")

# Map ---------------------------------------------------------------------
library(tmap)
library(tmaptools)

# make larger bounding box
klam_bb <- klam_poly %>% st_transform(3310) %>% 
  st_bbox() %>% st_as_sfc() %>% st_buffer(5000)

# map
klam_base <- tmaptools::read_osm(klam_bb, type="esri-topo", raster=TRUE)

# basemap
(map1 <- tm_shape(klam_base) + tm_rgb())

# full map
(map2 <- map1 + 
    tm_shape(ecoreg_l)+
    tm_fill(col = "L3_KEY", alpha=0.3, legend.show = FALSE) +
    tm_shape(huc_diss) + 
    tm_polygons(border.col = "slateblue4", alpha = 0, border.alpha = 0.5, lwd=2) +
    tm_shape(klam_flowline) + 
    tm_lines(col="steelblue", alpha=0.5, lwd=0.5) +
    tm_shape(klam_poly) + 
    tm_polygons(col="dodgerblue", border.col = "steelblue", border.alpha = 0.8) +
    tm_compass(size = 3, type="8star", position=c("left", "bottom"), show.labels = 2, color.light = "transparent") +
    tm_scale_bar(position=c("left", "bottom")))

tmap_save(map2, filename = "figures/klam_basemap_ecoregs.png", width = 8.5, height = 11, units = "in", dpi = 300)

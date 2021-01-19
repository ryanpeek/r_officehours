# make klamath map

library(sf)
library(tidyverse)
library(mapview)
mapviewOptions(fgb = FALSE)
library(USAboundaries)

# Get Data ----------------------------------------------------------------

hucs <- read_rds("data_output/klam_h12.rds")

states <- USAboundaries::us_boundaries(type="state",states=c("ca","or"))
ca <- states %>% filter(stusps=="CA")

#klam_riv_poly <- st_read("data_output/klam_watershed.gpkg", layer="klam_river_poly")
#klam_wb <- st_read("data_output/klam_watershed_full.gpkg", layer="NHDWaterbody")
#rivs <- st_read("data_output/klam_watershed_full.gpkg", layer="NHDFLowline_Network")
#ecoreg <- st_read("data_output/klam_watershed_full.gpkg", layer="ecoreg_ca_or")

# read in mainstem
klam_main <- read_rds(file = "data_output/klam_flowline_mainstem.rds")


# Dams --------------------------------------------------------------------

dams <- st_read("data/CA_OR_dams.shp") %>% st_transform(4269)

dams_c <- dams[h6_bound,] %>% 
  filter(NAME %in% c("Iron Gate", "Lewiston","Trinity") | damname %in% c("Keno Dam", "Upper Klamath Lake"))

# Get Tribs ---------------------------------------------------------------
# LOAD TRIBS
# get tribs
# load("data_output/klam_flowline_UT.rda") # flowlineUT

library(nhdplusTools)
# subset <- subset_nhdplus(comids = klam_tribs$nhdplus_comid,
#                          output_file = "data_output/klam_watershed.gpkg",
#                          nhdplus_data = "download", 
#                          flowline_only = FALSE,
#                          return_data = TRUE)

# get tribs
st_layers("data_output/klam_watershed.gpkg")
klam_wb <- st_read(dsn = "data_output/klam_watershed.gpkg", "NHDWaterbody")
klam_tribs <- st_read(dsn = "data_output/klam_watershed.gpkg", "NHDFlowline_Network")


# Filter Waterbodies ------------------------------------------------------

klam_wbl <- klam_wb %>% filter(id=="nhdwaterbody.439901")

# filter:
# nhdwaterbody.439901 (upper klam and agency)

# Dissolve Boundaries -----------------------------------------------------

h6_bound <- hucs %>% group_by(HUC_6) %>% summarize() # or use st_union
h8_bound <- hucs %>% group_by(HUC_8) %>% summarize() 

# get ecoregs
ecoreg <- read_sf("data/us_eco_l3.shp") %>% st_transform(4269)

# trim to just ca,or
ecoreg_l <- ecoreg[h6_bound,]

# relevel
ecoreg_l$US_L3NAME <- forcats::as_factor(ecoreg_l$US_L3NAME)
# add custom labels
levels(ecoreg_l$US_L3NAME)
ecoreg_l$US_L3name <- forcats::fct_recode(ecoreg_l$US_L3NAME, 'Coast Range'='Coast Range', 'Cascades'='Cascades', 'Cent. CA Foothills/Coastal Mntns'='Central California Foothills and Coastal Mountains', 'Klamath/CA High North Coast Range'='Klamath Mountains/California High North Coast Range', 'Eastern Cascades'='Eastern Cascades Slopes and Foothills') 
levels(ecoreg_l$US_L3name)

# tst mapview
mapview(h8_bound, col.regions="transparent", alpha.regions=0, color="gray", lwd=1.6, lty=2, legend=FALSE) + 
  mapview(ecoreg_l, zcol="US_L3NAME", alpha.regions=0.1, color="gray20", alpha=0.5, layer.name="Ecoregions") + 
  mapview(klam_main, color="dodgerblue", lwd=2, legend=FALSE)

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
#   rmapshaper::ms_dissolve(., field = "huc6") 

# Make a TMAP -------------------------------------------------------------

library(tmap)
library(tmaptools)

# make larger bounding box
klam_bb <- h6_bound %>% st_transform(3310) %>% 
  st_bbox() %>% st_as_sfc() %>% st_buffer(1000)

# crop ecoreg by this:
#eco_crop <- st_intersection(ecoreg_l, st_transform(klam_bb, 4269))

# add tribs level
klam_tribs <- klam_tribs %>% 
  mutate(streamorder_map = streamorde*2)

# map # options: stamen-toner, "osm-bw", "nps", "osm-transport" "esri-topo"
klam_base <- tmaptools::read_osm(klam_bb, type="stamen-toner", zoom = 8,raster=TRUE)

# basemap
map1 <- tm_shape(klam_base) + tm_rgb()

# full map
(map2 <- map1 + 
    #tm_shape(states) +
    #tm_polygons(col="black", lwd=2, alpha=0.5) +
    tm_shape(ecoreg_l)+
    tm_fill(col = "US_L3name", 
            legend.is.portrait = TRUE,
            alpha=0.6, 
            title = "Ecoregions", palette = "Greys") + 
    tm_layout(legend.outside = FALSE,
              legend.width = 1.5,
              frame=FALSE) +
              #fontfamily = "Roboto") +
    tm_shape(h6_bound) + 
    tm_borders(col = "cyan4", alpha = 0.9, lwd=1, lty=1) +
    tm_shape(h8_bound) + 
    tm_borders(col = "cyan4", alpha = 0.9, lwd=0.5, lty=3) +
    tm_shape(klam_tribs) + 
    tm_lines(col="steelblue", 
             lwd="streamcalc", scale = 2.25, 
             alpha=0.7, legend.lwd.show = FALSE) +

    tm_shape(klam_main) + 
    tm_lines(col="dodgerblue", alpha=1, lwd=2) +
    tm_shape(klam_wbl) +
    tm_sf(col="dodgerblue", border.col = "dodgerblue", alpha=0.9) +
    tm_shape(dams_c) +
    tm_sf(col="black", shape=17, size = 0.5) +
    tm_compass(type = "arrow", 
               color.light = "transparent") +
    tm_scale_bar(position=c("right", "bottom")))


# save out
tmap_save(map2, filename = "figures/klam_basemap_ecoregs.jpg", width = 8.5, height = 11, units = "in", dpi = 600)
tmap_save(map2, filename = "figures/klam_basemap_ecoregs.pdf", width = 8.5, height = 11, units = "in", dpi = 600)

# NOTES -------------------------------------------------------------------


## modify and add labels:
#tm_shape(lsh_springs) +
#  tm_dots(col="skyblue", size = 1.2, title = "Springs", legend.show = TRUE, shape = 21) +
#tm_text("Name", auto.placement = 0.7, just = "left", xmod = 0.35, shadow = TRUE)+
#  tm_layout(title = "Little Shasta",
#            frame = FALSE,
#            fontfamily = "Roboto Condensed",
#            legend.outside = FALSE, attr.outside = FALSE,
#            inner.margins = 0.01, outer.margins = (0.01),
            #legend.position = c(0.6,0.85),
#            title.position = c(0.7, 0.95)) +



## update map with
# Labeled dams – 
## Iron Gate (location, not dam), 
## Keno Dam, 
## Link River Dam, 
## Lewiston Dam, 
## Trinity Dam

# Labeled hatcheries – 
## Trinity River Hatchery, 
## Iron Gate Hatchery (decommissioned with dam removal)

# Labeled water bodies – 
## Upper Klamath Lake, 
## Agency Lake, 
## Trinity R., 
## S.Fk. Trinity R., 
## New R., 
## Klamath R.,
## Salmon R., 
## Williamson R., 
## Sprague R., 
## N.Fk. Sprague R., 
## Wood R., 
## Sevenmile Cr.
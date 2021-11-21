# maptime 2021-01-12: google earth engine talk by Christian

# Talk content: https://github.com/JepsonNomad/contextMaps
# Need GEE: https://code.earthengine.google.com/?authuser=1

# https://jepsonnomad.github.io/tutorials/EE_Maps.html


# Satellite Remote Sensing: -----------------------------------------------

# see here for image: https://i.stack.imgur.com/bVrxB.png

# maxPixels in Export.image.toDrive can be skewed...use scale equal to or larger than native image.


# Make Plot ---------------------------------------------------------------

## Curious about TOA vs SR? Check out this post for more:
# https://gis.stackexchange.com/questions/304180/what-are-the-min-and-max-values-of-map-addlayer-on-google-earth-engine


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(raster)
library(sf)
library(RStoolbox)
library(cowplot)
library(rmapshaper)

# Import Data -------------------------------------------------------------

## Raster
eeImg <- raster::stack("data_output/Yolo_LC08_RGB_2020_SR.tif")
eeImg

## Vector
## Yolo County
ROI <- st_read("data_output/Yolo_ROI.shp") #%>% 
  #st_union()
st_crs(ROI)

## CA State
CA <- USAboundaries::us_states() %>%
  st_as_sf() %>%
  filter(name == "California")
CA

## SN 
sn <- read_sf("data_output/SNevada_ROI.shp")
plot(sn$geometry)

## dissolve and simplify:
library(rmapshaper)
sn_poly <- ms_dissolve(sn, field="l3_key") # make single poly

# tstmap
plot(sn_poly$geometry, border="steelblue", lwd=3)
plot(sn$geometry, border="gray10", col="gray80", add=T)
plot(CA$geometry, add=T)
plot(ROI$geometry, add=T)

# simplify
sn_poly_s <- rmapshaper::ms_simplify(sn_poly)
sn_poly_s <- st_set_crs(sn_poly_s, 4326)
st_crs(sn_poly_s)
st_write(sn_poly_s, "data_output/sierra_nevada_polygon_simple.shp", delete_layer = TRUE)


# Data Wrangling ----------------------------------------------------------

## Raster

# Convert to data.frame
# Recall that we set maxPixels in the Earth Engine export to 1e8,
# that's the maximum value you should consider using here as well. 
# If your image is going to be a half the screen, you can use half
# the maxpixels. 
imgFortFull <- fortify(eeImg, maxpixels = 1e4) # test w 1e4
imgFort <- imgFortFull %>%
  rename("R" = vis.red,
         "G" = vis.green,
         "B" = vis.blue) %>%
  filter(R + G + B != 0) %>%
  mutate(Rsc = scales::rescale(R, to = c(0,1), from = c(0,255)),
         Gsc = scales::rescale(G, to = c(0,1), from = c(0,255)),
         Bsc = scales::rescale(B, to = c(0,1), from = c(0,255)))

# Basic GGPLOT ------------------------------------------------------------

## Vector context plot
(vectorPlot <- ggplot() +
  geom_sf(data = CA,
          fill = "grey90") +
   geom_sf(data = ROI,
          fill = "grey60") +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = "transparent"),  
        plot.background = element_rect(fill = "transparent", 
                                       colour = "transparent")))



## RGB plot
rgbPlot <- ggplot() +
  geom_tile(data = imgFort, 
            aes(x = x, y = y, 
                fill = rgb(Rsc,Gsc,Bsc)))  +
  scale_fill_identity() +
  geom_sf(data = ROI,
          fill = "transparent",
          lwd = 0.75,
          col = "grey40") +
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", 
                                        colour = "transparent"),  
        plot.background = element_rect(fill = "transparent", 
                                       colour = "transparent"))
#rgbPlot



#### Combine contextual vector and RGB imagery to a single plot with inset
## Generate a "complete" plot that includes contextual vector data
completePlot <- ggdraw() +
  draw_plot(rgbPlot) +
  draw_plot(vectorPlot, 
            x = 0.075, y = 0.075, 
            width = 0.4, height = 0.5)
# completePlot



## Save plots --------------------------------------------------------------
## Give some thought to the output metadata:
# A 4k display is 3840 x 2160 pixels. Therefore there's really no need to have presentation images be >3840 horizontal pixels or >2160 vertical pixels
# Let's do some math. If a figure is to take up half of a ppt slide, then we can happily take it to 1920 horizontal pixels. If that's the case, we can make final image dimensions 7x7 inches, with a resolution of 300 pixels per inch ("dpi") and lose no more information than we would from 4k viewing anyway (final dimensions 2100 x 2100 pixels).

ggsave(filename = "figures/yoloRGB_SR.jpg",
       rgbPlot, bg = "transparent",
       height = 7, width = 7, units = "in", dpi = 300)
ggsave(filename = "figures/yoloContext_SR.jpg",
       completePlot, bg = "transparent",
       height = 7, width = 7, units = "in", dpi = 300)



# Try TMAP? ---------------------------------------------------------------

library(tmap)
library(tmaptools)

# fix raster by replacing 0's with NAs
# replace 0 vals to NA with reclassify
eeImg_2 <- reclassify(eeImg, cbind(0, NA))

# make background
(map1 <- tm_shape(eeImg_2) + tm_rgb() +
    tm_layout(frame=FALSE))

# full map
(map2 <- map1 + 
    tm_shape(ROI)+
    tm_polygons(alpha=0, col = NA, border.col = "orange", lwd=4) +
    tm_compass(size = 3, type="8star", position=c("left", "bottom"), show.labels = 2, color.light = "transparent") +
    tm_scale_bar(position=c("left", "bottom")))

# make inset map
insetmap <- tm_shape(CA) + tm_fill(col="lightgrey") +
  tm_shape(ROI) + tm_borders(lwd = 2, col="orange") +
  tm_layout(bg.color = "transparent",inner.margins = c(0.04,0.04,0.04,0.04), outer.margins=c(0,0,0,0), frame = FALSE)
insetmap

# fit with viewport
library(grid)
xy <- st_bbox(ROI)
asp2 <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)
# set width and height
w <- 0.25
h <- asp2 * w
# had to tweak this to make it fit
vp <- viewport(x=0.08, y=0.18, width = w, height=h, just=c("left", "bottom"))

tmap_save(map2,filename="figures/yolo_gee_example.jpg",
          dpi=300, insets_tm=insetmap, insets_vp=vp,
          #height=asp2*7, 
          height=7,
          width=7, units="in")



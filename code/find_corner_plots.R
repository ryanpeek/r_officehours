# find corner plots


# load libraries ----------------------------------------------------------

library(smoothr)
library(sf)
library(tidyverse)
library(mapview)


# load data ---------------------------------------------------------------

dat <- read_rds("data/df.Rds")

# check types
table(dat$Landuse)

# add projected crs (UTM meters)
dat <- st_transform(dat, 24378)

# pull out roads as separate object and dissolve into single polygon
roads <- dat %>% filter(Landuse=="Road") %>% 
  st_union()

plots <- dat %>% filter(Landuse!="Road")

# quick map ---------------------------------------------------------------

library(mapview)
mapview(roads, col.regions="gray50") + mapview(plots, col.regions="maroon")

# simplify polygons -------------------------------------------------------

# drop the z values from the polygons and try first 2000 rows
p1 <- plots %>% st_zm() %>% 
  slice(1:2000)

# simplify the polygons
# this reduces the shape complexity and reduces file sizes
# but may not be helpful in this case
p1_s <- rmapshaper::ms_simplify(p1, 
                                keep = 0.4, # keep 40% of data
                                keep_shapes = TRUE)

# sample 4 points (1 per side assuming squareish shape)
# this may need subsetting to different plots that are non rectangular
p1_pts <- st_cast(p1, "LINESTRING") %>% 
  st_line_sample(n = 4, type = "regular")

# now buffer the mid points slightly and intersect with roads
intsx <- st_intersection(st_buffer(p1_pts, 5), roads) %>% 
  st_as_sf() %>% 
  #filter to only multipolygons (these are typically corner plots)
  dplyr::filter(st_is(., "MULTIPOLYGON")) %>% 
  rowid_to_column() %>% 
  rename(geometry=x)

# note, intsx is now all multipolygon, but these are small bubbles (1 meter in diam)
st_geometry_type(intsx)

# cast back to polygon to split out the multipart nature of things
intsx_pts <- intsx %>% st_zm() %>% st_cast("POLYGON") %>% 
  # now get the centroid and convert to points:
  st_centroid()
st_geometry_type(intsx_pts)

# now spatial join with the original polygons to see which plots are corners
p1_s_corners <- st_is_within_distance(p1_s, intsx_pts, dist = 5, sparse = FALSE) %>% apply(., 1, any)
# filter to the plots that are corners
p1_s_corner <- p1_s[p1_s_corners,]


# preview
mapview(intsx_pts, col.regions="yellow", legend=FALSE) +
  mapview(roads, col.regions="gray") +
  mapview(p1_pts, col.regions="maroon", cex=0.5) +
  mapview(p1_s, col.regions="#440154FF") +
  mapview(p1_s_corner, col.regions="yellow", legend=FALSE) 


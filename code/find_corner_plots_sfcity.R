# find corner plots


# load libraries ----------------------------------------------------------

library(smoothr)
library(sf)
library(tidyverse)
library(mapview)
library(tidycensus)


# load data ---------------------------------------------------------------

dat <- read_sf("~/Downloads/SF_LandUse/geo_export_0d13de99-a6d7-42fa-bac4-3e24566ba33c.shp")

# check types
table(dat$landuse)

# add projected crs (UTM meters)
dat <- st_transform(dat, 3310)

# plots
plots <- dat %>% filter(landuse=="RESIDENT")

# roads
# roads <- read_sf("~/Downloads/SanFrancisco-shp/shape/roads.shp") %>% 
#   st_transform(3310) %>% 
#   filter(type=="residential")

plot(plots$geometry, col="orange")
plot(roads$geometry, col="gray", add=T)

# roads
library(tigris)
roads <- roads(state = "CA", county = "San Francisco") %>% 
  st_as_sf() %>% 
  st_transform(3310) %>% 
  st_union()


# Census Track Stuff ------------------------------------------------------

# census tracts
racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")

sf_tracts <- get_decennial(geography = "block", year = 2010, cb = FALSE,
                           variables = racevars,summary_var = "P001001",
                           state = "CA", county = "San Francisco", geometry = TRUE)

sf_tracts <- sf_tracts %>% 
  mutate(pct = 100 * (value / summary_value)) %>% 
  filter(!is.na(pct))

sf_tracts %>% 
  ggplot(aes(fill = pct)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  coord_sf(crs = 3310) + 
  scale_fill_viridis_c()

mapview(sf_tracts %>% filter(variable=="Black"), zcol="pct")

# erase water?
st_erase <- function(x, y) {
  st_difference(x, st_union(y))
}

sf <- get_acs(geography = "tract", 
               variables = "B19013_001", 
               state = "CA", 
               county = "San Francisco", 
               geometry = TRUE, 
               cb = FALSE) 

sf_water <- area_water("CA", "San Francisco", class = "sf") 

sf_erase <- st_erase(sf, sf_water)

mapview(sf_erase, zcol = "estimate", legend = TRUE)

# quick map ---------------------------------------------------------------

mapview(roads, col.regions="gray50") + mapview(plots, col.regions="maroon")

# simplify polygons -------------------------------------------------------

# drop the z values from the polygons and try first 2000 rows
p1 <- plots %>% st_zm() %>% 
  slice(1:10000)

# simplify the polygons
# this reduces the shape complexity and reduces file sizes
# but may not be helpful in this case
p1_s <- rmapshaper::ms_simplify(p1, 
                                keep = 0.2, # keep 40% of data
                                keep_shapes = TRUE)

# Sample Points per PLot --------------------------------------------------

# sample 4 points (1 per side assuming squareish shape)
# this may need subsetting to different plots that are non rectangular
p1_pts <- 
  st_cast(p1, "POLYGON") %>%  
  st_cast(., "LINESTRING") %>% 
  st_line_sample(n = 4, type = "regular")

# now buffer the mid points slightly and intersect with roads
intsx <- st_intersection(st_buffer(p1_pts, 7), st_buffer(roads, 7)) %>% 
  st_as_sf() %>% 
  #filter to only multipolygons (these are typically corner plots)
  dplyr::filter(st_is(., "MULTIPOLYGON")) %>% 
  rowid_to_column() %>% 
  rename(geometry=x)

# plot
plot(p1$geometry, col="orange")
plot(roads$geometry, col="gray", add=T)
plot(p1_pts, col="black", add=T, pch=16, cex=0.3)

# note, intsx is now all multipolygon, but these are small bubbles (1 meter in diam)
st_geometry_type(intsx)

# cast back to polygon to split out the multipart nature of things
intsx_pts <- intsx %>% st_zm() %>% st_cast("POLYGON") %>% 
  # now get the centroid and convert to points:
  st_centroid()
st_geometry_type(intsx_pts)

# now spatial join with the original polygons to see which plots are corners
p1_s_corners <- st_is_within_distance(p1, intsx_pts, dist = 10, sparse = FALSE) %>% apply(., 1, any)

# filter to the plots that are corners
p1_s_corner <- p1[p1_s_corners,]

# preview
mapview(intsx_pts, col.regions="yellow", legend=FALSE) +
  mapview(roads, col.regions="gray") +
  mapview(p1_pts, col.regions="maroon", cex=0.5) +
  mapview(p1, col.regions="#440154FF") +
  mapview(p1_s_corner, col.regions="yellow", legend=FALSE) 


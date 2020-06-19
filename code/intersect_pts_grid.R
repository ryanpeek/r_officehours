# intersect points with grid

library(sf)
library(tidyverse)
library(mapview)

# load data:
pts <- read_csv("data/Example_point_data.csv") %>% 
  #drop a col
  select(-X1)

# change to sf object
pts <- st_as_sf(pts, coords=c("decimalLongitude","decimalLatitude"), remove=FALSE, crs=4326)

mapview(pts)

# make a boundary box based on points
bb1 <- st_make_grid(pts, n = 1)

# now make a grid based on the full extent
grid1 <- st_make_grid(bb1, cellsize = 0.5, square = TRUE)

# double check!
mapview(pts) + mapview(grid1, color="maroon", col.regions="transparent")

# now intersect or spatial join (see this good page as a reference)
mapview(grid1[pts, ])


# Libraries ---------------------------------------------------------------


library(choroplethrAdmin1)
library(sf)
#devtools::install_github("r-spatial/mapview@develop")
library(mapview)
library(dplyr)
library(ggplot2)


# Get Data ----------------------------------------------------------------

data(admin1.map)
class(admin1.map)

# filter to single country
afghan <- admin1.map %>% filter(admin == "afghanistan")

# only problem is this isn't an sf object class, it's a dataframe
class(afghan)

# convert to sf, by converting the points to sf
afghan_sf <- st_as_sf(afghan, coords = c("long", "lat"), remove = FALSE, crs=4326) %>% 
  # then grouping those points back into a polygon shape
  group_by(id, piece, admin, region) %>% 
  summarize(do_union=FALSE) %>% # aggregate by these groups
  st_cast("POLYGON") %>% # cast from point to a polygon type
  ungroup()

# check class again
class(afghan_sf)
st_crs(afghan_sf)

# now can do same ggplot, but using `geom_sf`
ggplot() + geom_sf(data=afghan_sf, aes(fill=region))


# Get More Data -----------------------------------------------------------

# Get geomapped ethnic groups here: https://icr.ethz.ch/data/epr/geoepr/

# read in the data and transform to same projection as above
epr_sf <- read_sf("~/Downloads/GeoEPR-2019/GeoEPR.shp") %>% 
  st_transform(st_crs(afghan_sf)) %>%
  # filter to only Afghanistan
  filter(statename=="Afghanistan") %>%
  # filter out the columns that have NA or same data across all rows
  select(-c(umbrella, sqkm, gwid, groupid))

# check geometry type
st_geometry_type(epr_sf) # so multiple types...may cause problems later

# check the area of each of these polygons:
st_area(epr_sf) %>% units::set_units(., km^2)

# so let's drop the polygons with zero area for now
epr_sf <- epr_sf %>% 
  mutate(sqkm = units::drop_units(st_area(.$geometry))) %>% 
  filter(sqkm > 0)

# quick map with mapview using group to see layers
mapview(epr_sf, zcol="group")


# Now Spatial Join --------------------------------------------------------

# see here: https://r-spatial.github.io/sf/reference/st_join.html

# check CRS are the same
st_crs(afghan_sf) == st_crs(epr_sf)

# spatial join by largest polygon (so largest overlap is what is assigned to the epr_sf group)...but many options (see above link)
largest <- st_join(epr_sf, afghan_sf[c("admin","region")], largest=TRUE)

mapview(largest, zcol="region", alpha.regions=0.8) + 
  mapview(afghan_sf, col.regions="transparent", alpha.regions=0.1,color="maroon")


# all possible combos using a left join
allcombos <- st_join(epr_sf, afghan_sf[c("admin","region")], largest=FALSE)

# look at how many admin regions occur within each group?
allcombos %>% st_drop_geometry() %>% 
  group_by(group) %>% 
  tally()

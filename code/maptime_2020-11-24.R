# maptime 2020-11-24
# r. peek

library(tidyverse) # wrangling and plotting
library(cowplot) # plotting multiple panels 
library(sf) # spatial anything
library(mapview) # interactive mapping
library(janitor) # cleaning names
library(tmap) # mapping 
library(tmaptools)
library(OpenStreetMap) # adding base layers to tmaps
library(magrittr) # for piping with %T>%
library(albersusa) # mapping US counties/boundaries
# install with remotes::install_git("https://git.sr.ht/~hrbrmstr/albersusa"))
# for global data, see library(rnaturalearth) 


# Import State Turkey Data ------------------------------------------------

# read in our turkey data!
# this comes from here: https://quickstats.nass.usda.gov

turkeys <- read_csv("https://raw.githubusercontent.com/ryanpeek/r_officehours/main/data/annual_turkey_data_by_state_USDA_1929-2020.csv")

turkeys <- read_csv("data/annual_turkey_data_by_state_USDA_1929-2020.csv")


# Tidy and Clean Data -----------------------------------------------------

table(turkeys$`Data Item`)


turkey_trimmed <- turkeys %>% 
  janitor::clean_names() %>% # fix column names
  select(year, state, state_ansi, data_item, value) %>% 
  # fix the value to be integer, and remove commas
  mutate(value=as.integer(gsub(pattern = ",", replacement = "", value)))

# rm NAs
turkey_trimmed <- turkey_trimmed %>% 
  filter(!is.na(value))


# let's recode this data_item column using case_when()
table(turkey_trimmed$data_item)

turkey_trimmed <- turkey_trimmed %>% 
  mutate(prod_type=case_when(
    data_item=="TURKEYS - PRODUCTION, MEASURED IN $" ~ "production_dollars",
    grepl(pattern="HEAD", data_item) ~ "production_head",
    grepl(pattern="LB", data_item) ~ "production_lbs"
  ))

# save back out as a clean data file we can use:
save(turkey_trimmed, file = "data/turkey_trimmed_us_1929-2020.rda") # .RData
load("data/turkey_trimmed_us_1929-2020.rda")

write_rds(turkey_trimmed, file = "data/turkey_trimmed_us_1929-2020.rds")
turkey_trimmed2 <- read_rds("data/turkey_trimmed_us_1929-2020.rds")

# rdata (rda) vs. rds
# many objects into a single Rdata file using save()
# with rds, you can only save one

# Get State Boundaries ----------------------------------------------------

library(albersusa)

us_comp <- usa_sf() # get a composite of USA
cnty_comp <- counties_sf() # composite counties

# plot:
plot(us_comp$geometry, border = "black", lwd=1)
plot(cnty_comp$geometry, border="gray", lwd=0.2, add=TRUE)

# write to geopackage:
st_write(us_comp,dsn = "data_output/map_base_layers.gpkg", layer = "us_composite", delete_layer = TRUE)
st_write(cnty_comp,dsn = "data_output/map_base_layers.gpkg", layer = "county_composite" , delete_layer = TRUE)

# check layers?
st_layers("data_output/map_base_layers.gpkg")


# Join Data ---------------------------------------------------------------

# how to read in a layer from a geopackage
us_comp <- read_sf(dsn="data_output/map_base_layers.gpkg", layer="us_composite")
cnty_comp <- st_read(dsn = "data_output/map_base_layers.gpkg", "county_composite")

# join by state names
turkey_states <- left_join(turkey_trimmed, us_comp, by=c("state_ansi"="fips_state")) %>% 
  st_as_sf()

# use mapview to make an interactive map to check:
t2019 <- turkey_states %>% 
  filter(year==2019, prod_type=="production_dollars")

# make map
mapview(t2019, zcol="value")


# Read in County Data for CA Turkeys --------------------------------------

ca_cntys <- cnty_comp %>% dplyr::filter(iso_3166_2=="CA")

mapview(ca_cntys)

# get turkey ca data and clean
ca_turkey <- read_csv("https://raw.githubusercontent.com/ryanpeek/r_officehours/main/data/chicken_turkey_CA_usda.csv") %>% 
  clean_names() %>% 
  filter(geo_level=="COUNTY") %>% 
  select(-c(week_ending, zip_code:watershed, 
            domain, domain_category, cv_percent)) %>% 
  filter(grepl("TURKEY", commodity),
         data_item=="TURKEYS - SALES, MEASURED IN HEAD") %>% 
  filter(!grepl("(D)", value)) %>% 
  mutate(value=as.integer(gsub(",", replacement = "", value)))

# join with counties spatial (sf)
ca_cnty_turkey <- left_join(ca_turkey, ca_cntys, 
                            by=c("county_ansi"="county_fips")) %>%
  st_as_sf()

class(ca_cnty_turkey)

# filter to just 2017
ca_cnty_turkey17 <- ca_cnty_turkey %>% filter(year==2017)

# mapview
mapview(ca_cnty_turkey17, zcol="value", layer.name="Head of Turkey")


# tmap mapping ------------------------------------------------------------

tmap_mode("plot") # plot=static, view=mapview style dynamic map

# get baselayer for ca_cntys (change zoom as needed)
osm_ca_cnty <- read_osm(ca_cntys, ext=1.1)

# pick two years
ca_cnty_turkey2 <- ca_cnty_turkey %>% filter(year %in% c(1997, 2017))

# tmap
m1 <- # need tm_shape first
  #tm_shape(osm_ca_cnty) + tm_rgb() +
  tm_shape(ca_cntys) + tm_polygons(border.col = "gray10", border.alpha = 0.1) +
  tm_shape(ca_cnty_turkey2) +
  tm_polygons(lwd=0.2, border.col = "gray", col="value", title="No. of Turkeys") +
  tm_text("county", size=0.5) +
  tm_facets(by="year", drop.empty.facets = TRUE) +
  tm_layout(frame=FALSE, legend.outside = FALSE, legend.bg.color = "white",
            legend.bg.alpha = 0.8, legend.width = 0.5, 
            legend.position = c(0.6, 0.8)) +
  tm_scale_bar(width=0.2, position=c('left', 'bottom')) +
  tm_compass(type="arrow")

m1

tmap::tmap_save(m1, filename = "figures/tmap_turkey_facet.png", 
                width = 11, height = 8.5, units = "in", dpi = 300)  



# California Enviroscreen Data --------------------------------------------


# https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30

# let's look at layers in this File Geodatabase
# data here: https://github.com/ryanpeek/r_officehours/blob/main/data/CES3_June2018update.gdb.zip

st_layers("data/CES3_June2018update.gdb/")

# read in as above:
ces <- st_read(dsn = "data/CES3_June2018update.gdb/", layer="CES3_June2018updateGDB")

st_crs(ces) # 3310
st_crs(ca_cnty_turkey17) # 4326

# transform our turkey data
ca_cnty_turkey <- st_transform(ca_cnty_turkey17, crs=3310)
st_crs(ca_cnty_turkey)

# mapview of asthma percentiles
mapview(ces, zcol="asthmaP")

# dissolve/aggregate
ces_asthma <- ces %>% group_by(California_County) %>% 
  summarize(asthma_median = median(asthmaP)) %>% 
  st_cast()

#
mapview(ces_asthma, zcol="asthma_median")

# drop the geometry, and join with another
ces_asthma <- st_drop_geometry(ces_asthma)
class(ces_asthma)

# JOIN
ces_asthma_cnty <- left_join(ca_cntys, ces_asthma, by=c("name"="California_County"))

# WHAT@!? Where's all the data?
mapview(ces_asthma_cnty, zcol="asthma_median")

## empty spaces...let's fix:
ces_asthma <- ces_asthma %>% 
  mutate(ca_county = stringr::str_squish(California_County))


# rejoin
ces_asthma_cnty <- left_join(ca_cntys, ces_asthma, by=c("name"="ca_county"))

mapview(ces_asthma_cnty, zcol="asthma_median")


# Make ggplot Side by Side Map --------------------------------------------


# first the asthma map
g1 <- ggplot() + 
  # asthma
  geom_sf(data=ces_asthma_cnty, aes(fill=asthma_median))+
  scale_fill_viridis_c("CES Asthma \n Median Percentile")+
  coord_sf(datum=NA) + # removes the gridlines when plotting spatial data with sf
  ggspatial::annotation_scale() +
  labs(caption = "Data from CalEnviroscape", 
       subtitle = "Correlation between turkeys and asthma") +
  theme_classic() +
  theme(legend.position = c(0.7, 0.8),
        legend.background = element_rect(fill="NA"))

g1

# turkey map
g2 <- ggplot() + 
  # turkeys
  geom_sf(data=ca_cntys, fill=NA)+
  geom_sf(data=ca_cnty_turkey2 %>% filter(year==2017), 
          aes(fill=value))+
  scale_fill_viridis_c("Number of Turkeys", option = "A")+
  coord_sf(datum=NA) +
  ggspatial::annotation_north_arrow(location="tr",
                                    height = unit(1.2,"cm"), 
                                    width = unit(0.9, "cm"),
                                    pad_x = unit(0.8, "cm"),
                                    pad_y = unit(0.8, "cm")) +
  labs(caption = "Data from USDA") +
  theme_classic() +
  theme(legend.position = c(0.7, 0.8),
        legend.background = element_rect(fill="NA"))
g2

# cowplot package is good for combining plots
combined_plot <- cowplot::plot_grid(g1, g2, align="h", 
                                    nrow=1, label_y=0.9, labels="AUTO")

combined_plot

ggsave(combined_plot, 
       filename = "figures/correlation_btwn_turkeys_asthma.jpg", 
       width = 11, height = 8, dpi=300, units = "in")

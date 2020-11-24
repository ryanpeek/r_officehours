###### A TUTORIAL IN MAPPING AND SPURIOUS CORRELATION
###### R. Peek, 2020

# Load Libraries ----------------------------------------------------------

library(tidyverse) # wrangling and plotting
library(cowplot) # plotting multiple panels 
library(sf) # spatial anything
library(mapview) # interactive mapping
library(janitor) # cleaning names
library(tmap) # mapping 
library(OpenStreetMap) # adding base layers to tmaps
library(magrittr) # for piping with %T>%
library(albersusa) # mapping US counties/boundaries
# install with remotes::install_git("https://git.sr.ht/~hrbrmstr/albersusa"))
# for global data, see library(rnaturalearth)

# Import State Turkey Data ------------------------------------------------

# read in our turkey data!
# this comes from here: https://quickstats.nass.usda.gov

turkeys <- read_csv("https://raw.githubusercontent.com/ryanpeek/r_officehours/main/data/annual_turkey_data_by_state_USDA_1929-2020.csv")

# turkeys <- read_csv("data/annual_turkey_data_by_state_USDA_1929-2020.csv")

# load("data/turkey_trimmed_us_1929-2020.rda")
# load(url("https://raw.githubusercontent.com/ryanpeek/r_officehours/main/data/turkey_trimmed_us_1929-2020.rda"))

# Tidy and Clean Data -----------------------------------------------------

# review data: Data Items are different production types
table(turkeys$`Data Item`)

# make a clean dataset with the data we need to map and fix names
turkey_trimmed <- turkeys %>%
  janitor::clean_names() %>% # fixes terrible names
  select(year, state, state_ansi, data_item, value) %>% 
  # fix the value column: has commas and is character
  mutate(value=as.integer(gsub(pattern = ",", replacement = "", value))) %>%
  # warning about NA's is ok! drop the NA values
  filter(!is.na(value))


# let's re-code awkward data_item categories with case_when()
turkey_trimmed <- turkey_trimmed %>%
  mutate(prod_type=case_when(
    data_item=="TURKEYS - PRODUCTION, MEASURED IN $" ~ "production_dollars",
    data_item=="TURKEYS - PRODUCTION, MEASURED IN HEAD" ~ "production_head",
    # here's a way to use grep to search for any thing with "LB" in it
    grepl(pattern = "LB", data_item) ~ "production_lbs"
  ))

# save back out as a clean data file we can use:
save(turkey_trimmed, file = "data/turkey_trimmed_us_1929-2020.rda")


# Get State Boundaries ----------------------------------------------------

library(albersusa)
us_comp <- usa_sf() # get a composite of USA
cnty_comp <- counties_sf() # composite counties

# plot:
plot(us_comp$geometry, border = "black", lwd=1)
plot(cnty_comp$geometry, border="gray", lwd=0.2, add=T)

# write to geopackage:
st_write(us_comp,dsn = "data_output/map_base_layers.gpkg", layer = "us_composite", delete_layer = TRUE)
st_write(cnty_comp,dsn = "data_output/map_base_layers.gpkg", layer = "county_composite" , delete_layer = TRUE)

# check layers?
st_layers("data_output/map_base_layers.gpkg")


# Join Data ---------------------------------------------------------------

# now we can join by state name
turkey_states <- left_join(turkey_trimmed, us_comp, by=c("state_ansi"="fips_state")) %>%
  # and make a sf object...works because of sticky geometry column
  st_as_sf()


# Now Map! ----------------------------------------------------------------

# mapview to make a quick map:
t2019 <- turkey_states %>%
  filter(year==2019, prod_type=="production_dollars")

# make an interactive map
mapview(t2019, zcol="value")

# Visualize ---------------------------------------------------------------

# first get the top 3 per year
turkey_t3_dollars <- turkey_states %>%
  st_drop_geometry() %>% 
  filter(!is.na(name)) %>% 
  select(name, prod_type, value, year) %>% 
  filter(prod_type=="production_dollars") %>% 
  group_by(prod_type, year) %>%
  slice_max(order_by = c(value, name), n=3)  

# then group and tally by state
turkey_t3_dollars <- turkey_t3_dollars %>% group_by(name) %>% 
  add_tally() %>% rename(tot_top3=n) %T>% # this allows us to keep object but also pipe to VIEW
  View()

# top three states?
turkey_top3 <- 
  turkey_t3_dollars %>% 
  ungroup %>% 
  select(-value, -year) %>% 
  distinct(name, .keep_all = T) %>% 
  slice_max(n=3, order_by=tot_top3) %T>% 
  View()


# make a plot of just production by top 3
ggplot() +
  geom_line(
    data=turkey_states %>% 
      filter(name %in% turkey_top3$name),
    aes(x=year, y=value, color=name, group=name), 
    show.legend = T) +
  labs(y="Value", x="") +
  scale_color_viridis_d("State", direction = -1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=c(seq(1930, 2020, 10)))+
  facet_grid(prod_type~., scales = "free_y") +
  theme_minimal()


# Read in COUNTY Data for Turkey in CA ------------------------------------

ca_turkey <- read_csv("data/chicken_turkey_CA_usda.csv") %>% 
  clean_names() %>% 
  filter(geo_level=="COUNTY") %>% 
  select(-c(week_ending, zip_code:watershed, 
            domain, domain_category, cv_percent)) %>% 
  filter(grepl("TURKEY", commodity),
         data_item=="TURKEYS - SALES, MEASURED IN HEAD") %>% 
  filter(!grepl("(D)", value)) %>% 
  mutate(value=as.integer(gsub(",", replacement = "", value)))

# map and join to CA counties


# TRIM CA COUNTIES --------------------------------------------------------

ca_cntys <- cnty_comp %>% dplyr::filter(iso_3166_2=="CA")

ca_cnty_turkey <- left_join(ca_turkey, cnty_comp %>% 
                              filter(iso_3166_2=="CA"), 
                            by=c("county_ansi"="county_fips")) %>% 
  st_as_sf()

# this includes all years though
ca_cnty_turkey17 <- ca_cnty_turkey %>% filter(year==2017)
mapview(ca_cnty_turkey17, zcol="value", layer.name="Head of Turkey") 

table(ca_cnty_turkey$year)

# make faceted map of 1997 and 2017 turkey production in CA
tmap_mode("plot")

# get baselayer for ca_cntys
library(OpenStreetMap)
osm_ca_cnty <- read_osm(ca_cntys, ext=1.1)

# pick two years
ca_cnty_turkey2 <- ca_cnty_turkey %>% filter(year %in% c(1997, 2017))

m1 <- 
  tm_shape(osm_ca_cnty) + tm_rgb() +
  tm_shape(ca_cntys) +
  tm_polygons(border.col = "gray10", border.alpha = 0.1) +
  tm_shape(ca_cnty_turkey2) + 
  tm_polygons(lwd = 0.2, border.col = "gray", col = "value", 
              title="No. of Turkeys") +
  tm_text("county", size = 0.5)+
  tm_facets(by = "year", drop.empty.facets = TRUE) +
  tm_layout(frame = FALSE, legend.outside = F, fontfamily = "Roboto", 
            legend.bg.color = "white", legend.bg.alpha = 0.8,
            legend.width = .4,
            #legend.format = list(format="f"),
            legend.position = c(0.6, 0.8))+
  tm_scale_bar(width = 0.2, position = c("left","bottom")) +
  tm_compass(type = "arrow")
m1


# Read in CalEnviroscreen Data ---------------------------------------------

st_layers("data/CES3_June2018update.gdb/")
ces <- st_read(dsn = "data/CES3_June2018update.gdb/", layer="CES3_June2018updateGDB")
st_crs(ces) # 3310
st_crs(ca_cnty_turkey17) # 4326

mapview(ces, zcol="asthmaP")
mapview(ces, zcol="pmP")


# DISSOLVE
# let's aggregate up to County level for Asthma scores and compare with turkey farming
ces_asthma <- ces %>% group_by(California_County) %>% 
  summarize(asthma_mean = median(asthmaP)) %>% 
  st_cast()
# takes a second
mapview(ces_asthma, zcol="asthma_mean") # a bit messy...let's use the clean counties layer

# drop geometry
ces_asthma <- st_drop_geometry(ces_asthma)

# JOIN
ces_asthma_cnty <- left_join(ca_cntys, st_drop_geometry(ces_asthma), by=c("name"="California_County"))

# WHAT@!?
mapview(ces_asthma_cnty, zcol="asthma_mean")

# empty spaces...let's fix:
ces_asthma <- ces_asthma %>% 
  mutate(ca_county = stringr::str_squish(California_County))

ces_asthma_cnty <- left_join(ca_cntys, ces_asthma, by=c("name"="ca_county"))
mapview(ces_asthma_cnty, zcol="asthma_mean")

# NOW A SIDE BY SIDE MAP WITH TURKEYS!!

# SIDE BY SIDE MAP! -------------------------------------------------------

# First, make Asthma Map:

g1 <- ggplot() + 
  # asthma
  geom_sf(data=ces_asthma_cnty, aes(fill=asthma_mean))+
  scale_fill_viridis_c("CES Asthma \n Median Percentile")+
  coord_sf(datum=NA) +
  ggspatial::annotation_scale() +
  labs(caption = "Data from CalEnviroscape", subtitle = "Correlation between turkeys and asthma") +
  theme_classic() +
  theme(legend.position = c(0.7, 0.8),
        legend.background = element_rect(fill="NA"))

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

(combined_plot <- cowplot::plot_grid(g1, g2, align = "h", nrow=1, label_y = 0.9, labels = "AUTO"))

ggsave(combined_plot, filename = "figures/correlation_btwn_turkeys_asthma.jpg", width = 11, height = 8, dpi=300, units = "in")


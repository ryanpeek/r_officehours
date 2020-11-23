# turkey maps

# Data from here: https://quickstats.nass.usda.gov


# Load Libraries ----------------------------------------------------------

# first load libraries
library(tidyverse)
library(sf)
library(USAboundaries)
library(mapview)
library(janitor)


# Import Data -------------------------------------------------------------

# read in our turkey data!
#turkeys <- read_csv("https://raw.githubusercontent.com/ryanpeek/cws_code_club/main/data/annual_turkey_data_by_state_USDA_1929-2020.csv")

turkeys <- read_csv("data/annual_turkey_data_by_state_USDA_1929-2020.csv")


# Tidy and Clean Data -----------------------------------------------------

# let's take a look at the different variables:
# # this is lbs and $ of turkey for each state for each year in the US

# review data
table(turkeys$`Data Item`)
summary(turkeys)

# make a clean dataset with the data we need to map and fix names
turkey_trimmed <- turkeys %>%
  select(Year, State, `State ANSI`, `Data Item`, Value) %>%
  janitor::clean_names() %>%
  # fix the value (remove commas and make integer)
  mutate(value=as.integer(gsub(pattern = ",", replacement = "", value))) %>%
  # warning about NA's is ok! drop the NA values
  filter(!is.na(value))


# let's recode awkward data_item categories with case_when()
turkey_trimmed <- turkey_trimmed %>%
  mutate(prod_type=case_when(
    data_item=="TURKEYS - PRODUCTION, MEASURED IN $" ~ "production_dollars",
    data_item=="TURKEYS - PRODUCTION, MEASURED IN HEAD" ~ "production_head",
    # here's a way to use grep to search for any thing with "LB" in it
    grepl(pattern = "LB", data_item) ~ "production_lbs"
  ))


# Get State Boundaries ----------------------------------------------------

# many spatial datasets exist...for US this library is good.
# for global data, see library(rnaturalearth)
# also see albersusa (remotes::install_git("https://git.sr.ht/~hrbrmstr/albersusa"))
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

ca_cntys <- cnty_comp %>% filter(iso_3166_2)

ca_cnty_turkey <- left_join(ca_turkey, cnty_comp %>% 
                              filter(iso_3166_2=="CA"), 
                            by=c("county_ansi"="county_fips")) %>% 
  st_as_sf()

# this includes all years though
ca_cnty_turkey17 <- ca_cnty_turkey %>% filter(year==2017)
mapview(ca_cnty_turkey17, zcol="value", layer.name="Head of Turkey") 

table(ca_cnty_turkey$year)

# make faceted map of each year of turkey production in CA
library(tmap)
library(tmaptools)

tmap_mode("plot")

m1 <- tm_shape(ca_cntys) +
  tm_polygons(border.col = "gray10", border.alpha = 0.1) +
  tm_shape(ca_cnty_turkey) + 
  tm_polygons(lwd = 0.2, border.col = "gray", col = "value", 
              title="No. of Turkeys") +
  #tm_facets(along = "year", free.scales.fill = FALSE, free.coords = FALSE) + # for animation
  tm_facets(by = "year", ncol = 2, nrow = 3, free.scales.fill = FALSE, drop.empty.facets = T) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.position = c(-0.75, 0.05))+
  tm_scale_bar(width = 0.2) +
  tm_compass()
m1

#tmap_animation(m1, delay=70)

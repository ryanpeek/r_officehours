
# Packages ----------------------------------------------------------------

library(dplyr)
library(sf)

# Data --------------------------------------------------------------------

# use river layer Ensi sent: (entire US rivers as sp): "lines.rivers"
load(url("http://sharpsightlabs.com/wp-content/datasets/usa_rivers.RData"))


# Convert to sf format (simple features) ----------------------------------
# sf is a smaller and tidier spatial format

# convert to sf
rivs_sf <- as(lines.rivers, "sf")

# check obj sizes:
library(pryr)
object_size(rivs_sf) # 78.7 MB (so half the size)
object_size(lines.rivers) # 133 MB



# FILTER ------------------------------------------------------------------


# now filter to only CA, NV, and OR: (or whatever states you want)
rivs_ca_sf <- filter(rivs_sf, STATE %in% c("CA", "NV", "OR"))

# quick check:
plot(rivs_ca_sf$geometry, col="blue", lwd=0.3)

# find out the diff TYPES
table(rivs_named_sf$FEATURE)

# filter to only the "named" rivers things that are "Stream"
rivs_named_sf <- rivs_ca_sf %>% 
  filter(!is.na(NAME), FEATURE %in% c("Stream")) %>% 
  st_set_crs(4326)

# plot again
plot(rivs_named_sf$geometry, col="steelblue", lwd=0.2)


# ADD STATES --------------------------------------------------------------

# probably a few packages that do this, I like this one:
library(USAboundaries)

states <- us_states(states=c("CA","NV","OR"))

# update plot
plot(states$geometry, border="gray50", lwd=1, lty=2)
plot(rivs_named_sf$geometry, col="blue", lwd=0.3, add=T)



# FILTER TO SPECIFIC RIVERS -----------------------------------------------

# if you want to filter to a specific river or rivers, use the same "filter" tool....you can also crop by the extend or boundary box of your sites (see sf::st_intersection)

sacramento <- filter(rivs_named_sf, grepl("Sacramento", NAME))

# plot again
plot(states$geometry, border="gray50", lwd=1, lty=2)
plot(rivs_named_sf$geometry, col="blue", lwd=0.3, add=T)
plot(sacramento$geometry, col="maroon", lwd=3, add=T)


# save it back out:
save(sacramento, rivs_named_sf, rivs_ca_sf, states, file = "data/rivers_example_data.rda")


# PLOT WITH GGPLOT --------------------------------------------------------

# to plot with ggplot
library(ggplot2)

ggplot()+ geom_sf(data=states, fill="transparent", color="gray40", lwd=0.8, lty=2) +
  geom_sf(data=rivs_named_sf, color="blue2", lwd=0.25) +
  geom_sf(data=sacramento, color="maroon", alpha=0.5, lwd=2)+
  theme_void()



# PLOT WITH TMAP ----------------------------------------------------------

library(tmap)
library(tmaptools)

tm_shape(states) + tm_borders(lwd=1, col = "gray", lty=2) +
  tm_shape(rivs_named_sf) + tm_lines(col="blue2", lwd=0.25) +
  tm_compass(position = c("left","bottom")) +
  tm_scale_bar(position = c("left","bottom"))

tmap::tmap_save(filename = "figures/demo_rivers_plot.png", width = 8, height = 11, units = "in", dpi = 200)

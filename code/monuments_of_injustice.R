# code to look at confederate monuments
# from @hrbrmstr: https://twitter.com/hrbrmstr/status/1271057843639259136?s=20

library(googlesheets4); 
library(tidylog)
library(albersusa); 
library(hrbrthemes); 
library(sf); 
library(tidyverse); 
gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets.readonly"); 

# get sheet
xdf <- range_read("17ps4aqRyaIfpu7KdGsy2HRZaaQiXUfLrpUbaR9yS51E",sheet=2); 
# xOpen <- googlesheets4::gs4_browse("17ps4aqRyaIfpu7KdGsy2HRZaaQiXUfLrpUbaR9yS51E")
xdf <- janitor::clean_names(xdf); 


glimpse(xdf); 

select(xdf,unique_id, coordinates,symbol_type, was_this_symbol_removed, location, state, feature_name) %>% 
  # fix missing comma:
  mutate(coordinates = if_else(unique_id==722, "31.568502, -89.838409", coordinates)) %>% 
  separate(coordinates,into=c("lat","lng"), sep=",", convert=TRUE) %>%  
  st_as_sf(coords=c("lng","lat"),crs=4326, remove=FALSE) -> confed; 

summary(confed)


count(confed, symbol_type,sort=TRUE); 

confed %>% 
  mutate(symbol_type=case_when(
    grepl("ghway",symbol_type) ~ "Roadway", 
    grepl("onum",symbol_type) ~ "Monument", 
    grepl("ool",symbol_type) ~ "School", 
    TRUE ~ symbol_type ) ) %>% 
    mutate(symbol_type=fct_infreq(symbol_type) ) -> confed; 

usa <- usa_sf(); 

# remove the "removed" symbols:
confed %>% filter(was_this_symbol_removed=="Active") -> confed

confed <- points_elided_sf(confed); 

ggplot() + 
  geom_sf(data=usa,size=0.125,fill="#f9f9f9") + 
  geom_sf(data=confed,aes(color=symbol_type),size=0.5 ) + 
  ggthemes::scale_color_tableau(name=NULL,"Tableau 20") + 
  coord_sf(crs=us_aeqd_proj,datum=NA) + 
  theme_ipsum_es(grid = " ") +
  labs(caption="source: https://twitter.com/hrbrmstr/status/1271057843639259136?s=20")

library(mapview)
mapview(confed, zcol="symbol_type")

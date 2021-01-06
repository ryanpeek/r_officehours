# 2021: get stream FFM 

#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library(ffcAPIClient)
library(tidyverse)
library(glue)
library(janitor)
library(sf)
library(nhdplusTools)
options(scipen = 100) # to print full string instead of sci

# set/get the token for using the FFC
ffctoken <- set_token(Sys.getenv("EFLOWS", ""))

# clean up
ffcAPIClient::clean_account(ffctoken)

library(fs)
library(here)
library(beepr) # to know when done!

# Get Data ----------------------------------------------------------------

xy <- read_csv("data/xy4flow.csv") %>% clean_names() %>% 
  select(-1)


# Get COMIDs --------------------------------------------------------------

type <- "start"

# TRANSFORM TO 3310
xystart <- xy %>% st_as_sf(coords=c("start_coord_x", "start_coord_y"), crs=4326, remove=FALSE) %>%   distinct(filter_id, .keep_all = TRUE) %>%
  st_transform(crs = 3310) # use CA Teale albs metric

#  Create dataframe for looking up COMIDS
segs <- xystart %>%
  mutate(comid=NA)

# use nhdtools to get comids
comids <- segs %>%
  group_split(filter_id) %>%
  set_names(x = ., nm = segs$filter_id) %>%
  map(~discover_nhdplus_id(.x$geometry))

# flatten into single dataframe instead of list
segs_df <- comids %>% flatten_dfc() %>% t() %>%
  as.data.frame() %>%
  rename("comid"=V1) %>% rownames_to_column(var = "filter_id")

# check for na?
summary(segs_df)

# how many negatives?
filter(segs_df, comid<0) %>% count()

# clean up
rm(comids, segs)

# join data back to original dataframe
xy_list_coms <- left_join(xystart, segs_df, by=c("filter_id"))

# save it back out
write_rds(xy_list_coms, file = glue("data_output/xy_{type}_comids.rds"))


# Get Predicted Flow Metrics from COMID --------------------------------------------

# https://ceff-tech.github.io/ffc_api_client/articles/getting_started.html#getting-predicted-flow-metrics-from-a-comid

ffcAPIClient::get_predicted_flow_metrics(segs_df$comid[1])

ffcs <- segs_df %>% 
  group_split(filter_id) %>%
  map(~get_predicted_flow_metrics(comid = .x$comid))
beep(2)

# unnest the list
ffc_out <- ffcs %>% bind_rows() %>% 
  # remove duplicates
  group_by(comid) %>% 
  distinct(.keep_all = TRUE)

# join back to original data set:
dat_out <- left_join(segs_df, ffc_out, by="comid")

# add the XY start point too to map
dat_out <- left_join(dat_out, xystart, by=c("filter_id")) %>% st_sf()

# quick map
mapview::mapview(dat_out)

# save out
write_csv(st_drop_geometry(dat_out), file = "data_output/ffc_data_edna_sites.csv")

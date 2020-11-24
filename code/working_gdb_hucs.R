# get WBD hucs

# some reason can't read gdb if not on main mounted drive using SF (won't work with external drive or SD card?)

# copied from here: "/Volumes/RAP200/nhdplus_seamless"

st_layers("~/Desktop/WBD_dataset.gdb")
wbd <- st_read("~/Desktop/WBD_dataset.gdb", "WBDSnapshot_National")

wbd <- st_read("~/Desktop/WBD_dataset.gdb", "WBDSnapshot_National")
wbd_ca <- wbd %>% filter(STATES=="CA")
save(wbd_ca, file = "data/wbd_ca.rda")

# dissolve layer by an attribute
h10 <- wbd_ca %>% group_by(HUC_10) %>%
  summarize() %>% st_cast()

# add previous attributes
h10j <- wbd_ca %>% st_drop_geometry() %>% select(1:3, 5, 9:12, 17:18, 21:23) %>% 
  distinct(HUC_10,.keep_all = TRUE)
huc10 <- left_join(h10, h10j, by="HUC_10")

mapview(huc10, zcol="HUC_10")

# save
save(huc10, file = "data/ca_huc10_wbd.rda")


# dissolve layer by an attribute
h12 <- wbd_ca %>% group_by(HUC_12) %>%
  summarize() %>% st_cast()

# add previous attributes
h12j <- wbd_ca %>% st_drop_geometry() %>% 
  distinct(HUC_12,.keep_all = TRUE)
huc12 <- left_join(h12, h12j, by="HUC_12")

mapview(huc12, zcol="HUC_12")

# save
save(huc12, file = "data/ca_huc12_wbd.rda")


# dissolve layer by an attribute
h8 <- wbd_ca %>% group_by(HUC_8) %>%
  summarize() %>% st_cast()

# add previous attributes
h8j <- wbd_ca %>% st_drop_geometry() %>% select(1:2, 5:6, 17:18, 21:23) %>% 
  distinct(HUC_8,.keep_all = TRUE)
huc8 <- left_join(h8, h8j, by="HUC_8")

mapview(huc8, zcol="HUC_8")

# save
save(huc8, file = "data/ca_huc8_wbd.rda")


# Look at All CA Data -----------------------------------------------------

st_layers("~/Desktop/ca_db/NHD_H_California_State_GDB.gdb/")
db <- "~/Desktop/ca_db/NHD_H_California_State_GDB.gdb"

flowlines <- st_read(db, "NHDFLowline")
h8 <- read_sf(db, "WBDHU8")
st_drivers("vector") %>% filter(write==TRUE)
mapview(h8)

# best compression is "xz" rds
write_rds(h8, file = "~/Desktop/h8_ca.rds", compress = "xz")

# get Shasta
h8_lsh <- h8 %>% filter(grepl("Shasta", Name))
write_rds(h8_lsh, file = "~/Documents/github/wcb_LS_flows/data_output/huc8_shasta.rds")


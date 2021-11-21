# Open Refine

#devtools::install_github("vpnagraj/rrefine")
library(rrefine)

head(lateformeeting)

# save out example data
write.csv(lateformeeting, file = "data/lateformeeting.csv", row.names = FALSE)

# upload to refine!
refine_upload(file = "data/lateformeeting.csv", project.name = "lfm_cleanup", open.browser = TRUE)

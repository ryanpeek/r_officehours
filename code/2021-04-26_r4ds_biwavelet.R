library(biwavelet)
library(zoo)
library(forecast)

site <- read.csv("data/biwavelet_Data.csv", sep = ",")

# make the date from factor to date
site$date <- as.Date(site$date, format = "%Y-%m-%d", tz = "Asia/Kolkata")
summary(site$value)

# interpolate over NA values and add back to orig dataset
site$value_interp <- forecast::na.interp(site$value) # defaults to TS

# make julian day seq
site$julian <- julian(site$date, site$date[1])

# make wavlet matrix
data <- cbind(site$julian, as.numeric(site$value_interp))

# perform wavelet transform
wt.t1 <- wt(data, dt = 1)

# plot it
plot.biwavelet(wt.t1, xlab = "Year", main = "plot",
               cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, ncol = 64)

## Identifying hydrograph peaks?

library(tidyverse)
library(lubridate)
library(ggrepel)
library(viridis)
library(caTools)
source("scripts/functions/f_doy.R")
source("scripts/functions/f_USGS_get_raw_dat.R")

# LOAD DATA ---------------------------------------------------------------

#load("data/flow_dv_cfs_full_6sites.rda")
load("data/flows/NFA_dv_USGS_11426500_1911_1941.rda")
master_df <- filename

cfs_cms <- function(cfs){
  cfs * 0.028316847 # cfs * cms
}

df <- master_df %>% filter(month(date)>3 & month(date)<8)

# bimonthly
dowy_labs1<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Mar-15", "Apr-01", "Apr-15", "May-01", "May-15", "Jun-01", "Jun-15", "Jul-01", "Jul-15", "Aug", "Aug-15", "Sep")
dowy_breaks<-c(1, 32, 62, 93, 124, 152, 167, 183, 198, 213, 228, 244, 259, 274, 289, 305, 320, 336)

# Monthly
dowy_labs2<-c("Oct","Nov","Dec","Jan", "Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
dowy_breaks2<-c(1, 32, 62, 93, 124, 152, 183, 213, 244, 274, 305, 336)


# ANNUAL MEAN -------------------------------------------------------------


# summarize data with 5/95
nfa_sum <- df  %>% 
  group_by(DOWY) %>% 
  summarize(meanAnnQ=mean(flow_cfs, na.rm = T),
            q95=quantile(flow_cfs, probs=c(0.95)),
            q05=quantile(flow_cfs, probs=c(0.05))) %>% 
  mutate(meanAnnQ_cms = cfs_cms(meanAnnQ),
         q95_cms = cfs_cms(q95),
         q05_cms = cfs_cms(q05))


# PEAK IDENTIFICATION -----------------------------------------------------

riv <- df %>% filter(WY==1935)
riv$flow_cfs[is.na(riv$flow_cfs)]<-50 # replace NAs with baseline


# KernSmooth
library(KernSmooth)

(gg11<-ggplot() + geom_line(data=riv, 
                     aes(x=DOWY, y=flow_cfs), color="black", lwd=1, show.legend = F) + 
  scale_x_continuous(breaks=dowy_breaks2, labels=dowy_labs2) +
  theme_classic(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        legend.key=element_blank(),
        axis.text.y = element_text(hjust = 1, size=12))+labs(x=""))

# baseplot
with(riv, plot(DOWY, flow_cfs, typ="l", col="black", lwd=1.5))

# get fit of line VERY FINE SCALE
#fit <- as.data.frame(locpoly(riv$DOWY, riv$Q_cfs, bandwidth = 0.25))
#lines(fit, col="red",lty=2)

# get fit of line SMOOTHED
fit2 <- as.data.frame(locpoly(riv$DOWY, riv$flow_cfs, bandwidth = 3))
lines(fit2, col="darkgreen",lty=4, lwd=2)

# calc difference in fit
#dsmooth<-diff(fit$y)
dsmooth2<-diff(fit2$y)

# calc local maxima using sign
#locmax<-sign(c(0,dsmooth))>0 & sign(c(dsmooth,0))<0
#points(fit$x[locmax],fit$y[locmax],cex=3,c=2)    
locmax2<-sign(c(0,dsmooth2))>0 & sign(c(dsmooth2,0))<0
points(fit2$x[locmax2],fit2$y[locmax2],cex=2,col="purple", pch=16)    
# make local maxima into tibble
locmaxes<-tibble(x=fit2$x[locmax2], "y"=fit2$y[locmax2])

# plot last 2
gg11 + #ylim(c(0,6000)) + 
  geom_point(data=locmaxes[c(nrow(locmaxes)-1, nrow(locmaxes)),], aes(x=x, y=y), fill="purple", pch=21, size=5)

# plot all
gg11 + ylim(c(0,1e4)) + 
  geom_point(data=locmaxes, aes(x=x, y=y), fill="purple", pch=21, size=5)

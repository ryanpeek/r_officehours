
# Read in Data ------------------------------------------------------------

library(tidyverse)
raw_data <- "ISO3 year DV emp fam edu
AGO 2015 0.6958275 0.25766926 0.2196814 0.19060157
BDI 2010 0.6458187 0.03167923 0.3991930 0.27182102
BDI 2016 0.6916594 0.30668319 0.4871276 0.42081000
BEN 1996 0.5501811 0.85316046 0.4003255 0.05189475
BEN 2001 0.5702534 0.73140387 0.4003370 0.07038437
BEN 2006 0.6075056 0.65812225 0.3743346 0.08480554
BEN 2011 0.7490880 0.37057324 0.3435336 0.12785360
BEN 2017 0.7152615 0.46780035 0.3401534 0.17235368
BFA 1993 0.4113012 0.57687559 0.4713230 0.04946605
BFA 1998 0.4312059 0.64771655 0.2834596 0.06128708"
dat <- read_delim(raw_data, delim = " ")


# Arrange and Group -------------------------------------------------------

# this is grouped by ISO, so only comparing differences WITHIN ISO3
# any combinations that are possible are filled with NA
dat_iso_diff <- dat %>% group_by(ISO3) %>% arrange(year) %>% 
  mutate(DV_diff = DV - lag(DV),
         emp_diff = emp - lag(emp),
         fam_diff = fam - lag(fam),
         edu_diff = edu - lag(edu))


# this is not grouped, but arranged by year
# so comparing differences between rows (ISO or year combos)
# only the first row has NA's because there's nothing to subtract from.
dat_diff <- dat %>% arrange(year, ISO3) %>% 
  mutate(DV_diff = DV - lag(DV),
         emp_diff = emp - lag(emp),
         fam_diff = fam - lag(fam),
         edu_diff = edu - lag(edu))

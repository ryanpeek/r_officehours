# add vertical bars

library(tidyverse)

dat <- read_csv("~/Downloads/cos_flow.csv")

# add an exceedance col
exceedVal <- 1000
dat <- dat %>% 
  mutate(exceedFlow = case_when(
    Flow_cfs > exceedVal ~ TRUE,
    TRUE ~ FALSE
  ))

# plot w vertical lines
ggplot(data=dat) +
  geom_vline(data = dat %>% filter(exceedFlow==TRUE),
             aes(xintercept=Date), alpha=0.15, color="gray20") +
  facet_grid(.~WY, scales = "free_x") +
  geom_line(aes(x=Date, y=Flow_cfs), color="steelblue", lwd=1) +
  theme_classic()

# and another option if you just want a big box between certain dates
# takes more customization but works
ggplot() +
  facet_grid(.~WY, scales = "free_x") +
  # 2015 box, filter to single record to keep facet option functional
  geom_rect(data = dat %>% filter(WY==2015) %>% slice(1),
           aes(xmin = 40, xmax=55, ymin=0, ymax=Inf), fill="gray20", alpha=0.2)+
  # 2017 box, filter to single record to keep facet option functional
  geom_rect(data = dat %>% filter(WY==2017) %>% slice(1),
            aes(xmin = 5, xmax=125, ymin=0, ymax=Inf), fill="gray20", alpha=0.2)+
  geom_line(data=dat, aes(x=day, y=Flow_cfs), color="steelblue", lwd=1) +
  theme_classic()
  

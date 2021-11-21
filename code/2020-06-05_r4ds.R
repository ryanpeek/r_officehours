
# Art with ggplot2 --------------------------------------------------------
# https://fronkonstin.com/
# https://fronkonstin.com/2017/11/07/drawing-10-million-points-with-ggplot-clifford-attractors/
# tweet: https://twitter.com/aschinchon/status/1267132884281851906

#Veil
library(tidyverse)
seq(from=-10, to=10, by = 0.05) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x = x+pi*sin(y), y= y+pi*sin(x))) +
  geom_point(alpha=.1, shape=20, size=0) + 
  coord_equal() +
  theme_void()


# COVID tsibble  ----------------------------------------------------------

# Covid Data and Post: https://kieranhealy.org/blog/archives/2020/04/23/apples-covid-mobility-data/
# https://kjhealy.github.io/covdata
# remotes::install_github("kjhealy/covdata")
library(covdata)
library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(socviz)
library(ggrepel)

library(showtext)
showtext_auto()
sysfonts::font_add_google("Barlow Condensed", family = "barlow")

library(tsibble)
library(feasts)
library(timeDate)

my_colors <- function(palette="cb"){
  ### The palette with black:
  cb.palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  ## Same one Reversed
  rcb.palette <- rev(cb.palette)
  ## Blue and yellow first choices
  bly.palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9", "#009E73", "#F0E442", "#D55E00", "#CC79A7")
  if (palette=="cb") return(cb.palette) else if (palette=="rcb") return(rcb.palette) else if (palette=="bly") return(bly.palette) else stop("Choose cb, rcb, or bly ony.")
}

is_max <- function(x) {
  seq_along(x) == which.max(x)
}

## Check for figures/ 
ifelse(!dir.exists(here("figures")),
       dir.create(file.path("figures")),
       FALSE)

# nyc
raw_ny <- apple_mobility %>%
  filter(region == "New York City") %>%
  select(region:index) %>%
  rename(mode = transportation_type) %>%
  mutate(mode = tools::toTitleCase(mode),
         weekend = isWeekend(date),
         holiday = isHoliday(as.timeDate(date), listHolidays())) %>%
  mutate(max_day = ifelse(is_max(index), date, NA),
         max_day = as_date(max_day))

(p_raw_ny <- ggplot(raw_ny, mapping = aes(x = date, y = index,
                                         group = mode, color = mode)) +
  geom_vline(data = subset(raw_ny, holiday == TRUE),
             mapping = aes(xintercept = date),
             color = my_colors("bly")[5], size = 2.9, alpha = 0.1) +
  geom_hline(yintercept = 100, color = "gray40") +
  geom_line() +
  geom_text_repel(aes(label = format(max_day, format = "%a %b %d")),
                  size = rel(2), nudge_x = 1, show.legend = FALSE) +
  scale_color_manual(values = my_colors("bly")) +
  labs(x = "Date", y = "Relative Mobility",
       color = "Mode",
       title = "New York City's relative trends in activity. Baseline data with no correction for weekly seasonality",
       subtitle = "Data are indexed to 100 for usage on January 13th 2020. Weekends shown as vertical bars. Date with highest relative activity index labeled.\nNote that in Apple's data 'Days' are defined as Midnight to Midnight PST.",
       caption = "Data: Apple. Graph: @kjhealy") +
  theme(legend.position = "top"))


focus_on <- c("Rio de Janeiro", "Lyon", "Bochum - Dortmund", "Dusseldorf",
              "Barcelona", "Detroit", "Toulouse", "Stuttgart",
              "Cologne", "Hamburg", "Cairo", "Lille")

raw_ts <- apple_mobility %>%
  filter(geo_type == "city") %>%
  dplyr::select(region:index) %>%
  dplyr::rename(mode = transportation_type) %>%
  mutate(mode = tools::toTitleCase(mode),
         weekend = isWeekend(date),
         holiday = isHoliday(as.timeDate(date), listHolidays())) %>%
  dplyr::filter(region %in% focus_on) %>%
  group_by(region) %>%
  mutate(max_day = ifelse(is_max(index), date, NA),
         max_day = as_date(max_day))

raw_ts %>%
  filter(!is.na(max_day))


ggplot(raw_ts, mapping = aes(x = date, y = index,
                             group = mode, color = mode)) +
  geom_vline(data = subset(raw_ts, holiday == TRUE),
             mapping = aes(xintercept = date),
             color = my_colors("bly")[5], size = 1.5, alpha = 0.1) +
  geom_hline(yintercept = 100, color = "gray40") +
  geom_line() +
  geom_text_repel(aes(label = format(max_day, format = "%a %b %d")),
                  size = rel(2), nudge_x = 1, show.legend = FALSE) +
  scale_color_manual(values = my_colors("bly")) +
  facet_wrap(~ region, ncol = 2) +
  labs(x = "Date", y = "Relative Mobility",
       color = "Mode",
       title = "Relative trends in activity, selected cities. No seasonal correction.",
       subtitle = "Data are indexed to 100 for each city's usage on January 13th 2020. Weekends shown as vertical bars.\nDate with highest relative activity index labeled.\nNote that in Apple's data 'Days' are defined as Midnight to Midnight PST.",
       caption = "Data: Apple. Graph: @kjhealy") +
  theme(legend.position = "top")    

feats <- apple_mobility %>%
  filter(geo_type == "city") %>%
  select(region:index) %>%
  rename(mode = transportation_type) %>%
  mutate(mode = tools::toTitleCase(mode)) %>%
  filter(region %in% focus_on) %>%
  as_tsibble(key = c(region, mode)) %>%
  features(index, feature_set(tags = "stl"))

resids <- apple_mobility %>%
  filter(geo_type == "city") %>%
  select(region:index) %>%
  rename(mode = transportation_type) %>%
  dplyr::mutate(mode = tools::toTitleCase(mode)) %>%
  filter(region %in% focus_on) %>%
  as_tsibble(key = c(region, mode)) %>%
  #filter(!is.na(index)) %>% 
  tsibble::fill_gaps(index=0L) %>% 
  #model(classical_decomposition(index)) %>%
  model(STL(index)) %>%
  components() %>%
  mutate(weekend = isWeekend(date),
         holiday = isHoliday(as.timeDate(date), listHolidays())) %>%
  as_tibble() %>%
  group_by(region) %>%
  mutate(max_day = ifelse(is_max(remainder), date, NA),
         max_day = as_date(max_day))

ggplot(resids, aes(x = date, y = remainder, group = mode, color = mode)) +
  geom_vline(data = subset(resids, holiday == TRUE),
             mapping = aes(xintercept = date),
             color = my.colors("bly")[5], size = 1.5, alpha = 0.1) +
  geom_line(size = 0.5) +
  geom_text_repel(aes(label = format(max_day, format = "%a %b %d")),
                  size = rel(2), nudge_x = 1, show.legend = FALSE) +
  scale_color_manual(values = my.colors("bly")) +
  facet_wrap(~ region, ncol = 2) +
  labs(x = "Date", y = "Remainder", color = "Mode",
       title = "Remainder component for activity data (after trend and weekly components removed)",
       subtitle = "Weekends shown as vertical bars. Date with highest remainder component labeled.\nNote that in Apple's data 'Days' are defined as Midnight to Midnight PST.",
       caption = "Data: Apple. Graph: @kjhealy") +
  theme(legend.position = "top",
        text = element_text(family="barlow"))         


# Spatial Polygons for Corner Plots ---------------------------------------

# 1. Calculate the midpoint of each polygon edge for each land plot (so you end up with 4 points per plot)
# 2. snap or intersect these points with your roads
# 3. Filter to plots that have 3 midpoints that snapped to roads

library(tidyverse)
library(ggridges)
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
firsts %>%
  select(year, category) %>%
  mutate(decade = year %/% 10 * 10) %>%
  select(decade, category) %>%
  count(decade, category) %>% 
  ggplot(aes(x = decade, y = fct_rev(category))) +
  geom_density_ridges()


library(tidyverse)
library(ggridges)
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
firsts %>%
  select(year, category) %>%
  mutate(decade = year %/% 10 * 10) %>%
  select(decade, category) %>%
  count(decade, category) %>% 
  ggplot(aes(x = decade, y = fct_rev(category), height = n)) +
  geom_ridgeline(scale = 0.1, alpha = 0.5)

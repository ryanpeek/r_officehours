# R help 2020-12-04


# Sorting and Distinct ----------------------------------------------------

library(tidyverse)

mat = matrix(rnorm(n = 9),
             nrow = 3,
             ncol = 3,
             dimnames = list(c("A","B","C"),
                             c("D","E","F")))

p <- mat %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  pivot_longer(cols = -rowname) %>%
  filter(value > 0) %>%
  rename(name1 = rowname,
         name2 = name) %>%
  rowwise() %>%
  mutate(myPairs = paste(sort(c(name1, name2), decreasing = TRUE), 
                         collapse = ", "))

p %>%
  distinct(myPairs)

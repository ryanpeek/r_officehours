## Summarize and Wrangle Salmon Data

library(tidyverse)
library(tibbletime)

# Load Data ---------------------------------------------------------------

data <- read.csv("data/dbo_TrendData_cv_070920.csv", header=TRUE)
head(data)

# Filter Data -------------------------------------------------------------

#subsetting salmon trends database to include only Deer Creek spring run to pilot VSP calculations

DeerCreek.VSP <- filter(data, PopID == 'deercrk.chin.spr')
DeerCreek.Years <- filter(DeerCreek.VSP, Year>=1970)

# Running 3 year total and Calcs -----------------------------------------

# calculates a running sum of the variable 'Total' over 3 years

# see here: https://cran.rstudio.com/web/packages/tibbletime/vignettes/TT-03-rollify-for-rolling-analysis.html

# make functions for 3 year windows:
sum3 <- rollify(sum, window = 3)
avg3 <- rollify(mean, window = 3)


# Make our new dataframe with all the calculations

DeerCreek.calc <- DeerCreek.Years %>%
  # let's just select the columns of interest:
  select(-c(Angler_harvest:Notes)) %>% 
  # next group by whatever it is we want to use
  group_by(PopID) %>% 
  # then arrange by year (to make sure the rolling window is correct)
  arrange(Year) %>% 
  # can use View() as a way to double check stuff, 
  # then comment out and run again
  # View()
  mutate(N = sum3(Total),
         logTotal = log(Total),
         S = avg3(Total), 
         logPop = log(N),
         delta_logPop = (logPop - lag(logPop, 3)),
         decline = 100*(1-exp(delta_logPop)))

# Plot! -------------------------------------------------------------------

# make a plot sort of like what you had in spreadsheet
library(ggplot2)

ggplot(data=DeerCreek.calc, aes(x=Year, y=delta_logPop)) +
  geom_col()+
  stat_smooth(method = "lm")


# Now do the same but with ALL Spring Run post 1970 -------------------

# filter to all post 1970
data_1970 <- filter(data, Year>=1970, Year<2021,
                    grepl("chin.spr$", PopID))

# now do the same as above but with full dataset
all.calc <- data_1970 %>%
  select(-c(Angler_harvest:Notes)) %>% 
  group_by(PopID) %>% 
  arrange(PopID, Year) %>% 
  # add a tally of number of years per PopID
  add_tally() %>% 
  # here we drop groups with less than 10 years of data
  filter(n>7) %>% 
  # rename the "n" to total number of years
  rename(totalYears = n) %>% 
  mutate(N = sum3(Total),
         logTotal = log(Total),
         S = avg3(Total), 
         logPop = log(N),
         delta_logPop = (logPop - lag(logPop, 3)),
         decline = 100*(1-exp(delta_logPop)))


# plot everything? 
ggplot(data=all.calc, aes(x=Year, y=logPop, color=PopID)) +
  geom_point(show.legend = FALSE)+
  stat_smooth(method = "lm", show.legend = FALSE) +
  facet_wrap(.~PopID)

ggsave(filename = "figures/logPop_year_spring_chinook.png", width = 8, height = 6, units = "in", dpi=200)

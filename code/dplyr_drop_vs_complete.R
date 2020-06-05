# tidyverse complete vs. .drop=TRUE
library(tidyverse)

# get data
surveys <- read_csv("https://tinyurl.com/y36xgftg")

# how many species in data? (n=40)
surveys %>%
  distinct(species) %>%
  tally() 

# how many sex classifications? (n=3, F, M, NA)
surveys %>%
  distinct(sex) 

# tally sex and species counts (n=81 rows)
surveys %>%
  group_by(sex, species) %>%
  tally()

# COMPLETE() WITHOUT NESTING n=81 ----------------------------------------------

# no nesting means fill works across all possible pairings, even if they don't exist in the data, so end up with all possible combos

surveys %>%
  group_by(sex, species) %>%
  tally() %>%
  # need to be working with ungrouped df
  ungroup() %>%
  complete(sex, species, fill = list(n=0)) %>%
  arrange(species) %>% View()

# so for above, there are 120 rows, corresponding to 3 rows per species (one for F, M, NA) for 40 species (3*40=120) even if that data doesn't exist in dataset!


# NESTING OPTION A: nesting(species, sex) n=81 -----------------------------------

# nesting in complete means the columns inside nesting() will only be filled with a value if they already exist in the data (no new rows will be created for variables inside nesting()). This is the same as if we just used group_by(sex, species) and then tally with nothing else.

surveys %>%
  group_by(sex, species) %>%
  tally() %>%
  # need to be working with ungrouped df
  ungroup() %>%
  complete(nesting(sex, species), fill = list(n=0)) %>%
  arrange(species) 

# here complete is only looking for missing variables inside the existing dataset, which only includes sex and species, so we get the same value as 

# OPTION B: species, nesting(sex) n=120 -----------------------------------

# this yields only 120 rows, because similar to the above option, 
# we are saying fill using only the levels available for the existing data
# in this case that is 3 possible: c(F, M, NA),
# and since species is OUTSIDE of nesting, it's filling species for each possible value (in the data) of sex.
surveys %>%
  group_by(sex, species) %>%
  tally() %>%
  # need to be working with ungrouped df
  ungroup() %>%
  complete(species, nesting(sex), fill = list(n=0)) %>%
  arrange(species)


# OPTION C: sex, nesting(species) n=120 -----------------------------------

# now if we switch species and sex in the nesting(), we STILL end up
# with 120 rows (3 per each of the 40 species) because ALL species are represented in the data, and so it's filling across all species
surveys %>%
  group_by(sex, species) %>%
  tally() %>%
  # need to be working with ungrouped df
  ungroup() %>%
  complete(sex, nesting(species), fill = list(n=0)) %>%
  arrange(species)


# NOW USING .drop=TRUE or FALSE -------------------------------------------

# this only works if grouping data is factor!
glimpse(surveys) # lots of character classes

# convert to factor with defaults (retains NA in factor)
surveys_fact1 <- surveys %>% 
  mutate_at(.vars = c("species", "sex", "species_id"), 
            .funs = as.factor)

# this won't show NA (drops them)
levels(surveys_fact1$sex)

# convert to factor but use an explicit factor level "Missing"
surveys_fact2 <- surveys %>% 
  mutate_at(.vars = c("species", "sex", "species_id"), .funs = forcats::fct_explicit_na)

# retains NA as a factor level called "missing"
levels(surveys_fact2$sex)

# OPTION A: WITH NAs: use .drop=FALSE
surveys_fact1 %>%
  group_by(sex, species, .drop = FALSE) %>%
  tally() %>%
  arrange(species)

# so this fills all levels with zero (doesn't drop any data)

# OPTION B: WITH ("Missing"): use .drop=FALSE
surveys_fact2 %>%
  group_by(sex, species, .drop = FALSE) %>%
  tally() %>%
  arrange(species)
# same results but instead of NA, "(Missing)"
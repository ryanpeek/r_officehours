# webscraping
library(xml2)
library(rvest)
library(stringr)
library(tidyverse)

# https://billpetti.github.io/2018-06-10-scraping-rstats-rvest/

url <- "https://itch.io/b/520/bundle-for-racial-justice-and-equality"

gamez <- read_html(url)

#//*[@id="index_grid_sizer_widget_0"]/div/div[1]/div[2]/a
titles <- gamez %>% 
  html_nodes(xpath = '//*[@id="index_grid_sizer_widget_0"]/div/div[1]/div[2]/a')
  #html_nodes('#index_grid_sizer_widget_0 .label') #%>% 
  #html_text %>% 
  #as_tibble()


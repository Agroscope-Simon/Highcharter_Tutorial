library(tidyverse)
library(highcharter)
library(htmlwidgets)
library(RColorBrewer)
library(here)


# Load data

nations <- read_csv(here("data", "nations.csv")) %>%
  mutate(gdp_tn = gdp_percap*population/1000000000000)
  
# prepare data
big4 <- nations %>%
  filter(iso3c == "CHN" | iso3c == "DEU" | iso3c == "JPN" | iso3c == "USA") %>%
  arrange(year) 

# basic symbol-and-line chart, default settings

big4 %>% 
hchart("streamgraph", hcaes(x = year, y = gdp_tn, group = country))
                
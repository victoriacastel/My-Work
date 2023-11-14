## STA 402 
## Homework 4

library(tidyverse)
library(maps)
library(readxl)
library(ggthemes)
library(mapproj)
library(stringr)
# setwd("")
# Comment: set the working directory to the path you store the datasets.


#===============================================================#
# ---- The following is a stating code for the question  ------ #
#===============================================================#
# Download the data (which is the excel data with name Data Download” from the website with a last uploaded date 9/10/2020)
# https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/


## local foods data from "LOCAL" tab in excel sheets
local_food <- read_excel(path="FoodEnvironmentAtlas.xls", sheet="LOCAL") %>% 
  select(FIPS, State, County, FRESHVEG_FARMS12, ORCHARD_ACRES12, 
         GHVEG_FARMS12, GHVEG_SQFTPTH12,DIRSALES_FARMS12)
head(local_food)
# Note: postal abbreviations on state names

## FIPS codes for State and county (unabbreviated) from "Supplemental Data - County" tab
county_fips <- read_excel(path="FoodEnvironmentAtlas.xls", sheet="Supplemental Data - County") %>% 
  select(FIPS, State, County)
head(county_fips)

## County boundary data
county_outlines <- map_data("county") 
head(county_outlines)


# ---------------Start your code for Question 2 below ------------------- #



## county_fips: remove " County" and put state in lowercase
county_fips2 <- county_fips %>%
  mutate(region = str_to_lower(State), ## changing to region so easier to merge with outlines
         County = gsub(pattern=" County| Parish", replacement="", County),
         subregion = str_to_lower(County) ) %>%
  select(FIPS, region, subregion)

## Recode some counties
county_outlines2 <- county_outlines %>%
  mutate(subregion = gsub("st ", "st. ", subregion)) 
  
## Merge county_fips and county_outlines
fips_outline <- full_join(county_fips2, county_outlines2, by=c("region", "subregion"))

## Mutate local_food observations
local_food2 <- local_food %>%
  mutate(subregion = str_to_lower(County) ) %>%
  select(-County) 

## Merge local_food and fips_outline 
food_join <- full_join(fips_outline, local_food2, by=c("FIPS", "subregion")) %>%
  select(FIPS, region, subregion, long, lat, group, order, DIRSALES_FARMS12)



## Choropleth map
p.map <-
  ggplot() +
  geom_polygon(data=food_join, 
               aes(x=long, y=lat, group=group, fill=DIRSALES_FARMS12) ) +
  theme_map() +
  coord_map() +
  scale_fill_gradient(low="lightblue", high="navy",
                      limits = c(1, 1000),
                      breaks = c(1, 10, 100, 1000),
                      labels = c("1", "10", "100", "1000"),
                      name = "Number\nof Farms",
                      na.value = "grey40",
                      guide = guide_colorbar(ticks=TRUE, label=TRUE,
                                             barheight=7.5, barwidth=1),
                      trans = "log10") + ## log transformation
  theme(plot.title = element_text(hjust=0.5, size=24),
        legend.position = c(0.91,0.21),
        plot.subtitle = element_text(size=15)) +
  labs(title = "Number of Farms with Direct Sales, 2020",
       subtitle = "Data Source: USDA Food Atlas, 9/10/2020")
p.map

## Save as PNG
ggsave("HW4_map.png", plot = p.map, width = 8, height = 5, dpi = 300, bg="white")
# STA 404 Project
# Complete R code file for all data handling and visuals

## Packages
library(tidyverse)
library(maps)
library(ggthemes)

## Project theme for graphs
theme_castel <- function() {
  theme_minimal() %+replace%
    theme(
      legend.position = "bottom",
      text = element_text(family="serif"),
      plot.caption = element_text(family="mono", hjust=1, size=9),
      plot.title = element_text(size=14, hjust = 0)
    )
}

## Load all data sets
## Birth data
birth_data <- read_csv(
  "/Users/victoriacastel/Desktop/STA 404/Project 1/NCHS_-_Teen_Birth_Rates_for_Age_Group_15-19_in_the_United_States_by_County.csv") %>%
  ## New col names
  rename("state_fips" = `State FIPS Code`,
         "county_fips" = `County FIPS Code`,
         "fips" = `Combined FIPS Code`,
         "birth_rate" = `Birth Rate`,
         "low_conf" = `Lower Confidence Limit`,
         "high_conf" = `Upper Confidence Limit`) 


## Population data (only 2020 data)
pop_data <- read_csv("/Users/victoriacastel/Desktop/STA 404/Project 1/CC-EST2020-ALLDATA.csv") %>%
  filter(YEAR == 13) %>%
  select(STATE, COUNTY, STNAME, CTYNAME, TOT_POP) %>%
  group_by(STNAME) %>%
  mutate(fips = as.numeric(paste0(STATE, COUNTY)),
         state_pop = sum(TOT_POP)) %>%
  select(STNAME, fips, state_pop )


## Map data
## State data
state_map <- map_data("state") 

## Income data
## Get income data
zip_file <- "/Users/victoriacastel/Desktop/STA 404/Project 1/CAGDP2.zip"

## List of all US states and the US
states_us <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
               "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
               "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
               "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
               "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
               "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", 
               "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

income_data <- read_csv(unzip(zip_file, files = unzip(zip_file, list = TRUE)$Name[1])) %>%
  filter(Description == "All industry total") %>% ## total gdp
  mutate(fips = as.numeric(GeoFIPS)) %>% 
  select(-`2001`, -`2002`, -`2021`, -IndustryClassification, -LineCode) %>% ## not important
  pivot_longer(cols=c(`2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, # years in birth df
                      `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Year",
               values_to = "GDP") %>%
  mutate(GDP = as.numeric(ifelse(GDP == "(NA)", NA, GDP))) 


## Data handling and calculations
## Birth county dfs
## County level data by year
county_births_year <- birth_data %>%
  select(State, County, Year, birth_rate, fips)

## State and year birth data ready to merge with population
county_births_year2 <- county_births_year %>%
  select(-County) %>%
  group_by(Year, State) %>%
  summarize(births = sum(birth_rate*1000)) %>% ## out of per 1000
  select(State, Year, births)
pop_data2 <- pop_data %>%
  select(-fips) %>%
  unique()

## County level data avg rate 2003-2020
county_births <- birth_data %>%
  group_by(County) %>%
  mutate(avg_births = mean(birth_rate)) %>%
  select(State, County, fips, avg_births) %>%
  unique()

## State level births get data ready to merege with population
county_births2 <- county_births %>%
  group_by(State) %>%
  mutate(births = avg_births*1000) %>% ## take out of per 1000
  summarize(state_total_births = sum(births)) 


## Income by county
income_county_years <- income_data %>%
  filter((!(GeoName %in% states_us)) & GeoName != "United States *") %>%
  select(GeoName, Year, GDP, fips) %>%
  mutate(Year = as.numeric(Year))
## avg for all years
income_county <- income_county_years %>%
  group_by(GeoName, fips) %>%
  summarise(avg_GDP = mean(GDP))

## Income by sate
income_state_years <- income_data %>%
  filter(GeoName %in% states_us) %>%
  mutate(Year = as.numeric(Year)) %>%
  select(GeoName, Year, GDP, fips)
income_state <- income_state_years %>%
  group_by(GeoName) %>%
  summarise(avg_GDP = mean(GDP))


## Merge data sets
## State birth data
## State level births by year
state_births_year <- full_join(county_births_year2, pop_data2, 
                               by=c("State"="STNAME")) %>%
  mutate(state_births = as.numeric(births / state_pop*100),
         Year = as.numeric(Year)) %>%
  select(State, Year, state_births)

## State level births
state_births <- full_join(county_births2, pop_data, by=c("State"="STNAME")) %>%
  mutate(state_births = state_total_births / state_pop *100) %>%
  select(State, state_births) %>%
  unique()


## Birth map
state_birth_map <- state_births %>%
  ungroup() %>%
  mutate(region = str_to_lower(State)) %>%
  select(-State) %>%
  full_join(state_map, by="region")


## Income map
## Income state level map
state_income_map <- income_state %>%
  mutate(region = str_to_lower(GeoName)) %>%
  select(-GeoName) %>%
  full_join(state_map, by="region")


## Income and birth
## County level with years
income_birth_county_years <- income_county_years %>%
  select(-GeoName) %>%
  full_join(county_births_year, by = c("fips", "Year")) 

## County level 
income_birth_county <- full_join(income_county, county_births, by="fips") %>%
  ungroup() %>%
  select(-GeoName)

## State level with years
glimpse(state_births_year)
glimpse(income_state_years)
income_birth_state_years <- income_state_years %>%
  mutate(Year = as.numeric(Year)) %>%
  full_join(state_births_year, 
            by=c("GeoName"="State", "Year")) %>%
  rename("State" = GeoName)
## State level
income_birth_state <- full_join(state_births, income_state, 
                                by=c("State"="GeoName")) 


#################################################################################
#################################################################################

## Graphs!

## Map of state births
p.birth_map <-
ggplot() +
  geom_polygon(data=state_birth_map, 
               aes(x=long, y=lat, group=group, fill=state_births)) +
  coord_map() +
  theme_map() +
  theme(text = element_text(family="serif"),
        plot.caption = element_text(family="mono",
                                    size=8),
        legend.position = "bottom",
        plot.title = element_text(size=14)) +
  scale_fill_gradient(low="lightblue", high="navy",
                      name = "Number of births",
                      breaks = c(0, 25, 50, 75, 100, 125),
                      labels = c("0", "25", "50", "75", "100", "125"),
                      guide = guide_colorbar(ticks=TRUE, label=TRUE,
                                             barheight=1, barwidth=15)) +
  labs(title = "Teen birth rate (per 1,000 residents) by state",
       caption = "Source: CDC and US Census")

## State level map of income
p.income_map <-
ggplot() +
  geom_polygon(data=state_income_map, 
               aes(x=long, y=lat, group=group, fill=log10(avg_GDP))) +
  coord_map() +
  theme_map() +
  theme(text = element_text(family="serif"),
        plot.caption = element_text(family="mono", size=8),
        legend.position = "bottom",
        plot.title = element_text(size=14)) +
  scale_fill_gradient(low="lightblue", high="navy",
                      name = "Log of GDP",
                      breaks = c(7.5, 8, 8.5, 9, 9.5),
                      labels = c("7.5", "8", "8.5", "9", "9.5"),
                      guide = guide_colorbar(ticks=TRUE, label=TRUE,
                                             barheight=1, barwidth=15)) +
  labs(title = "GDP per capita by state",
       caption = "Source: US Bureau of Economic Analysis")


## Time series plots
## Income over time by state
p.income_timeseries <-
ggplot(data=income_state_years) +
  geom_line(aes(x=Year, y=log10(GDP), group=GeoName),
            alpha=0.5, color="grey30") +
  geom_smooth(aes(x=Year, y=log10(GDP)), 
              method="lm", se=FALSE, color="navy") +
  theme_castel() +
  theme(axis.text = element_text(size=10)) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(title = "GDP per capita over time by state",
       caption = "Source: US Bureau of Economic Analysis",
       y = "Log of GDP per capita",
       x = "Year")

## Birth rate over time by state
p.birth_timeseries <-
ggplot(data=state_births_year) +
  geom_line(aes(x=Year, y=state_births, group=State),
            alpha=0.5, color="grey30") +
  geom_smooth(aes(x=Year, y=state_births), 
              method="lm", se=FALSE, color="navy") +
  theme_castel() +
  theme(axis.text = element_text(size=10)) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(title = "Teen birth rate over time by state",
       caption = "Source: CDC and US Census",
       y = "Birth rate (per 1,000 residents)",
       x = "Year")

## Scatter plot birth rate and income by state
p.scatter <-
ggplot(data=income_birth_state,
       aes(log10(avg_GDP), y=state_births)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="lightblue") +
  theme_castel() +
  theme(axis.text = element_text(size=10)) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(breaks = c(0, 50, 100, 150),
                     minor_breaks = NULL) +
  labs(title = "Teen birth rate vs GDP per capita",
       subtitle = "Negative association between teen birth rate and GDP per capita",
       caption = "Source: CDC, US Census, and BEA",
       x = "Log of GDP per capita",
       y = "Birth rate (per 1,000 residents)") +
  annotate("rect", xmin=8.3, xmax=9.4, ymin=65, ymax=75, fill="white", color="grey40") +
  annotate("text", x=8.85, y=70, 
           label="Birth rate = -33.3 * Log of GDP per capita + 304.1", 
           family="serif", size=4) 

## Birth rate ordered by GDP per capita
## Arrange data by birth rate
income_birth_state2 <- income_birth_state %>%
  arrange(avg_GDP) %>%
  filter(State != "District of Columbia") %>%
  mutate(State = factor(State, levels = State)) 
p.bar <-
  ggplot() +
  geom_col(data = income_birth_state2, aes(x = state_births, y = State, fill=log10(avg_GDP))) +
  scale_x_continuous(expand = c(0, 0), minor_breaks = NULL) +
  theme_castel() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size=10)) +
  labs(title = "Teen birth rate by state",
       subtitle = "Ordered by GDP per capita",
       caption = "Source: CDC, US Census, and BEA",
       x = "Birth rate (per 1,000 residents)") +
  scale_fill_gradient(low="lightblue", high="navy",
                      name = "Log of GDP",
                      breaks = c(7.5, 8, 8.5, 9, 9.5),
                      labels = c("7.5", "8", "8.5", "9", "9.5"),
                      guide = guide_colorbar(ticks=TRUE, label=TRUE,
                                             barheight=1, barwidth=15))


## Save all images
## Maps
ggsave("birth_map.png", plot = p.birth_map, width = 8, height = 6, dpi = 300, bg="white")
ggsave("income_map.png", plot = p.income_map, width = 8, height = 6, dpi = 300, bg="white")

## Scatterplot
ggsave("scatter.png", plot = p.scatter, width = 8, height = 6, dpi = 300, bg="white")

## Timeseries plots
ggsave("birth_timeseries.png", plot = p.birth_timeseries, width = 8, height = 6, dpi = 300, bg="white")
ggsave("income_timeseries.png", plot = p.income_timeseries, width = 8, height = 6, dpi = 300, bg="white")

## Ordered bar chart
ggsave("bar.png", plot=p.bar, width = 8, height = 6, dpi = 300, bg="white")

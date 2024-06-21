## Aggregated data for Shiny app

## Packages
library(tidyverse)
library(httr)
library(readxl)
library(stringr)
library(mapproj)
library(leaflet)
library(geojsonio)
library(ggthemes)
library(lubridate)
library(jsonlite)
library(sf)

## Download covid data using a function
## Files are separated by date
download_files_from_github <- function(owner, repo, path) {
  ## API for GitHub
  api <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", path)
  get <- GET(api)
  if (http_type(get) == "application/json") {
    files <- content(get)
    
    ## Files all end with this
    data_files <- files[grep("\\b2020\\b|\\b2021\\b", files$name)]
    data_files <- data_files[grep("\\b(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])-202[01]\\.csv$", data_files$name)]
    
    ## Download files with loop
    for (file in data_files) {
      file_url <- file$download_url
      file_name <- file$name
      file_content <- GET(url)
      
      ## Save files
      writeBin(content(file_content, "raw"), file_name)
    }
  } 
}

## Use function to download files from API
download_files_from_github("CSSEGISandData", "COVID-19", "csse_covid_19_data/csse_covid_19_daily_reports_us")

## Read all covid data files
file_list <- list.files(path = "~/Desktop/STA 404/Project 2/Covid Data", 
                        pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSV files using bind_rows
covid_data <- lapply(file_list, read_csv) %>%
  bind_rows()

## Population data
population_data <- read_xlsx("NST-EST2022-POP.xlsx", skip=8) %>%
  rename(State = West, ## rename columns
         est_base = `78588565`,
         pop_2020 = `78650958`,
         pop_2021 = `78589763`,
         pop_2022 = `78743364`) %>%
  mutate(State = str_remove(State, pattern = ".")) %>%
  slice(1:51) %>% ## deleting citation info
  select(State, pop_2020, pop_2021) ## only want these years

## Covid data and population data
covid_pops <- covid_data %>%
  full_join(population_data, by=c("Province_State" = "State")) %>%
  filter(!(Province_State %in% c("Diamond Princess", "Grand Princess", "Guam", "Recovered", 
                                 "American Samoa", "Northern Mariana Islands", "Puerto Rico", 
                                 "Virgin Islands"))) %>% ## only 50 states
  ## Calculating rates
  mutate(confirmed_rate = Confirmed/pop_2020 * 1000, ## per 1,000 residents
         death_rate = Deaths/pop_2020 * 1000,
         recovered_rate = Recovered/pop_2020 * 1000,
         active_rate = Active/pop_2020 * 1000) %>%
  ## Calculating number of new cases and deaths per day per 1,000
  arrange(Province_State, Date) %>%
  group_by(Province_State) %>%
  mutate(new_cases = Confirmed - lag(Confirmed, default = first(Confirmed)),
         new_case_rate = new_cases/pop_2020 * 1000) %>% ## new cases per 1000
  filter(new_case_rate >= 0) %>%
  select(Province_State, Date, confirmed_rate, death_rate, recovered_rate, active_rate, new_case_rate) 


## Get geojson data for using leaflet
get_geojson <- function() {
  state_codes <- c(
    "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
    "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
    "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
    "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY") ## Files begin with state letter code
  
  url_base <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries/USA/"
  
  us_geojson <- list()
  
  for (code in state_codes) { ## files beginning with AL, AK, etc
    url <- paste0(url_base, code, ".geo.json")  ## end with .geo.json
    file <- paste0(code, ".geo.json")
    download.file(url, destfile = file, mode = "wb") ## download file
    us_geojson[[code]] <- st_read(file)
    file.remove(file)
  }
  ## bind files together
  us_geojson <- do.call(rbind, us_geojson)
  
  return(us_geojson)
}

## Apply the function
usa_data <- get_geojson()

## Combine covid populations and shape data
map_data <- covid_pops %>%
  filter(Province_State != "District of Columbia") %>%
  left_join(usa_data, by=c("Province_State"="name"))

# Convert data to sf object
map_sf <- st_as_sf(map_data)

## Using political party data for scatterplot
## 2023 governor determines political party
political_data <- read_csv("raw_data.csv") %>%
  slice(4:54) %>%
  ## Give temporary short name
  rename(col = `Title: State Political Parties | KFF`) %>%
  filter(col != "District of Columbia,Democrat,1") %>%
  ## Split col into state and party
  separate(col, into = c("State", "Party"), sep = ",") %>%
  drop_na()

## Join with covid_pops data
covid_poli <- covid_pops %>%
  full_join(political_data, by=c("Province_State"="State"))

## Save all files
save(covid_pops, file = "covid_pops.RData")
save(map_sf, file = "map_sf.RData")
save(covid_poli, file = "covid_poli.RData")


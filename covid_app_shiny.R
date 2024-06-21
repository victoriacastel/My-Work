## R Shiny app code

## Load packages
library(tidyverse)
library(httr)
library(readxl)
library(stringr)
library(mapproj)
library(leaflet)
library(geojsonio)
library(ggthemes)
library(lubridate)
library(shiny)
library(jsonlite)
library(sf)
library(htmltools)
library(shinythemes)
library(RColorBrewer)

## Load data
load("covid_pops.RData")
load("map_sf.RData")
load("covid_poli.RData")

## Label vector
plot_case_death_input_labs <- c(confirmed_rate="Total number of cases (per 1,000)",
                death_rate="Total number of deaths (per 1,000)",
                new_case_rate="Number of new cases (per 1,000)") 

plot_mean_avg_input_labs <- c(date_new_cases="Total number of new cases (per 1,000)",
                              avg_date_cases="Average number of new cases (per 1,000)")

## Color vector
## First 9 states (name as # of colors in palette)
top_8 <- covid_pops %>%
  select(Province_State) %>%
  unique() %>%
  filter(Province_State != "District of Columbia") %>%
  head(8)

## Define color palette to use
plotcolors <- brewer.pal(8, name="Set2") 

## Assign colors to states for all input data sets
names(plotcolors) <- sort(top_8$Province_State)

## User Interface
ui <- fluidPage(
  theme = shinytheme("journal"), ## add a theme for the app
  titlePanel("Covid-19 Data Explorer April 2020 to December 2021"),
  
  sidebarLayout(
    sidebarPanel(
      
      ## Conditional for Timeseries and map
      conditionalPanel(
        condition = "input.tabs == 'Timeseries' || input.tabs == 'Maps'",
        
        ## Choice of total cases, total deaths, new cases (time, map)
        selectInput(inputId="case_death_input", 
                    label="Please Select",
                    choices=c("Total Number of Cases"="confirmed_rate",
                              "Total Number of Deaths"="death_rate",
                              "Number of New Cases"="new_case_rate"))
      ),
      
      ## Conditional for Timeseries and Scatterplot
      conditionalPanel(
        condition = "input.tabs == 'Timeseries' || input.tabs == 'Scatterplot'",
        
        ## Choice of state to highlight (time, scat)
        selectInput(inputId="state_input",
                    label="Select State to Highlight",
                    choices=covid_pops$Province_State,
                    multiple=TRUE)
      ),
      
      ## Conditional for Timeseries and Barchart
      conditionalPanel(
        condition = "input.tabs == 'Timeseries' || input.tabs == 'Barchart'", 
        
        ## Choice of date range (time, bar)
        dateRangeInput(inputId="date_range_input",
                       label="Select a Date Range",
                       start="2020-04-12",
                       end="2021-12-10",
                       min="2020-04-12",
                       max="2021-12-10",
                       format="yyyy-mm-dd")
      ),
      
      
      ## Conditional for Maps and Scatterplot
      conditionalPanel(
        condition = "input.tabs == 'Maps' || input.tabs == 'Scatterplot'",
        
        ## Choice of date (map, scat)
        dateInput(inputId="date_input",
                  label="Select a Date",
                  value="2021-12-10",
                  min="2020-04-12",
                  max="2021-12-10",
                  format="yyyy-mm-dd")
      ),
      
      ## Conditional for Barchart
      conditionalPanel(
        condition = "input.tabs == 'Barchart'",
        
        ## Choice of avg or total cases (bar)
        selectInput(inputId="mean_avg_input",
                    label="Please Select",
                    choices=c("Average new cases"="avg_date_cases",
                              "Total new cases"="date_new_cases")),
      ),
      
    ),
    
    mainPanel(
      tabsetPanel(id="tabs",
                  tabPanel(title="ReadMe", verbatimTextOutput("readme_plot")),
                  tabPanel(title="Timeseries", plotOutput("timeseries_plot")),
                  tabPanel(title="Maps", leafletOutput("map_plot")),
                  tabPanel(title="Scatterplot", plotOutput("scat_plot")),
                  tabPanel(title="Barchart", plotOutput("bar_plot"))
                  
      )
    )
  )
)

## Server function
server <- function(input, output, session) {
  
  ## ReadMe tab
  output$readme_plot <- renderText({

    c(" This is an exploration of Covid-19 data from April 2020 to December 2021.\n",
      "All rates are computed on a per capita basis, representing figures per 1,000 residents.\n",
      "\nTimeseries and Maps tabs:\n'Total Cases' and 'Total Deaths' are the cases/deaths to date. 'New Cases' refer to cases per day.\n",
      "\nBarchart tab:\n'Total new cases' and 'Average new cases' refer to the total or avgerage cases within the specified date range.\n",
      "\nSome fun inputs to try:",
      "\nHighlight New York and see how it behaves over time.",
      "\nCheck out the case spike in December 2020.\n",
      "\nReferences:",
      "\nJohan. (2016). Annotated geo-json geometry files for the world. GitHub. 
        https://github.com/johan/world.geo.json/tree/master/countries/USA",
      "\nJohn Hopkins University. (2023). Covid-19 data repository. Center for Systems Science and 
        Engineering (CSSE) at Johns Hopkins University. https://github.com/CSSEGISandData/COVID-19",
      "\nNational Governors Association. (2023). State political parties. Kaiser Family Foundation.
        https://www.kff.org/other/state-indicator/state-political-parties/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
      "\nUS Census. (2022). 2022 national and state population estimates press kit. United States 
        Census Bureau. https://www.census.gov/newsroom/press-kits/2022/2022-national-state-population-estimates.html#:~:text=The%20U.S.%20resident%20population%20increased,national%20and%20state%20population%20estimates.")
    
  })
  
  ## Timeseries plot
  output$timeseries_plot <- renderPlot({
    
    ## Filter for UI date range 
    timeseries_date_filtered <- covid_pops %>%
      filter(Date >= min(as.Date(input$date_range_input)),
             Date <= max(as.Date(input$date_range_input)))
    ## Filter for UI state to highlight with dates
    timeseries_state_filtered <- timeseries_date_filtered %>%
      filter(Province_State %in% input$state_input)
    
    ## Create timeseries graph
    ggplot() +
      ## This is for all the lines
      geom_line(data=timeseries_date_filtered, 
                aes_string(x="Date", y=input$case_death_input, 
                           group="Province_State"), color="grey10", alpha=0.5) +
      ## This is for the highlighted line
      geom_line(data=timeseries_state_filtered, 
                aes_string(x="Date", y=input$case_death_input,
                           group="Province_State", color="Province_State"), size=1.5) +
      theme_minimal() +
      theme(text = element_text(family="serif"), ## change text and sizes
            plot.caption = element_text(family="mono", color="grey30", size=12),
            plot.title = element_text(size=18),
            axis.text = element_text(size=12),
            axis.title = element_text(size=14),
            plot.subtitle = element_text(size=12),
            legend.text = element_text(size=12),
            legend.title = element_text(size=14)) +
      ## Use defined color palette
      scale_color_manual(values=plotcolors[input$state_input]) +
      scale_x_date(minor_breaks = NULL) + ## take away major gridlines
      scale_y_continuous(minor_breaks = NULL) +
      labs(y = plot_case_death_input_labs[input$case_death_input], ## dynamic labels
           caption = "Source: Johns Hopkins University and US Census",
           title = paste0(plot_case_death_input_labs[input$case_death_input], " over time"), ## dynamic title
           subtitle = paste0("From ", min(as.Date(input$date_range_input)), " to ", max(as.Date(input$date_range_input)))) +
      guides(color=guide_legend("State")) ## set up legend for UI
    
  }) 
  
  ## Interactive map
  output$map_plot <- renderLeaflet({
    
    ## Filter for UI date input
    map_filtered_data <- map_sf %>%
      filter(Date == as.Date(input$date_input))
    
    ## Selected column
    column_selected <- switch(input$case_death_input,
                              "confirmed_rate" = map_filtered_data$confirmed_rate,
                              "death_rate" = map_filtered_data$death_rate,
                              "new_case_rate" = map_filtered_data$new_case_rate)
    
    ## Define breaks
    breaks <- quantile(column_selected, probs = seq(0, 1, 0.2), na.rm = TRUE)
    breaks <- unique(breaks)  ## Unique for each column
    
    ## Red/orange/yellow color palette with breaks
    color_palette <- colorNumeric(palette = "YlOrRd", domain = column_selected)
    
    ## Create leaflet map
    leaflet() %>%
      setView(lng = -96, lat = 37.8, zoom = 4) %>%
      addTiles() %>%
      addPolygons(
        data = map_filtered_data, ## UI date
        fillColor = ~color_palette(column_selected), ## fill according to defined color palette
        fillOpacity = 0.5, ## somewhat opaque, can see cities under fill
        stroke = TRUE,
        popup = ~paste("State: ", Province_State, "<br>", ## define popup window
                       "Rate: ", round(column_selected, digits = 2), " per 1,000"),
        color = "grey0",
        weight = 1
      ) %>%
      addLegend( ## legend
        "bottomright",
        pal = color_palette, ## use defined color palette
        values = column_selected, ## different breaks for each col
        title = "Rate per 1,000",
        opacity = 0.5
      )
  })
  
  ## Scatter plot
  output$scat_plot<- renderPlot({
    
    ## Filter UI date and states to highlight
    covid_scatter <- covid_poli %>%
      filter(Date == as.Date(input$date_input),
             Province_State != "District of Columbia") ## no govorner in DC, value NA
    covid_scatter_states <- covid_scatter %>%
      filter(Province_State %in% input$state_input)
    
    ggplot() +
      ## All the points
      geom_point(data=covid_scatter, 
                 aes(x=confirmed_rate, y=death_rate, 
                            group=Province_State, shape=Party), color="grey20", size=2) +
      ## UI highlighted points
      geom_point(data=covid_scatter_states, 
                 aes(x=confirmed_rate, y=death_rate, shape=Party,
                     group=Province_State, color=Province_State), size=2.5) +
      ## regression line
      geom_smooth(data=covid_scatter, 
                  aes(x=confirmed_rate, y=death_rate),
                  method="lm", se=FALSE, color="darkorange") +
      theme_minimal() +
      theme(text = element_text(family = "serif"), ## change tezt and size
            plot.caption = element_text(family="mono", color="grey30", size=12),
            plot.title = element_text(size=18),
            axis.text = element_text(size=12),
            axis.title = element_text(size=14),
            plot.subtitle = element_text(size=12),
            legend.text = element_text(size=12),
            legend.title = element_text(size=14)) +
      ## Use defined color palette
      scale_color_manual(values=plotcolors[input$state_input]) +
      labs(title = "Cases to date vs deaths to date by state",
           x = "Total confirmed cases to date (per 1,000)",
           y = "Total deaths to date (per 1,000)",
           caption = "Source: Johns Hopkins University, US Census, and NGA",
           subtitle = paste0("Date: ", as.Date(input$date_range_input))) +
      scale_x_continuous(minor_breaks = NULL) + ## remove x grid lines
      scale_y_continuous(minor_breaks = NULL) + ## remove y grid lines
      guides(color = guide_legend("State")) ## set up for UI state to highlight
    
  })
  
  ## Bar chart
  output$bar_plot <- renderPlot({
    
    ## Filter UI month
    covid_months <- covid_pops %>%
      filter(Province_State != "District of Columbia",
             ## UI date range
             Date >= min(as.Date(input$date_range_input)),
             Date <= max(as.Date(input$date_range_input))) %>%
      group_by(Province_State) %>%
      ## Calculate total and avg new cases by state for selected dates
      summarize(date_new_cases = sum(new_case_rate),
             avg_date_cases = mean(new_case_rate))
    
    ## Reorder bars according to UI input variable
    if (input$mean_avg_input == "avg_date_cases") {
      covid_months$Province_State <- fct_reorder(covid_months$Province_State, 
                                                 covid_months$avg_date_cases,
                                                 .desc = TRUE)
    } else {
      covid_months$Province_State <- fct_reorder(covid_months$Province_State, 
                                                 covid_months$date_new_cases,
                                                 .desc = TRUE)
    }
    
    
    ## Bar chart with conditional coloring for the highlighted state
    ggplot() +
      geom_col(data=covid_months,
               aes_string(x=input$mean_avg_input, y="Province_State"),
               fill="grey30",
               position = position_dodge()) + ## this made the bars not weird
      theme_minimal() +
      theme(text = element_text(family="serif"), ## change text and size
            plot.caption = element_text(family="mono", color="grey30", size=12),
            plot.title = element_text(size=18),
            axis.text = element_text(size=10), 
            axis.title = element_text(size=14),
            plot.subtitle = element_text(size=12)) +
      labs(x = plot_mean_avg_input_labs[input$mean_avg_input],
           y = "",
           caption = "Source: Johns Hopkins University and US Census",
           title = paste0(plot_mean_avg_input_labs[input$mean_avg_input], " by state"), ## dynamic titles
           subtitle = paste0("From ", min(as.Date(input$date_range_input)), " to ", max(as.Date(input$date_range_input)))) +
      scale_x_continuous(expand = c(0,0), ## remove padding
                         minor_breaks = NULL)  ## remove major grid lines
    
  })
  
}

## Call to shiny app
shinyApp(ui = ui, server = server)

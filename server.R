library(matrixStats) # important - load this first, since otherwise clashes with tidyverse::count
library(DT)
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)
library(sgo)
library(janitor)
library(htmltools)
library(sf)

source("helpers/eBirdFileHelpers.R")

options(shiny.maxRequestSize = 100*1024^2)

# TODO:
# - adjust upload to take zip output straight from eBird
# - add instructions panel
# - add input validation (guarding against dodgy files)
# - add quick wins on data table (eg col filters)
# - apply suitable theme

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # import BOU list for species names
  bou <- read_csv("refData/BOU_British_List_working-to-10th-and-54th-w-11_2-v3.csv",
                  locale = locale(encoding = "CP1252")) %>%
    clean_names() %>%
    select(1:4) %>%
    rename(
      BOU_vernacular_name = british_english_vernacular_name,
      BOU_category = category
    )
  
  regions = read_csv("refData/eBirdRegions.csv")
  
  hotspots = read_csv("refData/GBHotspots.csv") %>% 
    inner_join(regions,by = c("subnational2Code" = "code")) %>% 
    rename(county = name)
  
  observerData = reactive({
    req(input$uploadUsers$datapath)
    
    read_tsv(input$uploadUsers$datapath) %>%
      mutate(full_name = paste(first_name, last_name))
  })
  
  rawData = reactive({
    req(input$upload)
    
    extractDataFromEbirdZip(input$upload$datapath)
    
  })
  
  data <- reactive({
    raw = rawData()
    
    if (isTruthy(input$uploadUsers)){
      
      users <- observerData()
      
      # join to user database
      raw <- raw %>% left_join(users, by = "observer_id")
    }
    
    # join to BOU names; coalesce to give a single non empty column
    # TODO: can we just use eBird taxonomy here?
    allFiltered <- raw %>%
      left_join(bou, names, by = "scientific_name") %>%
      mutate(BOU_Ebird_common_name = coalesce(BOU_vernacular_name, common_name))
    
    # pull out separate date cols
    allFiltered <- allFiltered %>%
      mutate(
        observation_day = day(observation_date),
        observation_month = month(observation_date),
        observation_year = year(observation_date)
      )
    
    # calculate and add OS grid refs
    os <- sgo_points(list(longitude = allFiltered$longitude,
                          latitude = allFiltered$latitude),
                     epsg = 4326) %>%
      sgo_lonlat_bng() %>%
      sgo_bng_ngr() 
    
    os = os$ngr
    
    os <- gsub(" ", "", os)
    
    allFiltered$os = os
    
    # find and append details of nearest hotspots
    withHotspots = attachNearestHotspots(allFiltered, hotspots)
    
    output = withHotspots %>% select(
      species = BOU_Ebird_common_name,
      scientific_name,
      subspecies_common_name,
      subspecies_scientific_name,
      observation_count,
      observation_date,
      breeding_code:age_sex,
      county,
      locality,
      latitude,
      longitude,
      os,
      time_observations_started,
      observation_date, 
      # observation_day:observation_year,
      species_comments,
      approved,
      contains("nearestHotspot"),
      any_of("full_name"))
    
    output
    
  })
  
  output$tbl = renderDT(
    data(), 
    filter = "top",
    # extensions = c("Scroller"),
    style = "bootstrap",
    options = list(deferRender = TRUE
                   # scrollY = 300, 
                   # scrollX = FALSE, 
                   # scroller = TRUE
                   ),
  )
  
  output$map <- renderLeaflet({
    recordsByLocation = data() %>% 
      count(locality, latitude,longitude)
    
    leaflet(recordsByLocation) %>% 
      addTiles() %>% 
      addMarkers(label = ~htmlEscape(str_glue("{locality}, {n} records")),
                 clusterOptions = markerClusterOptions()
      )
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "cleanedEbirdData.csv"},
    content = function(file) {
      write_csv(data(), file, na = "") 
    }
  )

})

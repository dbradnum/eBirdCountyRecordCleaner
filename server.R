suppressPackageStartupMessages(
{
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
  library(fs)
  library(dbplyr)
  library(arrow)
  library(duckdb)
})

source("helpers/eBirdFileHelpers.R")

options(shiny.maxRequestSize = 100*1024^2)

ALL_SPECIES = "-- All Species --"

# TODO:
# - add quick wins on data table (eg col filters)
# - apply suitable theme
# - work out why BBRC subspecies doesn't work
# - make shapefile upload robust to zero or multiple SHPs

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # import BOU list for species names
  bou <- read_csv("refData/BOU_British_List_working-to-10th-and-54th-w-11_2-v3.csv",
                  locale = locale(encoding = "CP1252"),
                  show_col_types = FALSE) %>%
    clean_names() %>%
    select(1,3:5) %>%
    rename(
      BOU_vernacular_name = british_english_vernacular_name,
      BOU_category = category
    ) %>% 
    mutate(codes = replace_na(codes,""),
           BBRC_species = str_detect(codes,"†"),
           BBRC_subspecies = str_detect(codes,"‡")) %>% 
    select(-codes,-BOU_category)
  
  ebirdTaxonomy = read_csv("refData/eBird-Clements-v2023-integrated-checklist-October-2023.csv",
                           show_col_types = F,
                           col_types = paste0("n",strrep("c",16))) %>% 
    clean_names() %>% 
    select(english_name,category) %>% 
    rename(eBird_category = category) %>% 
    mutate(eBird_category = as.factor(eBird_category))
  
  regions = read_csv("refData/eBirdRegions.csv",show_col_types = FALSE)
  
  hotspots = read_csv("refData/GBHotspots.csv",show_col_types = FALSE) %>% 
    left_join(regions,by = c("subnational2Code" = "code")) %>% 
    rename(county = name)
  
  duckCon = dbConnect(duckdb(), 
                      dbdir = tempfile(fileext = ".duckdb"), 
                      read_only = FALSE)
  
  dbExecute(conn = duckCon, paste0("SET memory_limit='200MB'"))

  
  # values <- reactiveValues(
  #   shp_upload_state = NULL
  # )
  # 
  # observeEvent(input$uploadShapefile, {
  #   values$shp_upload_state <- 'uploaded'
  # })
  # 
  # observeEvent(input$resetShapeFile, {
  #   values$shp_upload_state <- 'reset'
  # })
  
  observerData = reactive({
    req(input$uploadUsers$datapath)
    
    a = open_dataset(
      sources = input$uploadUsers$datapath, 
      # col_types = schema(ISBN = string()),
      format = "tsv"
    )
    
    a %>% 
      mutate(full_name = paste(first_name, last_name)) %>% 
      select(observer_id,full_name) %>% 
      to_duckdb(con = duckCon)
  })
  
  
  rawData = reactive({
    req(input$upload)
    
    extractDataFromEbirdZip(input$upload$datapath)
    
  })
  
  customRegionBoundary = reactive({
    # req(input$uploadShapefile)
    
    if (is.null(values$shp_upload_state)) {
      return(NULL)
    } else if (values$shp_upload_state == 'uploaded') {
      
      unzipPath = file.path(tempfile())
      unzip(input$uploadShapefile$datapath,exdir = unzipPath)
      
      shpFile = dir_info(unzipPath) %>% 
        filter(str_ends(path,"shp")) %>% 
        pull(path) %>% 
        head(1)
      
      read_sf(shpFile) %>%  
        st_transform(27700)
      
    } else if (values$shp_upload_state == 'reset') {
      return(NULL)
    }

    
  })
  
  data <- reactive({
    obs = rawData()
    
    # filter, since bizarrely can sometimes end up with data for UK counties 
    # listed against the wrong country!
    obs = obs %>% filter(country == "United Kingdom")
    
    counties = obs %>% count(county) %>% arrange(desc(n))
    nCounties = nrow(counties)
    topCounty = head(counties$county,1)
    
    cat(file = stderr(),
        str_glue("---------- Uploaded {nrow(obs)} rows data from {nCounties} county; most from {topCounty}\n"))
    
    if (isTruthy(input$uploadUsers)){

      users <- observerData()
      
      rawTbl = obs %>% to_duckdb(con = duckCon)
      
      obs = rawTbl %>% 
        left_join(users,
                  by = "observer_id" ) %>% 
        collect() 
    }
    
    # if (isTruthy(customRegionBoundary())){
    #   regionBounds = customRegionBoundary()
    #   
    #   sites = raw %>% 
    #     count(country, state, county,locality, locality_type,latitude, longitude,
    #           name = "nRecords") %>% 
    #     st_as_sf(coords = c("longitude", "latitude"),
    #              crs = 4326,
    #              remove = F) %>% 
    #     st_transform(27700)
    #   
    #   filteredSites = sites %>% 
    #     filter(st_intersects(.,regionBounds,sparse = F) ) %>% 
    #     st_drop_geometry() %>% 
    #     select(latitude, longitude)
    #   
    #   raw = raw %>% inner_join(filteredSites,by = c("latitude","longitude"))
    # }
    
    # join to BOU names; coalesce to give a single non empty column
    obs <- obs %>%
      left_join(bou, names, by = "scientific_name") %>%
      mutate(BOU_Ebird_common_name = coalesce(BOU_vernacular_name, common_name)) %>% 
      left_join(ebirdTaxonomy,by = c("common_name" = "english_name")) %>% 
      select(-BOU_vernacular_name,-common_name)
    
    # # pull out separate date cols
    # obs <- obs %>%
    #   mutate(
    #     observation_day = day(observation_date),
    #     observation_month = month(observation_date),
    #     observation_year = year(observation_date)
    #   )
    # 
    # calculate and add OS grid refs
    os <- sgo_points(list(longitude = obs$longitude,
                          latitude = obs$latitude),
                     epsg = 4326) %>%
      sgo_lonlat_bng() %>%
      sgo_bng_ngr() 
    
    os = os$ngr
    
    os <- gsub(" ", "", os)
    
    obs$os = os
    obs$os1km = paste0(str_sub(os,1,4),str_sub(os,8,9))
    
    # find and append details of nearest hotspots
    obs = attachNearestHotspots(obs, hotspots)
    
    obs %>% select(
      species = BOU_Ebird_common_name,
      scientific_name,
      subspecies_common_name,
      subspecies_scientific_name,
      BBRC_species,
      eBird_category,
      observation_count,
      observation_date,
      breeding_code:age_sex,
      county,
      locality,
      latitude,
      longitude,
      os,
      os1km,
      contains("nearestHotspot"),
      time_observations_started,
      observation_date, 
      species_comments,
      approved,
      reviewed,
      any_of("full_name"))
  
    
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
    speciesFilter = input$cboMapSpecies
    
    dataToMap = data()
    if (!is.null(speciesFilter) && 
        nchar(speciesFilter) > 0 && 
        speciesFilter != ALL_SPECIES)
    {
      dataToMap = dataToMap %>% 
        filter(species == speciesFilter)
    }
    
    recordsByLocation = dataToMap %>% 
      count(locality, latitude,longitude)
    
    leaflet(recordsByLocation) %>% 
      addTiles() %>% 
      addMarkers(label = ~htmlEscape(str_glue("{locality}, {n} records")),
                 lng = ~longitude,
                 lat = ~latitude,
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
  
  observeEvent(data(),
               {
                 d = data()
                 distinctSp = d %>% 
                   distinct(species) %>% 
                   arrange(species) %>% 
                   pull()
                 
                 updateSelectizeInput(inputId = "cboMapSpecies",
                                      choices = c(ALL_SPECIES,distinctSp),
                                      selected = character(0),
                                      server = TRUE
                                      )
               })

})

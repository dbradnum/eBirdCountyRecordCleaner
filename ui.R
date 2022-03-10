suppressPackageStartupMessages(
  {
    library(DT)
    library(shiny)
    library(shinycssloaders)
    library(leaflet)
  })

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = bslib::bs_theme(bootswatch = "journal"),
  
  # Application title
  titlePanel("eBird County Data Cleaner"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
   
    sidebarPanel(
      fileInput("upload", "Choose eBird Zip archive",
                accept = ".zip"),
      
      fileInput("uploadUsers", "Choose tab-separated Observer List",
                accept = c(".txt",".tsv")),
      
      downloadButton("downloadData", "Download")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data Table", 
                           DTOutput('tbl') %>% 
                             withSpinner(color="#0dc5c1", type = 4)),
                  tabPanel("Map", 
                           selectizeInput("cboMapSpecies",
                                          "Filter Species:", 
                                          choices = NULL),
                           leafletOutput("map",height = 600) %>% 
                             withSpinner(color="#0dc5c1", type = 4)),
                  tabPanel("Instructions",
                           includeMarkdown("instructions.md"))
      )
    )
    
    
  )
)
)

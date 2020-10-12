library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)

mass_shootings <- read.csv("mass-shootings.csv")
text_about <- "Welcome to, Emon's Map exploring app. This data was compiled by Mother Jones, nonprofit founded in 1976. Originally covering cases from 1982-2012, this database has since been expanded numerous times to remain current."

server <- function(input, output, session) {
    observeEvent(input$show_about, {
        showModal(modalDialog(text_about, title = "About"))
    })
    
    
    rval_mass_shootings <- reactive({
       
        mass_shootings %>%
            filter(fatalities >= input$nb_fatalities)
                  
        
        
        
        
        
        
    })
    output$map <- leaflet::renderLeaflet({
        rval_mass_shootings() %>%
            leaflet() %>% 
            addTiles() %>%
            setView( -98.58, 39.82, zoom = 5) %>% 
            addTiles() %>% 
            addCircleMarkers(
                
                popup = ~summary, radius = ~fatalities,
                fillColor = 'red', color = 'red', weight = 1
            )
    })
}
ui <- bootstrapPage(
    titlePanel("Mass Shooting"),
    shinythemes::shinytheme("simplex"),
    leaflet::leafletOutput('map', height = '100%', width = '100%'),
    absolutePanel(top = 10, right = 10, id = 'controls',
                  sliderInput('nb_fatalities', 'Minimum Fatalities', 1, 40, 10),
                  actionButton("show_about", "About This App")
    ),
    tags$style(type = "text/css", "
    html, body {width:100%;height:100%}     
    #controls{background-color:white;padding:20px;}
  ")
)

shinyApp(ui, server)

library(shiny)
library(plotly)
library(colourpicker)
library(gapminder)

library(ggplot2)

library(shinyWidgets)

dialog <- "Welcome to my interactive application. You spend a lot of time recreating the same plot over and over again by rerunning the same code but changing small parameters each time. To save time from the hassle of rerunning the code many times, I've created a demo of customizable plotting app with gapminder data. The gapminder dataset offers a few basic demographic stats for countries over time. You can also download your filtered data."

css <- "
 #table{
 color:black;
 background:#FFF8DC;
 }

"
    

ui <- fluidPage(
    titlePanel('Data Explore App- Emon'),
    theme = shinythemes::shinytheme("darkly"),
    tags$style(css),
    setBackgroundColor(
        color = c("#000000", "#008080", "#000080"),
        gradient = "radial",
        direction = c("top", "right")
    ),
    sidebarLayout(
        sidebarPanel(
            textInput("title", "Title", "GDP vs life exp"),
            numericInput("size", "Point size", 1, 1),
            checkboxInput("transparent", "Set Plot Transparent", FALSE),
            checkboxInput("fit", "Add line of best fit", TRUE),
            colourInput("color", "Pick Point color", value = "#DB896D"),
            sliderInput(inputId = "life", label = "Life expectancy",
                        min = 0, max = 120,
                        value = c(49, 82)),
            selectInput("continents", "Continents (Multiples Can Be Selected)",
                        choices = levels(gapminder$continent),
                        multiple = TRUE,
                        selected = "Europe"),
            sliderInput("years", "Years", step = 5,
                        min(gapminder$year), max(gapminder$year),
                        value = c(1977, 2002)),
            downloadButton(outputId = "download_data", label = "Download As Filtered"),
            actionButton("about", "About This App")
        ),
        mainPanel(
           
            tabsetPanel(
                
            tabPanel("Data Table", DT::DTOutput("table")),
            tabPanel("Customizable Plot", plotlyOutput("plot"))
            )
        )
    )
)


server <- function(input, output) {
    
    observeEvent(input$about, {
       showModal(modalDialog(title= "Emon's Interactive Plot", dialog))})
       
    filtered_data <- reactive({
        
        data <- gapminder
        data <- subset(
            data,
            continent %in% input$continents &
                year >= input$years[1] & year <= input$years[2] & lifeExp >= input$life[1] & lifeExp <= input$life[2])
          
    })
    
    output$download_data <- downloadHandler(
        filename = "Emon's Responsive Compliment.csv",
        content = function(file) {
           
            
            data <- filtered_data()
            write.csv(data, file, row.names = FALSE)
        }
    )
    
    output$table <- DT::renderDT({
       
        data <- filtered_data()
        data
        
    })
       
    
    output$plot <- renderPlotly({
       
        ggplotly({
            
                           
            
            p <- ggplot(filtered_data(), aes(gdpPercap, lifeExp)) +
                geom_point(size = input$size, col = input$color) +
                scale_x_log10() +
                ggtitle(input$title)
            
            if (input$fit) {
                p <- p + geom_smooth(method = "lm")
            }
            
            if (input$transparent) {
                p <- p + theme(
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "transparent",colour = NA),
                    plot.background = element_rect(fill = "transparent",colour = NA) 
                        
                )
            }
            p
        })
    })
}

shinyApp(ui = ui, server = server)

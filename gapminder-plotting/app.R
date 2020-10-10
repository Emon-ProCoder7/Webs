library(shiny)
library(plotly)
library(colourpicker)
library(gapminder)
library(shinythemes)
library(ggplot2)

dialog <- "Welcome to my interactive application. You spend a lot of time recreating the same plot over and over again by rerunning the same code but changing small parameters each time. The size of the points, the color of the points, the plot title, the data shown on the plot—these criteria all have to be just right before publishing the figure. To save time from the hassle of rerunning the code many times, I've created a demo of customizable plot with gapminder. The gapminder dataset offers a few basic demographic stats for countries over time,"

ui <- fluidPage(
    theme = shinythemes::shinytheme('slate'),
    
    sidebarLayout(
        sidebarPanel(
            textInput("title", "Title", "GDP vs life exp"),
            numericInput("size", "Point size", 1, 1),
            checkboxInput("fit", "Add line of best fit", TRUE),
            colourInput("color", "Pick Point color", value = "blue"),
            selectInput("continents", "Continents (Multiples Can Be Selected)",
                        choices = levels(gapminder$continent),
                        multiple = TRUE,
                        selected = "Europe"),
            sliderInput("years", "Years",
                        min(gapminder$year), max(gapminder$year),
                        value = c(1977, 2002)),
            actionButton("about", "About This App")
        ),
        mainPanel(
            # Replace the `plotOutput()` with the plotly version
            plotlyOutput("plot")
        )
    )
)

# Define the server logic
server <- function(input, output) {
    
    observeEvent(input$about, {
       showModal(modalDialog(title= "Emon's Interactive Plot", dialog))})
       
       
       
    # Replace the `renderPlot()` with the plotly version
    output$plot <- renderPlotly({
        # Convert the existing ggplot2 to a plotly plot
        ggplotly({
            data <- subset(gapminder,
                           continent %in% input$continents &
                               year >= input$years[1] & year <= input$years[2])
            
            p <- ggplot(data, aes(gdpPercap, lifeExp)) +
                geom_point(size = input$size, col = input$color) +
                scale_x_log10() +
                ggtitle(input$title)
            
            if (input$fit) {
                p <- p + geom_smooth(method = "lm")
            }
            p
        })
    })
}

shinyApp(ui = ui, server = server)

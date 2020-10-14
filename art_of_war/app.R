library(wordcloud2)
library(shiny)
library(tm)
library(tmap)
library(NLP)
library(markdown)


note <- "The more often a specific words appears in your text, the bigger and bolder it appears in your word cloud. You can write a text in the text box/ Upload a document for generating a wordcloud. Thus, you can have a quick overview of what the Novel/ Document / Speech /Journal Article is all about."

create_wordcloud <- function(data, num_words = 100, background = "white") {
    
    
    if (is.character(data)) {
        corpus <- Corpus(VectorSource(data))
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        tdm <- as.matrix(TermDocumentMatrix(corpus))
        data <- sort(rowSums(tdm), decreasing = TRUE)
        data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    
    if (!is.numeric(num_words) || num_words < 3) {
        num_words <- 3
    }  
    
   
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
        return(NULL)
    }
    
    wordcloud2(data, backgroundColor = background)
}


ui <- navbarPage("Emon,s WordCloud Generator",
    theme = shinythemes::shinytheme("slate"),
    tabPanel("Input Your Text",
    titlePanel("Text Processing"),
    sidebarLayout(
        sidebarPanel(
            
            textAreaInput("text", "Enter text", rows = 7),
            actionButton("draw", "Create Cloud!"),
            numericInput("num", "Maximum number of words",
                         value = 100, min = 5),
            checkboxInput("check","Book:Art of War (Example)", FALSE),
            conditionalPanel(
              condition = "input.check == true",
              h5("The Art of War is an ancient Chinese military treatise dating from the Late Spring and Autumn Period. The work, which is attributed to the ancient Chinese military strategist Sun Tzu, is composed of 13 chapters. Each one is devoted to an aspect of warfare and how it applies to military strategy and tactics.")
            ),
            actionButton("about", "About Wordcloud")
            ),
        mainPanel(
                        wordcloud2Output("textcloud"),
                        wordcloud2Output("examplecloud")
                    )
    )
    ),

                    tabPanel("Input Your File",
                             h1("Text File Processing"),
                            sidebarLayout(
                              sidebarPanel(
                                fileInput("file", "Select a text file (.txt)"),
                                numericInput("num2", "Maximum number of words",
                                             value = 100, min = 5),
                   actionButton("about2", "About Wordcloud")
                              ),
                   mainPanel(wordcloud2Output("filecloud"))
                            
                   )
                   ),
    tabPanel("About",
              includeMarkdown("about.md")
                 )
  
)
server <- function(input, output) {
  x <- reactive({
    if(input$check){
    data <- readLines("artofwar.txt")
    return(data)}
    return(input$text)
  })
    observeEvent(input$about,{
       showModal(
           modalDialog(title = "Emon's Wordcloud", note)
       ) 
    })
    
    observeEvent(input$about2,{
        showModal(
            modalDialog(title = "Emon's Wordcloud", note)
        ) 
    })
    
    input_file <- reactive({
        if (is.null(input$file)) {
            return("")
        }
       
        readLines(input$file$datapath)
    })
    
    
    
    output$textcloud <- renderWordcloud2({input$draw
      isolate({
       
        create_wordcloud(x(), num_words = input$num
        )
        })
    })
   
    
    output$filecloud <- renderWordcloud2({
       
        create_wordcloud(input_file(), num_words = input$num2
                         )
    })
}

shinyApp(ui = ui, server = server)


library(shiny)

source("test.R")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Next word prediction"),
   
    sidebarLayout(
        sidebarPanel(width=4,
            textInput("txt", "Write sentence"),
            textOutput("clean")
        ),
      
        mainPanel(
            h4("Next word guesses : "),
            textOutput("sug")
            )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    txt.clean <- reactive({
        out <- ""
        if (!is.null(input$txt))
            out <- cleaner(input$txt)
        out
    })
    output$clean <- renderText(
        if (is.null(txt.clean()==TRUE))
            ""
        else
            txt.clean()
    )
    
    observe({
        if (is.null(input$txt) == TRUE)
            return()
        if (is.na(input$txt) == TRUE)
            return()
        if (input$txt == "")
            return()
        txt <- input$txt
        if (stri_extract_last(txt, regex=".") == " ")
            txt.pred <- input$txt %>% cleaner() %>% predict.next(nr.out=5) %>%
            paste(collapse="     ")
        else
            txt.pred <- ""
            
        output$sug <- renderText({
            txt.pred
        })
    })
})

# Run the application 
shinyApp(ui = ui, server = server)


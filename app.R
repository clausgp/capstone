
library(shiny)
library(shinyjs)

source("test.R")

# Steve Ladavich
textareaInput <- function(id, label="", value="", rows=20, cols=35, class="form-control"){
    tags$div(
        class="form-group shiny-input-container",
        tags$label('for'=id,label),
        tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}

ui <- shinyUI(
    
    # tags$script(HTML("Shiny.addCustomMessageHandler('cursorEnd', function(*message*) {
    #                  var input = $('#txt');
    #                  input[0].selectionStart = input[0].selectionEnd = input.val().length;
    #                  });
    #                  ")),
    
    navbarPage("Next word prediction",
        tabPanel("App",
            useShinyjs(),
            sidebarLayout(
                sidebarPanel(width=4,
                    h4("Type in text."),
                    "The model is used to both predict the current word - ",
                    "when cursor at a word",
                    "and the next word - when cursor at space or punctuation"
                                  
                    # textOutput("clean")
                ),
                mainPanel(width=8,
                    actionButton("clear", label="clear text"),
                    textareaInput("txt", "Write sentence", "",
                                 rows=6, cols=35),
                    div(id="nextbuts",
                        h4("Next word guesses : "),
                        actionButton("next1", label=textOutput("next1_label")),
                        actionButton("next2", label=textOutput("next2_label")),
                        actionButton("next3", label=textOutput("next3_label")),
                        actionButton("next4", label=textOutput("next4_label")),
                        actionButton("next5", label=textOutput("next5_label"))
                    ),
                    div(id="currbuts",
                        h4("Current word guess : "),
                        actionButton("curr1", label=textOutput("curr1_label")),
                        actionButton("curr2", label=textOutput("curr2_label")),
                        actionButton("curr3", label=textOutput("curr3_label")),
                        actionButton("curr4", label=textOutput("curr4_label")),
                        actionButton("curr5", label=textOutput("curr5_label"))
                    )
                )
            )
        ),
        tabPanel("About",
            h4("packages used :"),
            "shiny\nshinyjs\ntext2vec\nstringi\ndata.table\ndplyr"
        )
   )
)

server <- shinyServer(function(input, output, session) {
    values <- reactiveValues()
    values$pred <- c("", "", "", "", "")
    values$curr <- c("", "", "", "", "")
    
    output$next1_label <- renderText({
        values$pred[1]
    })
    output$next2_label <- renderText({
        values$pred[2]
    })
    output$next3_label <- renderText({
        values$pred[3]
    })
    output$next4_label <- renderText({
        values$pred[4]
    })
    output$next5_label <- renderText({
        values$pred[5]
    })
    output$curr1_label <- renderText({
        values$curr[1]
    })
    output$curr2_label <- renderText({
        values$curr[2]
    })
    output$curr3_label <- renderText({
        values$curr[3]
    })
    output$curr4_label <- renderText({
        values$curr[4]
    })
    output$curr5_label <- renderText({
        if (is.na(values$curr[5]))
            ""
        else
            values$curr[5]
    })
    
    observeEvent(input$clear, {
        updateTextInput(session, "txt", value = "")
    })
    
    txt.clean <- reactive({
        if (!is.null(input$txt))
            out <- cleaner(input$txt)
        out
    })
    output$clean <- renderText(
        if (is.null(input$txt) | is.na(input$txt))
            ""
        else
            cleaner(input$txt)
    )
    
    observe({
        if (is.null(input$txt) == TRUE)
            return()
        if (is.na(input$txt) == TRUE)
            return()
        #if (input$txt == "")
        #    return()
        txt <- input$txt
        if (txt=="" | stri_extract_last(txt, regex=".") == " "){
            hide("currbuts")
            txt.curr <- c("", "", "", "", "")
            txt.pred <- txt %>% cleaner() %>% predict.next(nr=5)
            values$pred <- txt.pred
            show("nextbuts")
        }    
        else {
            hide("nextbuts")
            txt.pred <- c("", "", "", "", "")
            txt.curr <- txt %>% cleaner() %>% predict.curr(nr=5)
            txt.len <- length(txt.curr)
            if (txt.len<5){
                blank <- rep("", 5-txt.len)
                txt.curr <- c(txt.curr, blank)
            }
            values$curr <- txt.curr
            show("currbuts")
        }
    })
    observeEvent(input$next1, {
        updateTextInput(session, "txt",
                        value = paste0(input$txt, values$pred[1], " "))
        # session$sendCustomMessage("cursorEnd", TRUE)
    })
    observeEvent(input$next2, {
        updateTextInput(session, "txt",
                        value = paste0(input$txt, values$pred[2], " "))
    })
    observeEvent(input$next3, {
        updateTextInput(session, "txt",
                        value = paste0(input$txt, values$pred[3], " "))
    })
    observeEvent(input$next4, {
        updateTextInput(session, "txt",
                        value = paste0(input$txt, values$pred[4], " "))
    })
    observeEvent(input$next5, {
        updateTextInput(session, "txt",
                        value = paste0(input$txt, values$pred[5], " "))
    })
    observeEvent(input$curr1, {
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b[\\w]{1,100}$)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[1], " "))
    })
    observeEvent(input$curr2, {
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b[\\w]{1,100}$)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[2], " "))
    })
    observeEvent(input$curr3, {
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b[\\w]{1,100}$)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[3], " "))
    })
    observeEvent(input$curr4, {
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b[\\w]{1,100}$)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[4], " "))
    })
    observeEvent(input$curr5, {
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b[\\w]{1,100}$)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[5], " "))
    })
})

# Run the application 
shinyApp(ui = ui, server = server)

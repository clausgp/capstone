
library(shiny)
library(shinyjs)
library(markdown)

source("predict.R")

# Steve Ladavich
textareaInput <- function(id, label="", value="", nrows=20, ncols=35, class="form-control"){
    tags$div(
        class="form-group shiny-input-container",
        tags$label('for'=id, label),
        tags$textarea(id=id, class=class, rows=nrows, cols=ncols, value))
}

ui <- shinyUI(
    
    # tags$script(HTML("Shiny.addCustomMessageHandler('cursorEnd', function(*message*) {
    #                  var input = $('#txt');
    #                  input[0].selectionStart = input[0].selectionEnd = input.val().length;
    #                  });
    #                  ")),
    
    navbarPage("Next word suggestion",
        tabPanel("App",
            useShinyjs(),
            sidebarLayout(
                sidebarPanel(width=5,
                    h4("Next word suggestion :"),
                    "When the last letter in the textbox is a space, ",
                    "then the next words will be suggested, and shown as the bottoms, ",
                    "with the left-most being the most often next word.",
                    h4("Current word suggestion :"),
                    "When the last letter in the textbos is a letter, ",
                    "the likely continuations will be shown as the bottoms, ",
                    "with the left-most being the most often continuation."
                ),
                mainPanel(width=7,
                    actionButton("clear", label="clear text"),
                    textareaInput("txt", "Write sentence", "",
                                 nrows=6, ncols=35),
                    div(id="space", rows=4,
                    div(id="nextbuts",
                        h4("Next word suggestions : "),
                        actionButton("next1", label=textOutput("next1_label")),
                        actionButton("next2", label=textOutput("next2_label")),
                        actionButton("next3", label=textOutput("next3_label")),
                        actionButton("next4", label=textOutput("next4_label")),
                        actionButton("next5", label=textOutput("next5_label"))
                    ),
                    div(id="currbuts",
                        h4("Current word suggestions : "),
                        actionButton("curr1", label=textOutput("curr1_label")),
                        actionButton("curr2", label=textOutput("curr2_label")),
                        actionButton("curr3", label=textOutput("curr3_label")),
                        actionButton("curr4", label=textOutput("curr4_label")),
                        actionButton("curr5", label=textOutput("curr5_label"))
                    )
                    )
                )
            )
        ),
        tabPanel("Model-prose",
            sidebarLayout(
                sidebarPanel(width=5,
                    h4("Model prose :"),
                    p("Here you can have the model generate the next award winning prose.",
                        "Remember to take a deep breath before starting to read..."),
                    fluidRow(
                        actionButton("gprose1", label="generate from first suggestion")
                        ),
                    fluidRow(
                        actionButton("gprose2", label="generate from second suggestion")
                    ),
                    fluidRow(
                        actionButton("gproser", label="generate from random top 3 suggestions")
                    ),
                    p("The 2 first will allways generate the same sentence. ",
                      "The last choice will allways generate a new sentence",
                      "And its also clear by these long 'sentences' that my model",
                      "could really have benefitted by including an end-of-sentence-word.",
                      "This may come in a later iteration.")
                ),
                mainPanel(width=7,
                    textareaInput("prose", "", "", nrows=20, ncols=80)
                )
            )
        ),
        tabPanel("Notes",
            includeMarkdown("notes.md")
        ),
        tabPanel("About the author",
                 includeMarkdown("about.md")
        )
   )
)

server <- shinyServer(function(input, output, session) {
    # app
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
        values$curr[5]
    })
    
    observeEvent(input$clear, {
        updateTextInput(session, "txt", value = "")
    })
    
    # output$clean <- renderText(
    #     if (is.null(input$txt) | is.na(input$txt))
    #         ""
    #     else
    #         clean(input$txt)
    # )
    
    observe({
        if (is.null(input$txt) == TRUE)
            return()
        if (is.na(input$txt) == TRUE)
            return()
        txt <- input$txt
        if (txt=="" | stri_extract_last(txt, regex=".") == " "){
            hide("currbuts")
            # txt.curr <- c("", "", "", "", "")
            txt.pred <- txt %>% clean() %>% predict.next(nr=5)
            values$pred <- txt.pred
            show("nextbuts")
        }    
        else {
            hide("nextbuts")
            # txt.pred <- c("", "", "", "", "")
            txt.curr <- txt %>% clean() %>% predict.curr(nr=5)
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
        if (values$curr[1]=="")
            return()
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b\\w)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[1], " "))
    })
    observeEvent(input$curr2, {
        if (values$curr[2]=="")
            return()
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b\\w)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[2], " "))
    })
    observeEvent(input$curr3, {
        if (values$curr[3]=="")
            return()
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b\\w)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[3], " "))
    })
    observeEvent(input$curr4, {
        if (values$curr[4]=="")
            return()
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b\\w)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[4], " "))
    })
    observeEvent(input$curr5, {
        if (values$curr[5]=="")
            return()
        curtxt <- stri_extract_first(input$txt, regex="^.*(?=\\b\\w)")
        updateTextInput(session, "txt",
                        value = paste0(curtxt, values$curr[5], " "))
    })
    
    # model-prose
    observeEvent(input$gprose1, {
        txt <- predict.next("", nr=1)
        while(length(txt)<162){
            txt <- c(txt, predict.next(txt, nr=1))
        }
        updateTextInput(session, "prose", value = paste(txt, collapse=" "))
    })
    observeEvent(input$gprose2, {
        txt <- predict.next("", nr=2)[2]
        while(length(txt)<160){
            txt <- c(txt, predict.next(txt, nr=2)[2])
        }
        updateTextInput(session, "prose", value = paste(txt, collapse=" "))
    })
    observeEvent(input$gproser, {
        # Tommy, http://stackoverflow.com/questions/8810338/same-random-numbers-every-time
        set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
        txt <- predict.next("", nr=3)[sample(1:3, 1, prob=c(3/6, 2/6, 1/6))]
        while(length(txt) < 160){
            txt <- c(txt, predict.next(txt, nr=3)
                     [sample(1:3, 1, prob=c(3/6, 2/6, 1/6))])
        }
        updateTextInput(session, "prose", value = paste(txt, collapse=" "))
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
# runApp(display.mode="showcase")
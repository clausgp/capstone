# word suggestion app
library(shiny)
library(shinyjs)
library(markdown)

# stupid-backoff model prediction
source("predict.R")

# txtbox function from Steve Ladavich
textareaInput <- function(id, label="", value="", nrows=20, ncols=35, class="form-control"){
    tags$div(
        class="form-group shiny-input-container",
        tags$label('for'=id, label),
        tags$textarea(id=id, class=class, rows=nrows, cols=ncols, value))
}

ui <- tagList(
    useShinyjs(),
    navbarPage("Next word suggestion",
        tabPanel("App",
            sidebarLayout(
                sidebarPanel(width=5,
                    h4("Swiftkey functionality :"),
                    p("In trying to mimick a mobile keyboard app, i decided not to ",
                      "include a predict button as suggested in the peer evaluation.",
                      "Instead the app will do this automaticly as described below.",
                      "The 'prediction' is also not limited to 1 but gives further ",
                      "suggestions like a normal keyboard app."),
                    h4("Next word suggestion :"),
                    p("When the last letter in the textbox is a space, ",
                      "then the next words will be suggested, and shown as the buttons, ",
                      "with the left-most being the most often next word."),
                    h4("Current word suggestion :"),
                    p("When the last letter in the textbos is a letter, ",
                      "the likely continuations will be shown as the buttons, ",
                      "with the left-most being the most often continuation."),
                    h4("Warning :"),
                    p("I deliberately havent included a profanity filter")
                ),
                mainPanel(width=7,
                    actionButton("clear", label="clear text"),
                    textareaInput("txt", "Write sentence", "",
                                 nrows=6, ncols=35),
                    div(id="space",
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
                            #uiOutput("curr1_but"),
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
                    p(actionButton("gprose1", label="generate from first suggestion")),
                    p(actionButton("gprose2", label="generate from second suggestion")),
                    p(actionButton("gproser", label="generate from top 3 suggestions")),
                    p("The 2 first will allways generate the same sentence. ",
                      "The last choice will allways randomly generate a new sentence.",
                      "Its clear by these long 'sentences' that my model",
                      "could really have benefitted by including an end-of-sentence-word.",
                      "This may come in a later iteration.")
                ),
                mainPanel(width=7,
                    textareaInput("prose", "", "", nrows=14, ncols=80)
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
    values <- reactiveValues(pred=c("", "", "", "", ""), curr=c("", "", "", "", ""))
    
    observeEvent(input$clear, {
        updateTextInput(session, "txt", value = "")
    })
    # update text on the buttons as next/current changes
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
    
    # observer for txtbox
    observeEvent(input$txt, {
        txt <- input$txt
        if (txt=="" | stri_extract_last(txt, regex=".") == " "){
            # at space, show next word suggestions
            hide("currbuts")
            txt.pred <- txt %>% clean() %>% predict.next(nr=5)
            values$pred <- txt.pred
            show("nextbuts")
        }    
        else {
            # at letter, show current word suggestions
            hide("nextbuts")
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
    
    # update txtbox after next word click
    observeEvent(input$next1, {
        updateTextInput(session, "txt",
                        value = paste0(input$txt, values$pred[1], " "))
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
    
    # update txtbox after current word click
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
        sent <- predict.next("", nr=1)
        while(length(sent) < 100){
            sent <- c(sent, predict.next(sent, nr=1))
        }
        updateTextInput(session, "prose", value = paste(sent, collapse=" "))
    })
    observeEvent(input$gprose2, {
        sent <- predict.next("", nr=2)[2]
        while(length(sent) < 100){
            sent <- c(sent, predict.next(sent, nr=2)[2])
        }
        updateTextInput(session, "prose", value = paste(sent, collapse=" "))
    })
    observeEvent(input$gproser, {
        # Tommy, http://stackoverflow.com/questions/8810338/same-random-numbers-every-time
        set.seed( as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31) )
        sent <- predict.next("", nr=3)[sample(1:3, 1, prob=c(3/6, 2/6, 1/6))]
        while(length(sent) < 100){
            sent <- c(sent, predict.next(sent, nr=3)
                     [sample(1:3, 1, prob=c(3/6, 2/6, 1/6))])
        }
        updateTextInput(session, "prose", value = paste(sent, collapse=" "))
    })
})

# Run the application 
shinyApp(ui = ui, server = server)
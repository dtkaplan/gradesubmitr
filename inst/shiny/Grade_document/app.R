# Grade the multiple-choice questions in a tutorial document

library(shiny)
library(gradesubmitr)
library(dplyr)
library(ggformula)

# Get a list of all the items in a given document
document_item_names <- function(Events, doc_choice) {
    unique(subset(Events, document == doc_choice)$item)
}

Events <- readRDS("Events.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tags$h3(HTML("Grade <code>{learnr}</code> document")),

    # Sidebar with a slider input for number of bins
    fillRow(
            fileInput("Raw_events", "Select event file",
                      multiple = FALSE, accept = NULL,
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"), "   ",
            uiOutput("document_holder"), "  ",
            dateRangeInput("dates", "Set dates")
            ),
    tags$h2(" ..."), # just for spacing



    tabsetPanel(
      tabPanel("One item",
               uiOutput("item_holder"),
               htmlOutput("prompt"),
               tableOutput("overall"),
               tableOutput("bystudent")),
      tabPanel("Score document",
               downloadButton("downloadScores", "Download document scores"),
               tableOutput("scores")),
      tabPanel("Submissions",
               # a plot of when submissions were made
               # Show a plot of the generated distribution)
               plotOutput("when_plot"))
    )


)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  get_raw_events <- reactive({
    if (!is.null(input$Raw_events)) {
      get_submissions(csv_name = input$Raw_events$datapath)
    } else { # For debugging
      Events
    }
  })

  get_document_events <- reactive({
    if (isTruthy(input$document_name))
      get_raw_events() %>% filter(document == !!input$document_name)
    else
      get_raw_events()[NULL,] # right names, but no cases
  })
  get_item_events <- reactive({
    req(input$document_name)
    req(input$item_name)
    Tmp1 <- get_document_events()
    cat("Document has", nrow(Tmp1), "rows\n")
    Tmp2 <- Tmp1 %>% filter(item == input$item_name)
    cat("Item has", nrow(Tmp2), "rows\n")
      # filter(what == "question", !is.na(is_correct)) %>%
    Tmp3 <- Tmp2 %>%  filter(input$dates[1] <= time,
             input$dates[2] >= time)
    cat("Items in date rage are", nrow(Tmp3), "rows\n")

    Tmp3
  })
  get_item_summary <- reactive({
    req(input$document_name) # So it will change when the document changes

    req(input$item_name) # Which item is selected within the document
    cat("Summarizing item", input$item_name, "\n")
    Item_events <- get_item_events()
    item_type <- Item_events$type[1] # should be only one type, anyways
    cat("Item type is", item_type, "\n" )
    if (is.na(item_type)) {
      NULL
    } else if (item_type == "multiple-choice") {
      summarize_MC_item(Item_events, input$item_name,
                          fiducial_date = 0.25)
    } else if (item_type == "essay") {
      summarize_essay_item(Item_events, input$item_name, fiducial_date = 0.25)
    } else if (item_type == "checked-code") {
      summarize_checked_code_item(Item_events, input$item_name, fiducial_date = 0.2)
    }
  })
  output$overall <- renderTable({
    get_item_summary()$overall
  })
  output$bystudent <- renderTable({
    get_item_summary()$by_student %>% select(-document, -item)
  })
  output$scores <- renderTable({
    document_scores()
  })

  document_scores <- reactive({
    score_document(get_document_events())
  })

  output$prompt <- renderText({
    HTML(get_item_summary()$prompt)
    })
  output$document_holder <- renderUI({
      selectInput("document_name", "Document", choices = unique(Events$document))
  })
  output$item_holder <- renderUI({
      req(input$document_name)
      cat("Input$document is", input$document_name, "\n")
      selectInput("item_name", "Question ID",
                  choices = document_item_names(Events, input$document_name))
  })
  output$when_plot <- renderPlot({
    From_doc <- get_document_events() %>%
      mutate(time = as.POSIXct(time))
    gf_density(~ time, data = From_doc)
  })
  # Change the dates when the document changes
  observe({
    req(input$document_name)
    cat("Updating date range to",
        capture.output(min(get_document_events()$time)),
        capture.output(max(get_document_events()$time)),
        "\n")

   updateDateRangeInput(session, "dates",
                        start = as_date(min(get_document_events()$time)) - 1,
                        end = as_date(max(get_document_events()$time)) + 1)
  })

  output$downloadScores <- downloadHandler(
    filename = function() {
      paste0("scores-", input$document_name,"-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(document_scores(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

# Grade the multiple-choice questions in a tutorial document

library(shiny)
library(gradesubmitr)
library(dplyr)
library(ggformula)
library(readr)
library(lubridate)

# Get a list of all the items in a given document
document_item_names <- function(Events, doc_choice) {
    unique(subset(Events, document == doc_choice)$item)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tags$h3(HTML("Grade <code>{learnr}</code> document")),

    # Sidebar with a slider input for number of bins
    fillRow(
            fileInput("Raw_events", "Select event file",
                      multiple = FALSE, accept = NULL,
                      buttonLabel = "Browse...",
                      placeholder = "Please select raw events file."), "   ",
            selectInput("document_name", "Document",
                        choices = head(LETTERS)), "  ",
            dateRangeInput("dates", "Set dates")
            ),
    tags$h2(" ..."), # just for spacing



    tabsetPanel(
      tabPanel("Score document",
               downloadButton("downloadScores", "Download document scores"),
               tableOutput("scores")),
      tabPanel("One item",
               selectInput("item_name", "Question ID",
                           choices = head(letters, 3)),
               uiOutput("item_holder"),
               htmlOutput("prompt"),
               tableOutput("overall"),
               tableOutput("bystudent")),
      tabPanel("Submissions",
               # a plot of when submissions were made
               # Show a plot of the generated distribution)
               plotOutput("when_plot")),
      tabPanel("Time line",
               selectizeInput("student", "Student ID", 1:3, multiple=TRUE),
               plotOutput("timeline"))
    )
)

# Define server logic
server <- function(input, output, session) {
  current_document <- reactiveVal()
  observe({
    current_document(req(input$document_name))
    })
  current_item <- reactiveVal()
  observe({
    current_item(req(input$item_name))
    })

  get_raw_events <- reactive({
    req(input$Raw_events)
    if (!is.null(input$Raw_events)) {
      Events <-
        get_submissions_google_forms(csv_name =
                          input$Raw_events$datapath)
    }
    tmp <- unique(Events$document)
    current_document(tmp[1]) # assign a value
    updateSelectInput(session,"document_name",
                  choices = unique(Events$document),
                  selected = isolate(current_document()))
    updateDateRangeInput(session, "dates",
                         start = as_date(min(Events$event_time)) - 1,
                         end = as_date(max(Events$event_time)) + 1)


    return(Events)
  })

  get_document_events <- reactive({
    Events <- get_raw_events()
    if (isTruthy(current_document())) {
      Ret <- Events %>%
        filter(document == current_document()) %>%
        mutate(login = tolower(login)) # Avoid capitalization inconsistencies
      # update the user interface
      items <- unique(Ret$item)
      current_item(items[1]) # assignment
      updateSelectInput(session, "item_name",
                        choices = items,
                        selected = isolate(current_item()))

      return(Ret)
    }
    NULL
  })

  get_item_events <- reactive({
    req(current_document())
    req(current_item())
    Tmp1 <- get_document_events()


    Tmp2 <- Tmp1 %>% filter(item == current_item())

      # filter(what == "question", !is.na(is_correct)) %>%
    if (isTruthy(input$dates)) {
      Tmp3 <- Tmp2 %>%  filter(input$dates[1] <= event_time,
                               input$dates[2] >= event_time)
    } else {
      Tmp3 <- Tmp2
    }

    Tmp3
  })
  get_item_summary <- reactive({
    req(current_document()) # So it will change when the document changes

    req(current_item()) # Which item is selected within the document

    Item_events <- get_item_events()
    item_type <- Item_events$type[1] # should be only one type, anyways

    if (is.na(item_type)) {
      NULL
    } else if (item_type == "multiple-choice") {
      summarize_MC_item(Item_events, current_item(),
                          fiducial_date = 0.25)
    } else if (item_type == "essay") {
      summarize_essay_item(Item_events, current_item(), fiducial_date = 0.25)
    } else if (item_type == "checked-code") {
      summarize_checked_code_item(Item_events,
                                  current_item(), fiducial_date = 0.2)
    }
  })
  output$overall <- renderTable({
    get_item_summary()$overall
  })
  output$bystudent <- renderTable({
    Foo <- get_item_summary()$by_student
    if (all(c("document", "item") %in% names(Foo))){
      Foo %>% select(-document, -item)
    } else {
      Foo
    }
  })
  output$scores <- renderTable({
    mutate_if(document_scores(), is.numeric,
              function(x) format(x,  nsmall =  3))
  })

  document_scores <- reactive({
    Tmp <- get_document_events()
    score_document(Tmp)
  })

  output$prompt <- renderText({
    HTML(get_item_summary()$prompt)
    })

  output$when_plot <- renderPlot({
    From_doc <- get_document_events() %>%
      mutate(event_time = as.POSIXct(event_time))
    if (isTruthy(input$dates)) {
      From_doc <- From_doc %>%
        filter(input$dates[1] <= event_time,
               input$dates[2] >= event_time)
    }
    gf_histogram(~ event_time, data = From_doc, bins = 200)
  })
  # Change the dates when the document changes
  observe({
    get_raw_events()
    req(current_document())
    Tmp <- get_document_events()

    updateDateRangeInput(session, "dates",
                         start = as_date(min(Tmp$event_time)) - 1,
                         end = as_date(max(Tmp$event_time)) + 1)

  })

  output$downloadScores <- downloadHandler(
    filename = function() {
      paste0("scores-", current_document(),"-", Sys.Date(), ".csv")
    },
    content = function(file) {
      For_output <- document_scores() %>%
        tidyr::separate(login, into = c("login", "passcode"),
                        sep = ":")
      write.csv(For_output, file, row.names = FALSE)
    }
  )

  #update the list of students for the timeline
  observe({
    Events <- get_document_events()
    students <- sort(unique(gsub(":.*$", "", Events$login)))
    updateSelectInput(session, "student", choices  = students,
                      selected = students[1])
  })

  output$timeline <- renderPlot({

    students <- paste0(input$student, collapse="|")
    Events <- get_document_events() %>%
      filter(grepl(students, login)) %>%
      mutate(login  = gsub(":.*$", "", login),
             event_time = as.POSIXct(event_time))
    if (isTruthy(input$dates)) {
      Events <- Events %>%
        filter(input$dates[1] <= event_time,
               input$dates[2] >= event_time)
    }
    gf_point(login  ~  event_time, data = Events,
             color = ~ (correct  | type == "essay") )  %>%
      gf_text(label = ~ item, angle=90) %>%
      gf_refine(scale_x_datetime(date_labels="%m/%d %R"))

  })
}

# Run the application
shinyApp(ui = ui, server = server)

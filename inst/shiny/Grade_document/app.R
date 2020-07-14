# Grade the multiple-choice questions in a tutorial document

library(shiny)
library(gradesubmitr)
library(dplyr)

# Get a list of all the items in a given document
document_item_names <- function(Events, doc_choice) {
    cat("Document is", doc_choice, "\n")
    cat("    which has", nrow(subset(Events, document == doc_choice)), "items\n")
    unique(subset(Events, document == doc_choice)$item)
}
cat("In directory", getwd(), "\n")
cat("Reading events ...")
Events <- readRDS("Events.rds")
cat("read", nrow(Events), "events\n")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Grade submitr document"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("document_holder"),
            uiOutput("item_holder"),
            dateRangeInput("dates", "Set dates"),
            plotOutput("when_plot") # a plot of when submissions were made
        ),

        # Show a plot of the generated distribution
        mainPanel(
           htmlOutput("prompt"),
           tableOutput("overall"),
           tableOutput("bystudent")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  get_document_events <- reactive({
    if (isTruthy(input$document_name))
      Events %>% filter(document == !!input$document_name)
    else
      Events[NULL,] # right names, but no cases
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
    } else if(item_type == "essay") {
        summarize_essay_item(Item_events, input$item_name, fiducial_date = 0.25)
    }
  })
  output$overall <- renderTable({
    get_item_summary()$overall

  })
  output$bystudent <- renderTable({
    get_item_summary()$by_student %>% select(-time)
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
}

# Run the application
shinyApp(ui = ui, server = server)

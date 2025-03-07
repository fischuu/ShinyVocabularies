library(shiny)
library(DT)

# Function to load deck
load_deck <- function(deck_name) {
  filepath <- paste0(deck_name, "_vocabularies.csv")
  if (file.exists(filepath)) {
    read.csv(filepath, stringsAsFactors = FALSE)
  } else {
    return(NULL)
  }
}

# Function to save deck
save_deck <- function(deck_name, df) {
  filepath <- paste0(deck_name, "_vocabularies.csv")
  write.csv(df, filepath, row.names = FALSE)
}

ui <- fluidPage(
  titlePanel("Finnish Vocabulary Trainer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("deck", "Choose Deck:", choices = NULL),
      verbatimTextOutput("deck_stats"),
      actionButton("next_card", "Next Card (→)", icon = icon("arrow-right")),
      actionButton("correct", "Correct (C)", icon = icon("check")),
      actionButton("wrong", "Wrong (W)", icon = icon("times")),
    ),
    
    mainPanel(
      h3("Flashcard"),
      verbatimTextOutput("flashcard"),
      h4("Example Sentence"),
      verbatimTextOutput("example_sentence"),
      DTOutput("deck_table")
    )
  )
)

server <- function(input, output, session) {
  deck_names <- reactive({
    files <- list.files(pattern = "_vocabularies.csv")
    sub("_vocabularies.csv", "", files)
  })
  
  observe({
    updateSelectInput(session, "deck", choices = deck_names())
  })
  
  deck_data <- reactiveVal(NULL)
  current_card <- reactiveVal(NULL)
  
  observeEvent(input$deck, {
    df <- load_deck(input$deck)
    if (!is.null(df)) {
      df$LastAsked <- as.Date(df$LastAsked)
      deck_data(df)
    }
  })
  
  observeEvent(input$next_card, {
    df <- deck_data()
    if (!is.null(df) && nrow(df) > 0) {
      card <- df[sample(nrow(df), 1), ]
      current_card(card)
    }
  })
  
  output$flashcard <- renderText({
    card <- current_card()
    if (is.null(card)) return("Choose a deck and press 'Next Card'.")
    paste0("Finnish: ", card$Finnish, "\n\n(Press to reveal translation)")
  })
  
  observeEvent(input$correct, {
    df <- deck_data()
    card <- current_card()
    if (!is.null(card)) {
      df[df$Finnish == card$Finnish, "TotalAsked"] <- card$TotalAsked + 1
      df[df$Finnish == card$Finnish, "Correct"] <- card$Correct + 1
      df[df$Finnish == card$Finnish, "Streak"] <- card$Streak + 1
      df[df$Finnish == card$Finnish, "LastAsked"] <- Sys.Date()
      deck_data(df)
      save_deck(input$deck, df)
      current_card(NULL)
    }
  })
  
  observeEvent(input$wrong, {
    df <- deck_data()
    card <- current_card()
    if (!is.null(card)) {
      df[df$Finnish == card$Finnish, "TotalAsked"] <- card$TotalAsked + 1
      df[df$Finnish == card$Finnish, "Streak"] <- 0
      df[df$Finnish == card$Finnish, "LastAsked"] <- Sys.Date()
      deck_data(df)
      save_deck(input$deck, df)
      current_card(NULL)
    }
  })
  
  output$deck_stats <- renderText({
    df <- deck_data()
    if (is.null(df)) return("No deck selected.")
    total <- nrow(df)
    asked <- sum(df$TotalAsked > 0)
    accuracy <- ifelse(sum(df$TotalAsked) > 0, round(100 * sum(df$Correct) / sum(df$TotalAsked), 1), 0)
    paste0("Words: ", total, "\nWords Reviewed: ", asked, "\nAccuracy: ", accuracy, "%")
  })
  
  output$deck_table <- renderDT({
    df <- deck_data()
    if (is.null(df)) return(NULL)
    datatable(df, options = list(pageLength = 10))
  })
}

shinyApp(ui, server)

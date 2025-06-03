# app.R

library(shiny)
library(randomForest)
library(shinythemes)

# Load model and data
model <- readRDS("models/food_model.rds")
reference_data <- read.csv("data/zomato_cleaned.csv", stringsAsFactors = FALSE)

# Get top values for dropdowns
get_top <- function(colname, top_n = 20) {
  top <- names(sort(table(reference_data[[colname]]), decreasing = TRUE))[1:top_n]
  sort(top)
}

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  
  # Header with logo
  fluidRow(
    column(12, align = "center",
           tags$img(src = "https://c8.alamy.com/comp/PCYG1J/pizzeria-fast-food-logo-or-label-happy-chef-holding-pizza-and-scapula-in-hands-vector-illustration-PCYG1J.jpg", height = "100px"),
           tags$h2("DineLine – Stay Ahead of Hunger 🍽️", style = "color: #2C3E50; margin-top: 10px;"))
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      h4("🔧 Restaurant Settings"),
      selectInput("location", "📍 Location", choices = c(get_top("location"), "Other")),
      selectInput("rest_type", "🏢 Restaurant Type", choices = c(get_top("rest_type"), "Other")),
      selectInput("cuisines", "🍜 Cuisine Type", choices = c(get_top("cuisines"), "Other")),
      
      numericInput("rate", "⭐ Avg Rating", value = 4.0, min = 1, max = 5, step = 0.1),
      numericInput("votes", "🗳️ Number of Votes", value = 100, min = 0),
      numericInput("cost", "💸 Cost for Two (₹)", value = 500, min = 0),
      
      selectInput("type", "🚚 Order Type", choices = unique(reference_data$listed_in.type.)),
      selectInput("city", "🏙️ City", choices = unique(reference_data$listed_in.city.)),
      
      actionButton("predict", "🔍 Recommend Dishes", class = "btn btn-primary", width = "100%"),
      br(), br()
    ),
    
    mainPanel(
      h3("🍛 Top 3 Recommended Dishes"),
      tableOutput("prediction_table"),
      hr(),
      tags$p("Predictions are powered by machine learning on over 50,000 food records from the Zomato dataset.",
             style = "font-size: 14px; color: #888;")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$predict, {
    tryCatch({
      new_data <- data.frame(
        location = factor(input$location, levels = model$forest$xlevels$location),
        rest_type = factor(input$rest_type, levels = model$forest$xlevels$rest_type),
        cuisines = factor(input$cuisines, levels = model$forest$xlevels$cuisines),
        rate = input$rate,
        votes = input$votes,
        cost = input$cost,
        listed_in.type. = factor(input$type, levels = model$forest$xlevels$`listed_in.type.`),
        listed_in.city. = factor(input$city, levels = model$forest$xlevels$`listed_in.city.`)
      )
      
      probs <- predict(model, newdata = new_data, type = "prob")
      top3 <- sort(probs[1, ], decreasing = TRUE)[1:3]
      
      output$prediction_table <- renderTable({
        data.frame(
          Dish = names(top3),
          Probability = round(as.numeric(top3), 3)
        )
      })
    }, error = function(e) {
      output$prediction_table <- renderTable({
        data.frame(Error = e$message)
      })
    })
  })
}

shinyApp(ui = ui, server = server)

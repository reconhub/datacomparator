library(shiny)
library(compareDF)


# Define UI ----
ui <- fluidPage(
  titlePanel("Data Comparator"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("old_file", h4("Old file")),
      fileInput("new_file", h4("New file")),
      submitButton("Submit")
    ),
    mainPanel(
      "Results",
      htmlOutput("compare_results")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  htmlOutput({
    output$compare_results <- reactive ({
      compare_df(
      input$old_file, input$new_file, group_col = "id")
    })
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
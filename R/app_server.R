#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_compare_data_server, "compare_data_ui_1")
  
  observeEvent(input$browser,{
    browser()
  })
}

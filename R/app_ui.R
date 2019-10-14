#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage(
      id = "tabs",
      position = "fixed-top",
      title = "RECON datacomparator",
      #theme = shinytheme("sandstone"),
      collapsible = TRUE,
      
      tabPanel("", icon = icon("home"), mod_data_import_ui("data_import_ui_1"),
               verbatimTextOutput("dt"))
      
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'datacomparator')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    shinyjs::useShinyjs(),
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css")
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

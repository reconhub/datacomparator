#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    tags$header(
      class = "navbar navbar-default navbar-static-top",
      tags$div(
        class = "container-fluid",
        tags$div(
          class = "navbar-header",
          tags$div(class = "navbar-brand", HTML("<b>RECON</b> <i>datacomparator</i>"))
        ),
        # Place to add links on right hand side of navbar if required
        tags$ul(
          class = "nav navbar-nav navbar-right",
          tags$li(),
          tags$li()
        )
      )
    ),
    
    fluidPage(
      theme = shinythemes::shinytheme("simplex"),
      title = "RECON datacomparator",
      mod_compare_data_ui("compare_data_ui_1")
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

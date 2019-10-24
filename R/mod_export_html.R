# Module UI
  
#' @title   mod_export_html_ui and mod_export_html_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_export_html
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_export_html_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::downloadButton(ns("download"), label = "Export Table", class = "btn-info", style = "color: white;")
  )
}
    
# Module Server
    
#' @rdname mod_export_html
#' @export
#' @keywords internal
    
mod_export_html_server <- function(input, output, session, html, filename){
  ns <- session$ns
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(filename, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      writeLines(html(), con = file, useBytes = TRUE)
    }
  )
}
    
## To be copied in the UI
# mod_export_html_ui("export_html_ui_1")
    
## To be copied in the server
# callModule(mod_export_html_server, "export_html_ui_1")
 

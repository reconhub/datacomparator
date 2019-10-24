# Module UI
  
#' @title   mod_export_xlsx_ui and mod_export_xlsx_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_export_xlsx
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_export_xlsx_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::downloadButton(ns("download"), label = "Export Table", class = "btn-info", style = "color: white;")
  )
}
    
# Module Server
    
#' @rdname mod_export_xlsx
#' @export
#' @keywords internal
    
mod_export_xlsx_server <- function(input, output, session, old_dups, new_dups, filename){
  ns <- session$ns
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(filename, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(list("Old Data" = old_dups(), "New Data" = new_dups()), path = file)
    }
  )
}
    
## To be copied in the UI
# mod_export_xlsx_ui("export_xlsx_ui_1")
    
## To be copied in the server
# callModule(mod_export_xlsx_server, "export_xlsx_ui_1")
 

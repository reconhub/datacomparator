# Module UI
  
#' @title   mod_data_import_ui and mod_data_import_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_import
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(
      width = 4, 
      wellPanel(
        fileInput(ns("new_data"), "New Data File",
                  accept = c(".xlsx", ".xls", ".csv"),
                  placeholder = "Excel or CSV file"),
        
        fileInput(ns("old_data"), "Old Data File",
                  accept = c(".xlsx", ".xls", ".csv"),
                  placeholder = "Excel or CSV file"),
        
        shinyjs::hidden(
          div(id = ns("hidden_inputs"),
            selectizeInput(ns("date_var"), "Select Date Column", choices = ""),
            numericInput(ns("n_days"), "Number of days to compare", value = 42, min = 1, step = 1)
          )
        ),
        
        tags$hr(),
        
        shinyjs::disabled(
          actionButton(ns("go"), "Compare Data", class = "btn-primary", icon = icon("not-equal"))
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_import
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
    
mod_data_import_server <- function(input, output, session){
  ns <- session$ns
  
  imported_data <- reactiveValues(new_data = NULL, new_data_date = NULL,
                                  old_data = NULL, old_data_date = NULL,
                                  launch_modules = 0)
  
  observeEvent(input$new_data, {
    inFile <- input$new_data
    
    if (is.null(inFile)) return(NULL)
    
    fe <- tools::file_ext(inFile$name)
    
    if (fe == "csv") {
      df <- readr::read_csv(inFile$datapath, guess_max = 1e5)
    } else if (grepl("xls", fe)) {
      df <- readxl::read_excel(inFile$datapath, guess_max = 1e5)
    }
    
    imported_data$new_data <- df
    imported_data$new_data_date <- linelist::guess_dates(inFile$name)
    
    updateSelectizeInput(session, "date_var", choices = c("Select date column to clean" = "", names(df)))
  })
  
  observeEvent(input$old_data, {
    inFile <- input$old_data
    
    if (is.null(inFile)) return(NULL)
    
    fe <- tools::file_ext(inFile$name)
    
    if (fe == "csv") {
      df <- readr::read_csv(inFile$datapath, guess_max = 1e5)
    } else if (grepl("xls", fe)) {
      df <- readxl::read_excel(inFile$datapath, guess_max = 1e5)
    }
    
    imported_data$old_data <- df
    imported_data$old_data_date <- linelist::guess_dates(inFile$name)
  })
  
  observe({
    shinyjs::toggle(
      id = "hidden_inputs", anim = TRUE,
      condition = (!is.null(imported_data$new_data) & !is.null(imported_data$old_data))
    )
  })
  
  observe({
    shinyjs::toggleState(id = "go", 
                         condition = (!is.null(imported_data$new_data) & 
                                        !is.null(imported_data$old_data) &
                                        input$date_var != "")
                         )
  })
  
  # when button is clicked, clean and filter data based on inputs
  observeEvent(input$go, {
    date_var <- rlang::sym(input$date_var)
    start_at <- imported_data$new_data_date - input$n_days
    
    imported_data$new_data <- imported_data$new_data %>%
      dplyr::mutate(date_report = linelist::guess_dates(!!date_var)) %>% 
      dplyr::filter(date_report >= start_at)
    
    imported_data$old_data <- imported_data$old_data %>%
      dplyr::mutate(date_report = linelist::guess_dates(!!date_var)) %>% 
      dplyr::filter(date_report >= start_at)
    
    imported_data$launch_modules <- input$go
  })
  
  # return data and datestamps to server
  reactive({
    reactiveValuesToList(imported_data)
  })
  
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")
    
## To be copied in the server
# callModule(mod_data_import_server, "data_import_ui_1")
 

# Module UI
  
#' @title   mod_compare_data_ui and mod_compare_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_compare_data
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_compare_data_ui <- function(id){
  ns <- NS(id)
  tagList(
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          tags$h3("Inputs"),
          tags$hr(),
          
          fileInput(ns("new_data"), "New Data File",
                    accept = c(".xlsx", ".xls", ".csv"),
                    placeholder = "Excel or CSV file"),
          
          fileInput(ns("old_data"), "Old Data File",
                    accept = c(".xlsx", ".xls", ".csv"),
                    placeholder = "Excel or CSV file"),
          
          shinyjs::hidden(
            div(id = ns("hidden_inputs"),
                selectizeInput(ns("id_var"), "Select ID Column", choices = ""),
                selectizeInput(ns("date_var"), "Select Date Column", choices = ""),
                selectizeInput(ns("keep_vars"), "Select other columns to compare", choices = "", multiple = TRUE),
                numericInput(ns("n_days"), "Max days to compare", value = 42, min = 1, step = 1)
            )
          ),
          
          tags$hr(),
          
          shinyjs::hidden(
            actionButton(ns("go"), "Compare Data", class = "btn-primary", icon = icon("search"))
          )
        ),
        
        mainPanel(
          tabsetPanel(
            id = ns("tabs"), 
            tabPanel(
              "Home", icon = icon("home"), 
              wellPanel(includeMarkdown(system.file("app", "www", "README.md", package = "datacomparator")))
            )
          )
        )
      )
  )
}
    
# Module Server
    
#' @rdname mod_compare_data
#' @importFrom magrittr %>%
#' @export
#' @keywords internal
    
mod_compare_data_server <- function(input, output, session){
  ns <- session$ns
  
  imported_data <- reactiveValues(new_data_raw = NULL, new_data = NULL, new_data_date = NULL,
                                  old_data_raw = NULL, old_data = NULL, old_data_date = NULL)
  
  observeEvent(input$new_data, {
    inFile <- input$new_data
    
    if (is.null(inFile)) return(NULL)
    
    fe <- tools::file_ext(inFile$name)
    
    if (fe == "csv") {
      df <- readr::read_csv(inFile$datapath, guess_max = 1e5)
    } else if (grepl("xls", fe)) {
      df <- readxl::read_excel(inFile$datapath, guess_max = 1e5)
    }
    
    imported_data$new_data_raw <- df
    imported_data$new_data_date <- linelist::guess_dates(inFile$name)
    
    updateSelectizeInput(session, "date_var", choices = c("Select date column to clean" = "", names(df)))
    updateSelectizeInput(session, "id_var", choices = c("Select case ID column" = "", names(df)))
  })
  
  observe({
    choices <- names(imported_data$new_data_raw)
    choices <- choices[!choices %in% c(input$id_var, input$date_var)]
    updateSelectizeInput(session, "keep_vars", choices = choices)
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
    
    imported_data$old_data_raw <- df
    imported_data$old_data_date <- linelist::guess_dates(inFile$name)
  })
  
  observe({
    shinyjs::toggle(
      id = "hidden_inputs", anim = TRUE,
      condition = (!is.null(imported_data$new_data_raw) & !is.null(imported_data$old_data_raw))
    )
  })
  
  observe({
    shinyjs::toggle(
      id = "go", 
      condition = (!is.null(imported_data$new_data_raw) & 
                     !is.null(imported_data$old_data_raw) &
                     input$date_var != "" &
                     input$id_var != "")
    )
  })
  
  # when button is clicked, clean and filter data based on inputs
  observeEvent(input$go, {
    date_var <- rlang::sym(input$date_var)
    id_var <- rlang::sym(input$id_var)
    start_at <- imported_data$new_data_date - input$n_days
    
    imported_data$new_data <- imported_data$new_data_raw %>%
      dplyr::mutate(id = !!id_var) %>% 
      dplyr::mutate(date = linelist::guess_dates(!!date_var)) %>% 
      dplyr::filter(date >= start_at) %>% 
      dplyr::select("id", "date", input$keep_vars)
    
    imported_data$old_data <- imported_data$old_data_raw %>%
      dplyr::mutate(id = !!id_var) %>% 
      dplyr::mutate(date = linelist::guess_dates(!!date_var)) %>% 
      dplyr::filter(date >= start_at) %>% 
      dplyr::select("id", "date", input$keep_vars)
  })
  
  observeEvent(input$go, {
    if (input$go == 1) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "linelist::compare_data", #icon = icon("home"),
          shiny::verbatimTextOutput(ns("linelist"))
        ),
        select = TRUE
      )
      
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "compareDF::compare_df", #icon = icon("home"),
          htmlOutput(ns("compareDF"))
        )
      )
      
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "Old data duplicates", #icon = icon("home"),
          DT::DTOutput(ns("od_dups"))
        )
      )
      
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "New data duplicates", #icon = icon("home"),
          DT::DTOutput(ns("nd_dups"))
        )
      )
      
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "Compare duplicates", #icon = icon("home"),
          htmlOutput(ns("compare_dups"))
        )
      )
    } else if (input$go > 1) {
      updateTabsetPanel(session, "tabs", selected = "linelist::compare_data")
    }
  })
  
  output$linelist <- renderPrint({
    validate(need(input$go > 0, "Please enter inputs and click the compare button for results"))
    linelist::compare_data(imported_data$new_data, imported_data$old_data, use_values = FALSE)
  })
  
  output$compareDF <- renderUI({
    validate(need(input$go > 0, "Please enter inputs and click the compare button for results"))
    
    comparison <- compareDF::compare_df(
      imported_data$new_data, 
      imported_data$old_data,
      group_col = "id",
      limit_html = 1000,
      color_scheme = comp_colors,
      stop_on_error = FALSE)
    
    shiny::HTML(comparison$html_output)
  })
  
  duplicates_old <- eventReactive(input$go, {
    old_data <- imported_data$old_data
    
    to_keep <- old_data %>% dplyr::filter(duplicated(id)) %>% dplyr::pull(id)
    
    old_data %>% dplyr::filter(id %in% to_keep)
  })
  
  duplicates_new <- eventReactive(input$go, {
    new_data <- imported_data$new_data
    
    to_keep <- new_data %>% dplyr::filter(duplicated(id)) %>% dplyr::pull(id)
    
    new_data %>% dplyr::filter(id %in% to_keep)
  })
  
  output$od_dups <- DT::renderDT({
    validate(
      need(row(duplicates_old()) > 0, "No duplicates found in old data set.")
    )

    DT::datatable(duplicates_old(), options = list(scrollX = TRUE))
  })

  output$nd_dups <- DT::renderDT({
    validate(
      need(row(duplicates_new()) > 0, "No duplicates found in new data set.")
    )

    DT::datatable(duplicates_new(), options = list(scrollX = TRUE))
  })
  
  output$compare_dups <- renderUI({
    comparison_duplicates <- compareDF::compare_df(
      duplicates_new(),
      duplicates_old(),
      group_col = "id",
      limit_html = 1000,
      color_scheme = comp_colors,
      keep_unchanged_rows = TRUE,
      stop_on_error = FALSE)
    
    shiny::HTML(comparison_duplicates$html_output)
  })
  
}
    
## To be copied in the UI
# mod_compare_data_ui("compare_data_ui_1")
    
## To be copied in the server
# callModule(mod_compare_data_server, "compare_data_ui_1")
 

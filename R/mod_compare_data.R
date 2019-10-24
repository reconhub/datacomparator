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
      position = "right",
      
      sidebarPanel(
        width = 3, style = "margin-top: 40px;",
        
        helpText("Inputs"),
        tags$hr(),
        
        fileInput(ns("new_data"), "New Data File",
                  accept = c(".xlsx", ".xls", ".csv"),
                  placeholder = "Excel or CSV file"),
        
        fileInput(ns("old_data"), "Old Data File",
                  accept = c(".xlsx", ".xls", ".csv"),
                  placeholder = "Excel or CSV file"),
        
        shinyjs::hidden(
          div(
            id = ns("hidden_inputs"),
            selectizeInput(ns("id_var"), "Select ID Column", choices = "", multiple = FALSE),
            shinyWidgets::pickerInput(
              ns("keep_vars"), 
              "Select other columns to compare", 
              choices = "", 
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 3",
                `count-selected-text` = "{0} selected",
                style = "btn-light",
                showTick = TRUE
              ),
              multiple = TRUE
            ),
            checkboxInput(ns("show_date"), "If data contains dates, limit number of days to compare?", value = FALSE),
            selectizeInput(ns("date_var"), "Select Date Column", choices = ""),
            numericInput(ns("n_days"), "Max days to compare", value = 42, min = 1, step = 1)
          )
        ),
        
        tags$hr(),
        
        shinyjs::hidden(
          actionButton(ns("go"), "Compare Data", class = "btn-primary", icon = icon("search"))
        )
      ),
      
      mainPanel(
        width = 9,
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
  
  # ==============================================================================
  # Reactive Values
  # ==============================================================================
  
  # setup empty reactive vals we will store data in after it is imported
  
  imported_data <- reactiveValues(new_data_raw = NULL, new_data = NULL, new_data_date = NULL,
                                  old_data_raw = NULL, old_data = NULL, old_data_date = NULL)
  
  # ==============================================================================
  # Data Import + populate inputs
  # ==============================================================================
  
  # when new data file is uploaded read it into R and save to imported_data$new_data_raw reactive value
  # Parse the date in the filename and save to imported_data$new_data_date
  # also update the id and date inputs with the colnames of the data file
  
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
    
    updateSelectizeInput(session, "id_var", choices = c("Column to identify duplicates" = "", names(df)))
    updateSelectizeInput(session, "date_var", choices = c("Select date column to clean" = "", names(df)))
  })
  
  # updated choices of input$keep_vars with colnames of new data file minus 
  # user selected id and date cols
  
  observe({
    choices <- names(imported_data$new_data_raw)
    choices <- choices[!choices %in% c(input$id_var)]
    shinyWidgets::updatePickerInput(session, "keep_vars", choices = choices)
  })
  
  # when old data file is uploaded read it into R and save to imported_data$old_data_raw reactive value
  # Parse the date in the filename and save to imported_data$old_data_date
  
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
  
  # ==============================================================================
  # Observers to show inputs
  # ==============================================================================
  
  # when both datasets have successfully been uploaded, show hidden inputs
  
  observe({
    shinyjs::toggle(
      id = "hidden_inputs", anim = TRUE,
      condition = (!is.null(imported_data$new_data_raw) & !is.null(imported_data$old_data_raw))
    )
  })
  
  observeEvent(input$show_date, {
    if(input$show_date) {
      shinyjs::show(id = "date_var", anim = TRUE)
      shinyjs::show(id = "n_days", anim = TRUE)
    } else {
      shinyjs::hide(id = "date_var", anim = TRUE)
      shinyjs::hide(id = "n_days", anim = TRUE)
    }
  })
  
  # when data and inputs have been filled, show the compare data button
  
  observe({
    shinyjs::toggle(
      id = "go", 
      condition = (!is.null(imported_data$new_data_raw) & 
                     !is.null(imported_data$old_data_raw) &
                     length(input$id_var) &
                     input$id_var != "" 
      )
    )
  })
  
  # ==============================================================================
  # Prepare Data
  # ==============================================================================
  
  # when compare button is clicked, clean and filter data based on inputs
  
  observeEvent(input$go, {
    id_var <- rlang::sym(input$id_var)
    
    df_new <- imported_data$new_data_raw %>% dplyr::mutate(id = !!id_var)
    
    df_old <- imported_data$old_data_raw %>% dplyr::mutate(id = !!id_var) 
    
    if (length(input$date_var) & input$date_var != "") {
      date_var <- rlang::sym(input$date_var)
      df_new <- df_new %>% dplyr::mutate(date = linelist::guess_dates(!!date_var))
      df_old <- df_old %>% dplyr::mutate(date = linelist::guess_dates(!!date_var))
      if (length(input$n_days)) {
        start_at <- imported_data$new_data_date - input$n_days
        df_new <- df_new %>% dplyr::filter(date >= start_at) 
        df_old <- df_old %>% dplyr::filter(date >= start_at) 
      }
    }
    
    imported_data$new_data <- df_new %>% dplyr::select("id", input$keep_vars)
    imported_data$old_data <- df_old %>% dplyr::select("id", input$keep_vars)
  })
  
  # ==============================================================================
  # Generate UI elements
  # ==============================================================================
  
  # when compare data button is clicked for the first time, append new UI tabs to show the compared data outputs
  # If it has already been clicked and the user is re-running with new data/inputs, switch selected tab
  # to the linelist::compare_data tab
  
  observeEvent(input$go, {
    if (input$go == 1) {
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "Summary", #icon = icon("home"),
          shiny::verbatimTextOutput(ns("linelist"))
        ),
        select = TRUE
      )
      
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "Compare DF", #icon = icon("home"),
          column(width = 3, 
                 includeMarkdown(system.file("app", "www", "colour_guide.md", package = "datacomparator")),
                 mod_export_html_ui(ns("export_compare_df"))),
          column(width = 9, style = 'overflow-x: scroll', 
                 htmlOutput(ns("compareDF")) %>% shinycssloaders::withSpinner())
        )
      )
      
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "Old data duplicates", #icon = icon("home"),
          #mod_export_xlsx_ui(ns("export_xlsx_ui_1")),
          tags$br(),
          DT::DTOutput(ns("od_dups"))
        )
      )
      
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "New data duplicates", #icon = icon("home"),
          tags$br(),
          DT::DTOutput(ns("nd_dups"))
        )
      )
      
      appendTab(
        inputId = "tabs",
        tabPanel(
          title = "Compare duplicates", #icon = icon("home"),
          column(width = 3, 
                 includeMarkdown(system.file("app", "www", "colour_guide.md", package = "datacomparator")),
                 mod_export_html_ui(ns("export_compare_duplicates"))),
          column(width = 9, style = 'overflow-x: scroll', 
                 htmlOutput(ns("compare_dups")) %>% shinycssloaders::withSpinner())
        )
      )
    } else if (input$go > 1) {
      updateTabsetPanel(session, "tabs", selected = "Summary")
    }
  })
  
  # ==============================================================================
  # Outputs and downloads
  # ==============================================================================
  
  output$linelist <- renderPrint({
    #validate(need(input$go > 0, "Please enter inputs and click the compare button for results"))
    linelist::compare_data(ref = imported_data$old_data, x = imported_data$new_data, use_values = FALSE)
  })
  
  comparison_df <- reactive({
    comparison <- compareDF::compare_df(
      imported_data$new_data, 
      imported_data$old_data,
      group_col = "id",
      limit_html = 1000,
      color_scheme = comp_colors,
      stop_on_error = FALSE)
    
    comparison$html_output
  })
  
  output$compareDF <- renderUI({
    shiny::HTML(comparison_df())
  })
  
  callModule(mod_export_html_server, "export_compare_df", 
             html = comparison_df, filename = "comparison_table")
  
  duplicates_old <- eventReactive(input$go, {
    old_data <- imported_data$old_data
    to_keep <- old_data %>% dplyr::filter(duplicated(id)) %>% dplyr::pull(id)
    old_data %>% dplyr::filter(id %in% to_keep)
  })
  
  output$od_dups <- DT::renderDT({
    show_table(duplicates_old())
  })
  
  duplicates_new <- eventReactive(input$go, {
    new_data <- imported_data$new_data
    to_keep <- new_data %>% dplyr::filter(duplicated(id)) %>% dplyr::pull(id)
    new_data %>% dplyr::filter(id %in% to_keep)
  })
  
  output$nd_dups <- DT::renderDT({
    show_table(duplicates_new())
  })
  
  # callModule(mod_export_xlsx_server, "export_xlsx_ui_1", 
  #            duplicates_old, duplicates_new,
  #            filename = "duplicates")
  
  comparison_dups <- reactive({
    comparison_duplicates <- compareDF::compare_df(
      duplicates_new(),
      duplicates_old(),
      group_col = "id",
      limit_html = 1000,
      color_scheme = comp_colors,
      keep_unchanged_rows = TRUE,
      stop_on_error = FALSE)
    
    comparison_duplicates$html_output
  })
  
  output$compare_dups <- renderUI({
    if(nrow(duplicates_new()) < 1 & nrow(duplicates_old()) < 1) return(tags$h3("No duplicates to compare!"))
    
    shiny::HTML(comparison_dups())
  })
  
  callModule(mod_export_html_server, "export_compare_duplicates", 
             html = comparison_dups, filename = "comparison_duplicates_table")
  
}

## To be copied in the UI
# mod_compare_data_ui("compare_data_ui_1")

## To be copied in the server
# callModule(mod_compare_data_server, "compare_data_ui_1")


#' Render DT datable with export button and scrollX enabled
#'
#' @param df 
#'
#' @return a DT datatable
#' @export
show_table <- function(df) {
  DT::datatable(df, 
                extensions = "Buttons",
                options = list(
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = 
                    list(list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Export'
                    )))
  )
}
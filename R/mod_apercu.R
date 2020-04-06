#' apercu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr %>% 
#' @importFrom utils head
mod_apercu_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(tableOutput(ns("apercu")))
  )
}
    
#' apercu Server Function
#'
#' @noRd 
mod_apercu_server <- function(input, output, session,r){
  ns <- session$ns
  output$apercu <- renderTable({
    req(r$covid)
    r$covid %>% 
      head(25)
  })
}
    
## To be copied in the UI
# mod_apercu_ui("apercu_ui_1")
    
## To be copied in the server
# callModule(mod_apercu_server, "apercu_ui_1")
 

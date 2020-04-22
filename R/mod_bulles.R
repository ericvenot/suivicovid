#' bulles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom RColorBrewer brewer.pal
#' @importFrom bubbles bubblesOutput renderBubbles
mod_bulles_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("ERRRRRRRRRICCCCC"),
    bubblesOutput(ns("bulles")),
    h2("VANUATU")
  )
}
    
#' bulles Server Function
#'
#' @noRd 
mod_bulles_server <- function(input, output, session,r){
  ns <- session$ns
  
  output$bulles <- renderBubbles({
    req(r$deces)
    top1000 <- r$deces %>% 
      filter(total_deces>1000)
    n<-nrow(top1000)
    palette <- colorRampPalette(brewer.pal(12,"Paired2"))(n)
    bubbles(top1000$total_deces, top1000$pays, key = top1000$pays,color = palette )
  })
}
    
## To be copied in the UI
# mod_bulles_ui("bulles_ui_1")
    
## To be copied in the server
# callModule(mod_bulles_server, "bulles_ui_1")
 

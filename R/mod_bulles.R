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
#' @importFrom bubbles bubbles bubblesOutput renderBubbles

mod_bulles_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Pr\u00E9sentation des pays avec plus de 1000 d\u00E9c\u00E8s"),
    bubblesOutput(ns("bulles")),
    h2("(Rem: package bubble)")
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
    palette <- colorRampPalette(brewer.pal(12,"Paired"))(n)
    bubbles(top1000$total_deces, top1000$pays, key = top1000$pays,color = palette )
  })
}
    
## To be copied in the UI
# mod_bulles_ui("bulles_ui_1")
    
## To be copied in the server
# callModule(mod_bulles_server, "bulles_ui_1")
 
